# Amiga JuggleR. OpenGL con R
# www.overfitting.net
# https://www.overfitting.net/2018/04/amiga-juggler-opengl-con-r.html

library(rgl)


# FUNCIONES 3D

# Esfera mejorada
spheres3d.plus=function(x=0, y=0, z=0, radius=1, n=101, ...){
  f=function(s, t) cbind(radius*cos(t)*cos(s) + x,
                         radius*       sin(s) + y,
                         radius*sin(t)*cos(s) + z)
  persp3d(f, slim=c(-pi/2, pi/2), tlim=c(0, 2*pi), n=n, add=T, ...)
}

# Elipse para sombras
ellipse.xy=function(x=0, y=0, z=0, radius=1, scale=1, theta=0, n=51, ...){
  t=seq(0, 2*pi, len=n)
  xp=radius*scale*cos(t)
  yp=radius*sin(t)
  if (theta) {  # Rotación por theta
    xtmp=xp*cos(theta)-yp*sin(theta)
    yp  =xp*sin(theta)+yp*cos(theta)
    xp=xtmp
  }
  polygon3d(xp+x, yp+y, seq(z, z, len=n), ...)
}

iif = function(condicion, val1, val2) {
  if (condicion) return(val1)
  return(val2)
}


# AMIGA JUGGLER

# Paráms. de definición
N=30  # Núm. de frames
NRES=51  # Facetado esferas
WIDTH=1280  # 800  # 512  # Resolución de salida
HEIGHT=960  # 600  # 384
SCALE=150  # Factor de escala

# Leer universo
jug=read.table("amigajuggler.csv", header=T, sep=";", dec=",")  # CSV de Excel


# Paráms. geométricos estáticos

# Caderas, tronco y brazos ("biela")
Rb=10  # Radio "biela"
Lb=jug$z[jug$desc=='cuello']-jug$z[jug$desc=='pelvis']  # Long. "biela"
arm=((jug$y[jug$desc=='codo izq.']-jug$y[jug$desc=='hombro izq.'])^2+
     (jug$z[jug$desc=='hombro izq.']-jug$z[jug$desc=='codo izq.'])^2)^0.5
phi0=atan(  # Ángulo brazos vs eje Z
    (jug$y[jug$desc=='codo izq.']-jug$y[jug$desc=='hombro izq.'])/
   ((jug$z[jug$desc=='hombro izq.']-jug$z[jug$desc=='codo izq.'])))
dphi=abs(phi0*0.12)  # Oscilación brazo vs tronco

# Piernas
L=125  # Long. muslo
l=125  # Long. pierna
d=jug$x[jug$desc=='pie der.']  # dder=-dizq
ZFOOT=jug$z[jug$desc=='pie der.']

# Antebrazos
alpha=atan(  # Ángulo antebrazos vs plano XZ
    (jug$y[jug$desc=='mano izq.']-jug$y[jug$desc=='codo izq.'])/
   ((jug$x[jug$desc=='mano izq.']-jug$x[jug$desc=='codo izq.'])))
forearmXY=((jug$x[jug$desc=='mano izq.']-jug$x[jug$desc=='codo izq.'])^2+
           (jug$y[jug$desc=='mano izq.']-jug$y[jug$desc=='codo izq.'])^2)^0.5
forearm=(forearmXY^2+(jug$z[jug$desc=='mano izq.']-jug$z[jug$desc=='codo izq.'])^2)^0.5
gamma0=atan(  # Ángulo antebrazos vs plano XY
    (jug$z[jug$desc=='mano izq.']-jug$z[jug$desc=='codo izq.'])/forearmXY)
dgamma=abs(gamma0*0.6)  # Oscilación antebrazo vs vertical

# Malabares
D=jug$y[jug$desc=='bola izq.']-jug$y[jug$desc=='bola der.']
v0y=D/N
Hlo=85  # Altura tiro parabólico inferior...
Hhi=jug$z[jug$desc=='bola sup.']-jug$z[jug$desc=='bola der.']  # ...y superior
glo=8*Hlo/N^2  # Gravedad tiro parabólico inferior...
ghi=2*Hhi/N^2  # ...y superior
v0zlo=glo*N/2
v0zhi=ghi*N
Href=jug$z[jug$desc=='bola der.']  # Altura ref. malabares

# Dirección de la luz (fuente en el oo)
xd=50
yd=10
zd=45
scale=(xd^2+yd^2+zd^2)^0.5/zd  # Elongación elíptica 
theta=atan(yd/xd)  # Ángulo respecto al eje X del haz de luz
ZSHADOW=0.001
ALPHASHADOW=0.5


for (f in 0:(N-1)) {  # f=frame
  # Paráms. geométricos dinámicos
  
  # Caderas, tronco y brazos ("biela")
  beta=2*pi*f/N
  cb=Rb*sin(beta)
  bb=(Rb^2-cb^2)^0.5
  ab=(Lb^2-cb^2)^0.5
  hb=Lb+Rb*(1-cos(beta))-ab
  phi=phi0+dphi*sin(beta)
  
  # Piernas
  h=jug$z[jug$desc=='cadera der.']-jug$z[jug$desc=='pie der.']+hb
  H=(h^2+d^2)^0.5
  a=(H^2-L^2+l^2)/(2*H)
  b=(H^2-l^2+L^2)/(2*H)
  c=(L^2-b^2)^0.5
  
  # Antebrazos
  gamma=gamma0+dgamma*sin(beta)

    
  # Partimos de posición equilibrio
  jug2=jug
  
  # Cinemática caderas, tronco y brazos ("biela")
  jug2$y[jug2$desc=='codo der.']=jug$y[jug$desc=='hombro der.']-arm*sin(phi)
  jug2$y[jug2$desc=='codo izq.']=jug$y[jug$desc=='hombro izq.']+arm*sin(phi)
  jug2$z[jug2$desc=='codo der.']=jug$z[jug$desc=='hombro der.']-arm*cos(phi)
  jug2$z[jug2$desc=='codo izq.']=jug$z[jug$desc=='hombro izq.']-arm*cos(phi)
  ZREF=jug$z[jug$desc=='pelvis']
  for (i in c(1:9,11:12)) {  # Nos saltamos las manos
    db=jug2$y[i]
    Ll=jug2$z[i]-ZREF  # L+l
    jug2$y[i]=   (Ll*cb+db*ab)/Lb
    jug2$z[i]=hb+(Ll*ab-db*cb)/Lb+ZREF
  }
  
  # Cinemática piernas
  jug2$z[jug2$desc=='cadera der.']=jug$z[jug$desc=='cadera der.']+hb
  jug2$z[jug2$desc=='cadera izq.']=jug$z[jug$desc=='cadera izq.']+hb
  jug2$x[jug2$desc=='rodilla der.']= d-(a*  d -c*h)/H
  jug2$x[jug2$desc=='rodilla izq.']=-d-(a*(-d)-c*h)/H
  jug2$z[jug2$desc=='rodilla der.']=(a*h+c*d)/H + ZFOOT
  jug2$z[jug2$desc=='rodilla izq.']=(a*h-c*d)/H + ZFOOT
  
  # Cinemática antebrazos
  jug2$x[jug2$desc=='mano der.']=forearm*cos(gamma)*cos(alpha)
  jug2$x[jug2$desc=='mano izq.']=forearm*cos(gamma)*cos(alpha)
  jug2$y[jug2$desc=='mano der.']=jug2$y[jug2$desc=='codo der.']-
                                 forearm*cos(gamma)*sin(alpha)
  jug2$y[jug2$desc=='mano izq.']=jug2$y[jug2$desc=='codo izq.']+
                                 forearm*cos(gamma)*sin(alpha)
  jug2$z[jug2$desc=='mano der.']=jug2$z[jug2$desc=='codo der.']+
                                 forearm*sin(gamma)
  jug2$z[jug2$desc=='mano izq.']=jug2$z[jug2$desc=='codo izq.']+
                                 forearm*sin(gamma)

  # Cinemática malabares
  jug2$y[jug2$desc=='bola der.']=v0y*f-D/2
  jug2$z[jug2$desc=='bola der.']=v0zlo*f-glo*f^2/2+Href
  jug2$y[jug2$desc=='bola izq.']=D/2-v0y/2*f
  jug2$z[jug2$desc=='bola izq.']=v0zhi*f-ghi*f^2/2+Href
  jug2$y[jug2$desc=='bola sup.']=-v0y/2*f
  jug2$z[jug2$desc=='bola sup.']=v0zhi*(N-1-f)-ghi*(N-1-f)^2/2+Href
  
  open3d()
  
  # Dibujar suelo a cuadros
  z=array(0,c(2,2))  
  for (x in -2:0) {
    for (y in -2:0) {
      persp3d(x = 0:1+2*x-1, y = 0:1+2*y-1, z, col='yellow', add=T)
      persp3d(x = 0:1+2*x,   y = 0:1+2*y,   z, col='yellow', add=T)
      persp3d(x = 0:1+2*x,   y = 0:1+2*y-1, z, col='green',  add=T)
      persp3d(x = 0:1+2*x-1, y = 0:1+2*y,   z, col='green',  add=T)
    }
  }

  # Dibujar Juggler
  jug2[,1:4]=jug2[,1:4]/SCALE  # Escalamos al universo el dataframe recalculado
  jug2[nrow(jug2)+1,1:4] = c(0,0,0,0)  # EOF
  
  for (i in 1:(nrow(jug2)-1)) {  # Última fila de jug2[] = EOF
    ninterp=jug2$ninterp[i]
    dx=(jug2$x[i+1]-jug2$x[i])/(ninterp+1)
    dy=(jug2$y[i+1]-jug2$y[i])/(ninterp+1)
    dz=(jug2$z[i+1]-jug2$z[i])/(ninterp+1)
    dr=(jug2$radius[i+1]-jug2$radius[i])/(ninterp+1)
    
    for (n in 1:(ninterp+1)) {  # No dibujamos la última para no duplicar
      x=jug2$x[i]+dx*(n-1)
      y=jug2$y[i]+dy*(n-1)
      z=jug2$z[i]+dz*(n-1)
      r=jug2$radius[i]+dr*(n-1)
      
      # Esferas
      spheres3d.plus(x, y, z, r,
        color=iif(substr(jug2$desc[i], 1, 4)=='bola',
          terrain.colors(5), jug2$colour[i]),
        alpha=jug2$alpha[i], n=NRES)
      
      # Sombras
      lambdas=-z/zd
      xs=x+lambdas*xd
      ys=y+lambdas*yd
      ellipse.xy(xs, ys, ZSHADOW, r, scale=scale, theta=theta,
        col='black', alpha=ALPHASHADOW, n=NRES)
    }
  }
  
  # Acondicionar escena
  
  # Iluminación
  clear3d("lights")
  light3d(x=xd, y=yd, z=zd, viewpoint.rel=F)  # specular="#999999"
  bg3d(color="lightblue")
  
  # Punto de vista
  
  # (Colocación de esferas invisibles omitida)
  view3d(theta=0, phi=-88)  # Ajuste de phi
  um=par3d()$userMatrix
  um=rotate3d(um, -pi/2-pi/4*1.006, 0, 0, 1)
  par3d(FOV=110, zoom=0.33, userMatrix=um,
      windowRect=c(100, 100, WIDTH+100, HEIGHT+100))

  # Guardamos frame en PNG
  snapshot3d(paste0("amigajuggler", iif(f<10, "00", iif(f<100, "0", "")),
      f, ".png"), fmt="png", top=T)
  rgl.close()
  print(paste0("Frame ", f+1, "/", N))
  
}


# SONIDO 'PING!'

library(tuneR)

# Análisis sonido original (22KHz, 8 bits)
jugglerwav=readWave("amigajuggler.wav")
play(jugglerwav)
jugglerwav

dft=abs(fft(jugglerwav@left-mean(jugglerwav@left)))
N=round(length(dft)/2)  # Primera mitad de la FFT
maxfreq=jugglerwav@samp.rate/2/1000  # Máx. frecuencia FFT en KHz
plot(seq(from=0, to=maxfreq, len=N),
    dft[1:N]/max(dft), main='FFT "amigajuggler.wav"',
    xlab='Frecuencia (KHz)', ylab='Amplitud', col='red', type='l', axes=F)
axis(side=1, at=c(0:maxfreq))

# Regeneramos sonido con más calidad (44KHz, 16 bits)
fs=44100
bits=16
T=0.5  # 0.5s de duración
N=fs*T  # Long. en muestras
t=seq(from=0, to=T, len=N)  # t en s
tono1KHz=sin(2*pi*1000*t)

# Envolvente ADSR (sin fase de sostenimiento)
ADSR=seq(N)
nA=round(0.01*N)  # A: hasta 1%
nD=round(0.6*N)  # D: hasta 60%
lD=0.1  # Nivel D (10%)
ADSR[1 :nA]=seq(from=0,  to=1,  len=nA)  # A
ADSR[nA:nD]=seq(from=1,  to=lD, len=nD-nA+1)  # D
ADSR[nD:N ]=seq(from=lD, to=0,  len=N-nD+1)  # R
plot(t, ADSR, type='l', main='Envolvente ADSR', xlab='Tiempo (s)',
    ylab='Amplitud', col='red')
abline(h=0, col='black', lty='dotted')

restauracion=tono1KHz*ADSR
restauracion=restauracion/max(abs(restauracion))

# Volcamos al WAV preexistente ajustando parámetros y guardamos
jugglerwav@left=round(restauracion*(2^(bits-1)-1))
jugglerwav@samp.rate=fs
jugglerwav@bit=bits
writeWave(jugglerwav, filename="amigajugglerrestauracion.wav")


# SALIDA VÍDEO

# MP4 (MPEG-4 AVC/H.264):
# ffmpeg -loop 1 -framerate 30 -i amigajuggler%3d.png -i amigajuggler30s.wav /
# -t 30 -c:v libx264 -crf 15 -pix_fmt yuv420p amigajuggler.mp4

# GIF animado:
# magick -delay 3 -loop 0 amigajuggler*.png amigajuggler.gif
