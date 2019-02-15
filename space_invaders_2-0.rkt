;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;SPACE INVADERS BY CRISTIAN RUZ 


(require 2htdp/universe 2htdp/image)
(require 2htdp/planetcute)
(require racket/list)
(require racket/gui/base)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;estructuras para gestionar los datos necesarios

(define-struct pos (x y)) ;;representa un punto en el espacio

(define-struct player (posicion)) ;;representa al jugador

(define-struct ufo (x y direction)) ;; representa un ufo enemigo

(define-struct bullet (x y)  ) ;; representa una bala

(define-struct world (player ufos bullets)) ;; representa la estructura que maneja la funcion bigbang (ufos y bullets son listas de estructuras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;definiciones generales

(define PLAYER-INITIAL-X 500)
(define PLAYER-INITIAL-Y 500)
(define AVANCE-X 20)
(define WIDTH 800)     ;; limite ufos
(define HEIGHT 500)    
(define BULLET_DIST 7) ;; distancia que recorre una bullet
(define SIZE 10)       ;; tamano de bloque
(define UFO-WIDTH 50)
(define MAX-BULLETS 4) ;; numero maximo de bullets en la ventana
(define FONDO (bitmap/file "fondo.jpg")) ;;imagen de fondo del juego 

(define NAVE (make-player (make-pos PLAYER-INITIAL-X PLAYER-INITIAL-Y))) ;; condiciones iniciales del jugador

(define BLOCK ;; bloque para construir los ufos
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "green")
           (rectangle SIZE SIZE "outline" "black")))

(define BLOCK2 ;; bloque para construir los ufos
  (overlay (rectangle (- SIZE 1) (- SIZE 1) "solid" "red")
           (rectangle SIZE SIZE "outline" "black")))

;;codigo extraido de la documentacion de racket 

; stack : non-empty-list-of-images -> image
; stacks 'imgs' on each other, separated by 40 pixels

(define (stack imgs)
  (cond
    [(empty? (rest imgs)) (first imgs)]
    [else (overlay/xy (first imgs)
                      0 40
                      (stack (rest imgs)))]))


;;suelo realizado con la libreria planetcute

(define BACKGROUND 
  (beside
         (stack (list tree-tall dirt-block))
         (stack (list dirt-block))
         (stack (list grass-block))
        (stack (list grass-block))
        (stack (list grass-block))
        (stack (list grass-block))
        (stack (list grass-block))
        (stack (list grass-block))
         (stack (list dirt-block))
         (stack (list rock dirt-block))
    
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; Nombre: get-player-x
;; Objetivo: obtener la coordenada x del player
;; Parametros: w representa la estructura wordl
;;       
;; Resultado: coordenada x del player
;;        
;; Descripcion: accede a la estructura y devuelve la coordenada x
;;      
;; Funciones a las que llama: 
;;       
;;
(define (get-player-x w)
  (pos-x (player-posicion (world-player w)))
)


;; 
;; Nombre: get-player-x
;; Objetivo: obtener la coordenada x del player
;; Parametros: w representa la estructura wordl
;;       
;; Resultado: coordenada x del player
;;        
;; Descripcion: accede a la estructura y devuelve la coordenada x
;;      
;; Funciones a las que llama: 
;;       
;;
(define (get-player-y w)
  (pos-y (player-posicion (world-player w)))
)

;; 
;; Nombre: draw-ufo
;; Objetivo: dibuja un ufo
;; Parametros: ufo (estructura) background imagen 
;;       
;; Resultado: dibuja un ufo en pantalla
;;        
;; Descripcion:
;;      
;; Funciones a las que llama: 
;;       
;;

(define (draw-ufo ufo background)
  (place-image/align BLOCK (ufo-x ufo) (ufo-y ufo) "left" "bottom"
   (place-image/align BLOCK (- (ufo-x ufo) SIZE) (ufo-y ufo) "left" "bottom"
    (place-image/align BLOCK (+ (ufo-x ufo) SIZE) (ufo-y ufo) "left" "bottom"
     (place-image/align BLOCK (ufo-x ufo) (+ (ufo-y ufo) SIZE) "left" "bottom"
         background)))))

;; 
;; Nombre: draw-bullet
;; Objetivo: dibujar una bullet
;; Parametros: bullet (estructura) background imagen
;;       
;; Resultado: dibuja un bullet en pantalla
;;        
;; Descripcion:
;;      
;; Funciones a las que llama: 
;;       
;;

(define (draw-bullet bullet background)
   (place-image/align BLOCK2 (bullet-x bullet) 
                            (bullet-y bullet) "left" "bottom" background))

;;recorre la lista de bullets y por cada una usa la funcion draw-bullet
(define (draw-bullets bullets background)
  (cond ((empty? bullets) background)
        (else (draw-bullet (first bullets) 
                           (draw-bullets (rest bullets) background)))))

;;recorre la lista de ufos y por cada uno usa la funcion draw-ufo
(define (draw-ufos ufos background)
  (cond ((empty? ufos)  background)
        (else (draw-ufo (first ufos) (draw-ufos (rest ufos) background)))))

;;dibuja al jugador en pantalla
(define (draw-nave w background)
  (place-image
   
   character-boy (get-player-x w) 500 
   (place-image
    BACKGROUND 500 520
    background)
  )
)

;; 
;; Nombre: draw-last-scene
;; Objetivo: dibujar la ultima escena del juego que se activa con stop-when
;; Parametros: world 
;;       
;; Resultado: Pantalla que indica el final del juego y tu puntuacion
;;        
;; Descripcion:
;;      
;; Funciones a las que llama: 
;;       
;;
(define (draw-last-scene world)
  (define score-final "1000")
  (place-image/align (text "FIN DEL JUEGO" 50 "white") 500 250 "center" "center"
  (place-image/align (text "Tu puntuacion es :" 45 "white") 500 300 "center" "center"
  (place-image/align (text score-final 40 "white") 500 350 "center" "center"
  FONDO)))

)

;; 
;; Nombre: make-ufos-for-y
;; Objetivo: crear una fila de ufos
;; Parametros:y-cordinate coordenada y ufos lista de ufos
;;       
;; Resultado: una fila de ufos (misma coordenada y)
;;        
;; Descripcion:
;;      
;; Funciones a las que llama: 
;;       
;;
(define (make-ufos-for-y y-cordinate ufos)
  (cond ((empty? ufos) (make-ufos-for-y y-cordinate  
                                        (cons (make-ufo 50 y-cordinate 1) ufos)))
        ((> (+ (ufo-x (first ufos)) UFO-WIDTH) WIDTH) ufos)
        (else ;;(printf  "first ~a" (+ (ufo-x (first ufos))))
              (make-ufos-for-y y-cordinate 
                               (cons  (make-ufo (+ (ufo-x (first ufos)) 
                                           UFO-WIDTH) y-cordinate 1) ufos )))))


;; 
;; Nombre: make-ufos
;; Objetivo: crear una lista de ufos
;; Parametros: rows (filas) ufos 
;;       
;; Resultado: lista de ufos para añadir a world
;;        
;; Descripcion:
;;      
;; Funciones a las que llama: make-ufos-for-y
;;       
;;
(define (make-ufos rows ufos)
  (cond ((= 0 rows) ufos)
        (else (make-ufos (- rows 1) 
                         (append ufos (make-ufos-for-y (- ( * rows 60) 30) 
                                                       empty))))))

;; 
;; Nombre: render-scene
;; Objetivo: dibujar la escena que se ve por pantalla en cada tick
;; Parametros: wordl
;;       
;; Resultado: escena 
;;        
;; Descripcion: usa las funciones de dibujado del personaje ufos y bullets 
;; usando el parametro background de cada una para dibujar la siguiente
;;      
;; Funciones a las que llama: draw-ufos draw-nave draw-bullets
;;       
;;

(define (render-scene world)
  (draw-ufos (world-ufos world)
             (draw-nave world 
                        (draw-bullets (world-bullets world)
                                      FONDO))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funciones que manejan el movimiento del player 
;;crean una nueva estructura player con la coordenada x actualizada 
;;y crean una nueva estructura wordl con los datos actualizados

(define (move-left w)
  (define NEW_NAVE (make-player (make-pos (- (get-player-x w) AVANCE-X) PLAYER-INITIAL-Y)))
  (make-world NEW_NAVE (world-ufos w) (world-bullets w))
)

(define (move-right w)
  (define NEW_NAVE (make-player (make-pos (+ (get-player-x w) AVANCE-X) PLAYER-INITIAL-Y)))
  (make-world NEW_NAVE (world-ufos w) (world-bullets w))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funciones que se encargan de las bullets
;;fire-tank crea la bullet 
;;move bullet se encarga del movimiento individual de cada bullet sumandole el avance establecido a su coordenada y
;;move.bullets recorre la lista de bullets aplicandole move-bullet

(define (fire-tank world)
  (cond ((eq? MAX-BULLETS (length (world-bullets world)))  world);;comprueba que no se supere el maximo de bullets
        (else ;;se crea una nueva bullet con las coordenadas iniciales de donde se encontraba el player
         (begin (play-sound "bullet.wav" #t)(make-world (world-player world)(world-ufos world)(cons (make-bullet (get-player-x world )(get-player-y world)) (world-bullets world) ))))))


(define (move-bullet bullet)
  (make-bullet (bullet-x bullet) (- (bullet-y bullet) BULLET_DIST)))

(define (move-bullets bullets)
  (cond ((empty? bullets) bullets)
        (else  (cond ((> 0 (bullet-y (first bullets))) 
                         (move-bullets (rest bullets)))
                     (else
                         (cons (move-bullet (first bullets))
                               (move-bullets (rest bullets))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funciones encargadas del movimiento de los ufos
;;future ufo comprueba donde estaria el ufo en el proximo movimiento
;;move ufo cambia la coordenada del ufo y su direccion en caso de que llegue 
;;a los limites establecidos
;;change-ufo-direction cambia la direccion del ufo
;;move-ufos recorre la lista de ufos y aplica move-ufo

(define (future-ufo-x  ufo)
   (+ (* 4 (ufo-direction ufo)) (ufo-x ufo) ))


(define (move-ufo ufo)
 ;;(future-ufo-x  ufo) (ufo-direction ufo) WIDTH)
  (cond ((> 50 (future-ufo-x ufo))  (change-ufo-direction ufo))
        ((< (- 900 10) (future-ufo-x  ufo)) (change-ufo-direction ufo))
        (else    (make-ufo (future-ufo-x  ufo) 
                           (ufo-y ufo) (ufo-direction ufo)))))
(define (change-ufo-direction ufo)
  (make-ufo (ufo-x ufo) (+ (ufo-y ufo) 30) (* -1 (ufo-direction ufo))))

(define (move-ufos ufos)
  (cond ((empty? ufos) ufos)
        (else (cons (move-ufo (first ufos)) (move-ufos (rest ufos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funciones que se encargan de controlar la colision entre bullets y ufos
;;between? predicado utilizado para comprobar si un numero se encuentra en un rango
;;collide-helper? comprueba la colision individual de una bullet y un ufo
;;collide? recorre la lista de bullets y comprueba si alguno colisiona usando collide-helper?
;;bullet-collision-ufos recorre la lista de ufos comprobando si alguno colisiona con alguna bullet

(define (between? x low high)
  (cond ((and (> x low) (< x high) true))
        (else false)))


(define (collide-helper?  bullet ufo)
  (cond ((and (between? (bullet-x bullet) (- (ufo-x ufo) SIZE 5) 
                                          (+ (ufo-x ufo)  SIZE 5))
              (between? (bullet-y bullet) (- (ufo-y ufo) SIZE 5)
                                          (+ (ufo-y ufo) SIZE 5)))
               true)
        (else false)))

(define (collide? ufo bullets)
  (cond ((empty? bullets) false)
        ((collide-helper?  (first bullets) ufo) true)
        ( else (collide? ufo (rest bullets)))))






(define (bullet-collision-ufos ufos bullets)
  (cond ((empty? ufos)       ufos )
        (else (cond ((collide? (first ufos) bullets) 
                               (bullet-collision-ufos (rest ufos) bullets))
                    (else (cons (first ufos) 
                                (bullet-collision-ufos (rest ufos) bullets)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funcion que se activa en cada tick y actualiza el estado de todos
;;los elementos del juego

(define (progress-world world)
  (define NEW_NAVE (world-player world))
  (define NEW_UFOS (bullet-collision-ufos  (move-ufos (world-ufos world)) (world-bullets world))) 
  (define NEW_BULLETS (move-bullets (world-bullets world)))
        ;;(display (length (world-ufos world)))

  (make-world NEW_NAVE NEW_UFOS NEW_BULLETS)

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funciones encargadas de finalizar el juego
;;comprueban si se ha acabado con todos los ufos
;;o si alguno a llegado al limite

(define (ufo-limit ufos)
  (if (ufo? ufos);;evitamos fallos cuando solo queda un ufo y por tanto no es una lista
      (if (> (ufo-y  ufos) 450);;si rebasa esa coordenada el juego debe finalizar
      #t
      #f)
  (if (empty? ufos)
      #f
  (if (> (ufo-y (first ufos)) 450)
      #t
      #f)))
)

(define (check-all-ufos world);;recorre la lista de ufos comprobando si no estan en el limite
  (cond ((empty? world-ufos) #f)
        (else
          (cond ((ufo-limit (world-ufos world)) #t)
                (else
                  (ufo-limit (rest (world-ufos world)))
                )
           )
        )
      )
)


(define (end-game? world)
  (cond ( (empty? (world-ufos world)) #t)
              ( (check-all-ufos world)  #t)
        ( (empty? (world-ufos world)) #t)
        (else #f)
      )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funcion generica para tratar los controles del juego
(define (deal-with-guess w key)
(cond [(key=? key "left") (move-left w)]
        [(key=? key "right") (move-right w)]
        [(key=? key " ") (fire-tank w)]
        [else w]))
(define w (make-world NAVE (make-ufos 3 empty ) empty) )

(define (game)
(play-sound "melody.wav" #t);;musica de fondo
(big-bang w
          (to-draw render-scene)
          (on-key deal-with-guess)
          (on-tick progress-world 0.001)
          (name "space invaders")
          (stop-when end-game? draw-last-scene))
  
  
)

(game)