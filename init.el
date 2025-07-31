;; Desactiva flechas
(global-set-key (kbd "<left>") 'ignore)
(global-set-key (kbd "<right>") 'ignore)
(global-set-key (kbd "<up>") 'ignore)
(global-set-key (kbd "<down>") 'ignore)

;; No cortar líneas automáticamente al escribir
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; Mostrar líneas largas en una sola línea (no truncarlas visualmente)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Desactivar visual-line-mode en todos los buffers
(when (fboundp 'visual-line-mode)
  (global-visual-line-mode -1))


;; Carga el juego
(load-file ".juego.el")

;; Abre nivel 1
(find-file "niveles/nivel1.txt")

