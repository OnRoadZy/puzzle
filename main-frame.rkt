;main-frame.rkt
;拼图GUI界面：
#lang racket/gui

(require "data.rkt")

(provide create-main-frame
         show-main-frame
         init-main-frame)

;视图控件变量================================
(define main-frame void);主框架
(define canvas/target void);目标图片画布
(define canvas/puzzle void);拼图画布
(define button/select-picture void);选择图片按钮
(define button/blend-cells void);混合拼图按钮
(define button/play-again void);再来一次按钮
(define button/exit void);退出按钮
(define text/row void);行数设置按钮
(define text/col void);列数设置按钮
(define choice/r&c void);行列数选择按钮
(define message/status void);信息状态显示条

;创建视图及布局=============================
;创建界面主框架：
(define (create-main-frame)
  (set! main-frame
        (new frame%
             [label "拼图"]
             [width 1000]
             [height 600]
             [style '(no-resize-border
                      no-system-menu)]))
  (create-layout))

;创建布局：
(define (create-layout)
  (let ([hp (new horizontal-pane%
                 [parent main-frame])])
    (create-left-layout hp)
    (create-right-layout hp)))

;创建左侧布局：
(define (create-left-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)]
                 [min-width 300]
                 [stretchable-width #f])])
    ;创建目标图画布：
    (set! canvas/target
          (new canvas%
               [parent vp] 
               [horiz-margin 3]
               [paint-callback
                  canvas/target-callback]))
    ;创建命令功能区：
    (create-command-area vp)
    ;创建状态显示区：
    (create-status-area vp)))

;创建设置功能区：
(define (create-command-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(center top)])])
    (create-row&col-set-group vp)
    (create-puzzle-command-button-group vp)
    (set! button/exit
        (new button%
             [label "退出"]
             [parent vp]
             [callback button/exit-callback]))))

;定义行列设置区：
(define (create-row&col-set-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图排列"]
                 [parent p]
                 [min-height 30]
                 [stretchable-height #f])])
    (let ([gp/hp (new horizontal-pane%
                      [parent gp])])
      (set! text/row (new text-field%
                          [label "行数"]
                          [parent gp/hp]
                          [init-value "3"]
                          [callback text/row-callback]))
      (set! text/col (new text-field%
                          [label "列数"]
                          [parent gp/hp]
                          [init-value "3"]
                          [callback text/col-callback])))
    (let ([gp/hp-r&c (new horizontal-pane%
                          [parent gp])])
      (set! choice/r&c (new choice%
                            [label "行列"]
                            [parent gp/hp-r&c]
                            [stretchable-width #t]
                            [choices '("自定义行列数"
                                       "3行3列"
                                       "5行5列"
                                       "8行8列"
                                       "10行10列"
                                       "13行13列"
                                       "15行15列")]
                            [callback choice/r&c-callback])))))
  
;定义拼图命令按钮区：
(define (create-puzzle-command-button-group p)
  (let ([gp (new group-box-panel%
                 [label "拼图命令"]
                 [parent p]
                 [stretchable-height #f])])
    (create-button button/select-picture gp "选择..."
                   button/select-picture-callback)
    (create-button button/blend-cells gp "混合拼图"
                   button/blend-cells-callback)
    (create-button button/play-again gp "再来一次"
                   button/play-again-callback)))

;定义创建命令功能区按钮的宏：
(define-syntax-rule (create-button n p l prc)
  (set! n
        (new button%
             [label l]
             [parent p]
             [stretchable-width #t]
             [callback prc])))

;创建状态显示区：
(define (create-status-area p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [alignment '(left top)]
                 [min-height 30]
                 [stretchable-height #f])])
    (set! message/status
          (new message%
               [label "准备就绪。"]
               [parent vp]))))

;创建右侧布局：
(define (create-right-layout p)
  (let ([vp (new vertical-panel%
                 [parent p]
                 [style '(border)])])
    (set! canvas/puzzle
          (new puzzle-canvas%
               [parent vp]
               [paint-callback
                (lambda (c dc)
                  (draw-puzzle-picture dc))]))))

;;画布类定义==============================================
;混合图片画布类：
(define puzzle-canvas%
  (class canvas%
    (inherit get-dc
             get-x
             get-y)
    
    ;类字段：
    (field (dragging #f);单元格拖动状态
           (over #f));游戏结束标志

    (super-new)
    
    ;覆盖定义鼠标事件：
    (define/override (on-event event)
      ;如果游戏结束，不再处理鼠标事件：
      (unless over
        (let ([dc (get-dc)]
              [x (send event get-x)]
              [y (send event get-y)])
          (msg-mouse-pos x y);显示鼠标位置信息。
          ;根据事件情况进行动态显示：
          (cond
            ;移动但不拖拽。
            ;对鼠标位置单元格进行标记并记录为当前单元格：
            [(and (send event moving?)
                  (not (send event dragging?)))
             (let ([id (x&y->id x y)])
               (unless (= id (get-current-id))
                 (begin
                   (draw-puzzle-picture dc)
                   (define id/old (get-current-id))
                   (draw-lose-focus-cell dc)
                   (when (has-id? id)
                     (set-current-id! id)
                     (draw-focus-cell dc)))))]
            ;拖拽并移动。
            ;移动当前单元格，在进入其它单元格时模拟调换场景：
            [(and (send event moving?)
                  (send event dragging?))
             (let ([id/target (drag-x&y->id x y)]
                   [id (get-current-id)])
               (if (has-id? id/target)
                   ;绘制两个单元格调换状态：
                   (begin
                     (draw-puzzle-picture dc)
                     (draw-blank-cell id/target dc)
                     (draw-cell-to-id/target id id/target dc)
                     (draw-cell-to-x&y x y id dc))
                   ;绘制当前单元格拖动状态：
                   (begin
                     (draw-puzzle-picture dc)
                     (draw-blank-cell id dc)
                     (draw-cell-to-x&y x y id dc)))
               ;设置单元格拖动状态为#t：
               (unless dragging (set! dragging #t)))]
            ;释放鼠标左键。
            ;将当前单元格与目标单元格调换：
            [(and (send event button-up? 'left)
                  dragging)
             (let ([id/target (drag-x&y->id x y)])
               (if (has-id? id/target)
                   (let ([id (get-current-id)])
                     ;将两个单元格调换：
                     (swap-cell id/target id)
                     (set-puzzle-picture!)
                     (draw-puzzle-picture dc)
                     ;检查是否成功拼合图片：
                     (when (success?)7
                       (begin
                         (msg-string "恭喜！成功完成拼图！")
                         (set! over #t))))
                   ;重新显示混合图片：
                   (draw-puzzle-picture dc))
               ;设置单元格拖动状态为#f：
               (when dragging (set! dragging #f)))]
            [else
             (draw-puzzle-picture dc)]))))

    ;设置游戏重启：
    (define/public (replay)
      (set! over #f))
    ))

;;控件响应函数=================================
;目标画布响应函数：
(define (canvas/target-callback control dc)
  (draw-target-picture dc))

;退出按钮响应函数：
(define (button/exit-callback control event)
  (send main-frame on-exit))

;text/row控件响应函数：
(define (text/row-callback control event)
  (text/r&c-callback/in control "行"))

;text/col控件响应函数：
(define (text/col-callback control event)
  (text/r&c-callback/in control "列"))

;行列控件响应函数模板：
(define-syntax-rule (text/r&c-callback/in control r/c)
  (let [(v (string->number (send control get-value)))]
    (if v
        (if (> v 0)
            (begin
              (msg-string (format "设置为~a~a。" v r/c))
              (unless (= (send choice/r&c get-selection)
                         0)
                (send choice/r&c set-selection 0)))
            (begin
              (send control set-value "")
              (msg-string
               (format "输入错误！~a必须大于0。" r/c))))
        (begin
          (send control set-value "")
          (msg-string
           (format "输入错误！~a数应该是数字。" r/c))))))

;choice/r&c行列选择框响应函数：
(define (choice/r&c-callback control event)
  (let ([i (send control get-selection)])
    (case i
      [(1) (set-r&c-text-value 3 3)]
      [(2) (set-r&c-text-value 5 5)]
      [(3) (set-r&c-text-value 8 8)]
      [(4) (set-r&c-text-value 10 10)]
      [(5) (set-r&c-text-value 13 13)]
      [(6) (set-r&c-text-value 15 15)])))

;设置行/列text-field%控件值：
(define (set-r&c-text-value r c)
  ;设置行/列控件值：
  (send text/row set-value (number->string r))
  (send text/col set-value (number->string c))
  (msg-string (format "设置为~a行，~a列。" r c)))

;显示鼠标坐标：
(define (msg-mouse-pos x y)
  (send message/status set-label
        (format "当前光标位置：x=~a，y=~a" x y)))

;显示子串信息：
(define (msg-string str)
  (send message/status set-label str))

;button/select-picture选择图片按钮响应函数：
(define (button/select-picture-callback control event)
  ;根据用户输入取得源图片：
  (let ([in (get-file "请选择你需要的图片"
                        main-frame
                        (current-directory)
                        #f
                        "*.jp*g;*.png"
                        null
                        '(("JPG图片" "*.jpg")
                          ("JPEG图片" "*.jpeg")
                          ("PNG图片" "*.png")))])
    (get-source-picture in)
    (adjust-to-picture))
  ;重置拼图：
  (reset-puzzle-picture)
  ;绘制目标图片：
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))

;button/blend-cells混合图片按钮响应函数：
(define (button/blend-cells-callback control event)
  (when (setted-r&c-value?)
    (begin
  ;初始化宽高：
  (set-picture-size! (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  ;初始化行列数：
  (string->number (send text/row get-value))
  (string->number (send text/col get-value))
  ;重置混合图片：
  (reset-puzzle-picture)
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))))

;设置行列数，如果不成功返回#f：
(define (setted-r&c-value?)
  (let ([r (string->number (send text/row get-value))]
        [c (string->number (send text/col get-value))])
    (if (and (and r c)
             (> r 0)
             (> c 0))
        (begin
          (set-rows! r)
          (set-cols! c)
          #t)
        (begin
          (cond
            [(and (not r) (not c))
             (msg-string "行数及列数必须是数字。")]
            [(and (<= r 0) (<= c 0))
             (msg-string "行数及列数必须是大于0的数字。")]
            [(not r)
             (msg-string "行数必须是数字。")]
            [(not c)
             (msg-string "列数必须是数字。")]
            [(<= r 0)
             (msg-string "行数必须是大于0的数字。")]
            [(<= c 0)
             (msg-string "列数必须是大于0的数字。")])
          #f))))
                        
;button/play-again再来一次按钮响应函数：
(define (button/play-again-callback control event)
  (recover-puzzle-picture)
  ;重新显示混合图片：
  (send canvas/puzzle refresh-now draw-puzzle-picture)
  (send canvas/puzzle replay)
  (msg-string "再来一次！"))

;;==============================================
;初始化主界面：
(define (init-main-frame)
  ;初始化宽高：
  (set-picture-size! (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  ;初始化行列数：
  (set-rows! (string->number (send text/row get-value)))
  (set-cols! (string->number (send text/col get-value)))
  ;初始化拼图数据：
  (init-puzzle-data)
  ;绘制目标图片：
  (set-target-size);设置目标图片画布尺寸
  (draw-target-picture (send canvas/target get-dc))
  ;绘制拼图图片：
  (draw-puzzle-picture (send canvas/puzzle get-dc))
  ;设置拼图画布重玩：
  (send canvas/puzzle replay))

;设置目标图片画布尺寸：
(define (set-target-size)
  (let* ([w/p (send canvas/puzzle get-width)]
         [h/p (send canvas/puzzle get-height)]
         [w/t (send canvas/target get-width)]
         [h/t (floor (* h/p (/ w/t w/p)))])
    (send canvas/target min-height h/t)))

;显示主界面：
(define (show-main-frame)
  (send main-frame show #t))

;测试========================================
(module+ test
  (create-main-frame)
  (send main-frame show #t)
  (init-main-frame)
  #|
  ;取得混合的图片：
  (define in (build-path
              (current-directory)
              "test"
              "test.jpg"))
  (adjust-to-picture (read-bitmap in)
                     (send canvas/puzzle get-width)
                     (send canvas/puzzle get-height))
  (reset-puzzle-picture)
  (define dc (send canvas/puzzle get-dc))
  (draw-puzzle-picture dc)
  (draw-cell-to-x&y 127 77 3 dc)
|#
  )
