;;; opengl.el
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, April 2008
;; 
;; This is a minor mode for editing C/C++ programs that use OpenGL. It
;; highlights OpenGL functions nicely and lets you look up docs
;; online.
;;
;; If you have a copy of the OpenGL reference locally, set
;; `opengl-url'. If you renamed them to .html files, set `opengl-ext'
;; to ".html". You might have done this because w3m displays .xml
;; files as plain text, and the official OpenGL reference uses that
;; extension...
;;
;; To enable,
;; (require 'opengl)
;; M-x opengl-mode
;;
;; or (add-hook 'c-mode-common-hook 'turn-on-opengl-mode)


;; User variables
(defvar opengl-ext ".xml" "extension to find page names")
(defvar opengl-url "http://www.opengl.org/sdk/docs/man/xhtml/"
  "opengl reference pages are at `opengl-url' functionName 'opengl-ext'")
(defvar opengl-redbook-url "http://www.glprogramming.com/red/"
  "Url to find the opengl programming guide")


(defvar opengl-function-name-list
  '("glAccum" "glActiveTexture" "glAlphaFunc" "glAreTexturesResident" "glArrayElement" "glAttachShader" "glBegin" "glBeginQuery" "glBindAttribLocation" "glBindBuffer" "glBindTexture" "glBitmap" "glBlendColor" "glBlendEquation" "glBlendEquationSeparate" "glBlendFunc" "glBlendFuncSeparate" "glBufferData" "glBufferSubData" "glCallList" "glCallLists" "glClear" "glClearAccum" "glClearColor" "glClearDepth" "glClearIndex" "glClearStencil" "glClientActiveTexture" "glClipPlane" "glColor" "glColorMask" "glColorMaterial" "glColorPointer" "glColorSubTable" "glColorTable" "glColorTableParameter" "glCompileShader" "glCompressedTexImage1D" "glCompressedTexImage2D" "glCompressedTexImage3D" "glCompressedTexSubImage1D" "glCompressedTexSubImage2D" "glCompressedTexSubImage3D" "glConvolutionFilter1D" "glConvolutionFilter2D" "glConvolutionParameter" "glCopyColorSubTable" "glCopyColorTable" "glCopyConvolutionFilter1D" "glCopyConvolutionFilter2D" "glCopyPixels" "glCopyTexImage1D" "glCopyTexImage2D" "glCopyTexSubImage1D" "glCopyTexSubImage2D" "glCopyTexSubImage3D" "glCreateProgram" "glCreateShader" "glCullFace" "glDeleteBuffers" "glDeleteLists" "glDeleteProgram" "glDeleteQueries" "glDeleteShader" "glDeleteTextures" "glDepthFunc" "glDepthMask" "glDepthRange" "glDetachShader" "glDisable" "glDisableClientState" "glDisableVertexAttribArray" "glDrawArrays" "glDrawBuffer" "glDrawBuffers" "glDrawElements" "glDrawPixels" "glDrawRangeElements" "glEdgeFlag" "glEdgeFlagPointer" "glEnable" "glEnableClientState" "glEnableVertexAttribArray" "glEnd" "glEndList" "glEndQuery" "glEvalCoord" "glEvalMesh" "glEvalPoint" "glFeedbackBuffer" "glFinish" "glFlush" "glFog" "glFogCoord" "glFogCoordPointer" "glFrontFace" "glFrustum" "glGenBuffers" "glGenLists" "glGenQueries" "glGenTextures" "glGet" "glGetActiveAttrib" "glGetActiveUniform" "glGetAttachedShaders" "glGetAttribLocation" "glGetBufferParameteriv" "glGetBufferPointerv" "glGetBufferSubData" "glGetClipPlane" "glGetColorTable" "glGetColorTableParameter" "glGetCompressedTexImage" "glGetConvolutionFilter" "glGetConvolutionParameter" "glGetError" "glGetHistogram" "glGetHistogramParameter" "glGetLight" "glGetMap" "glGetMaterial" "glGetMinmax" "glGetMinmaxParameter" "glGetPixelMap" "glGetPointerv" "glGetPolygonStipple" "glGetProgram" "glGetProgramInfoLog" "glGetQueryObject" "glGetQueryiv" "glGetSeparableFilter" "glGetShader" "glGetShaderInfoLog" "glGetShaderSource" "glGetString" "glGetTexEnv" "glGetTexGen" "glGetTexImage" "glGetTexLevelParameter" "glGetTexParameter" "glGetUniform" "glGetUniformLocation" "glGetVertexAttrib" "glGetVertexAttribPointerv" "glHint" "glHistogram" "glIndex" "glIndexMask" "glIndexPointer" "glInitNames" "glInterleavedArrays" "glIsBuffer" "glIsEnabled" "glIsList" "glIsProgram" "glIsQuery" "glIsShader" "glIsTexture" "glLight" "glLightModel" "glLineStipple" "glLineWidth" "glLinkProgram" "glListBase" "glLoadIdentity" "glLoadMatrix" "glLoadName" "glLoadTransposeMatrix" "glLogicOp" "glMap1" "glMap2" "glMapBuffer" "glMapGrid" "glMaterial" "glMatrixMode" "glMinmax" "glMultMatrix" "glMultTransposeMatrix" "glMultiDrawArrays" "glMultiDrawElements" "glMultiTexCoord" "glNewList" "glNormal" "glNormalPointer" "glOrtho" "glPassThrough" "glPixelMap" "glPixelStore" "glPixelTransfer" "glPixelZoom" "glPointParameter" "glPointSize" "glPolygonMode" "glPolygonOffset" "glPolygonStipple" "glPopAttrib" "glPopClientAttrib" "glPopMatrix" "glPopName" "glPrioritizeTextures" "glPushAttrib" "glPushClientAttrib" "glPushMatrix" "glPushName" "glRasterPos" "glReadBuffer" "glReadPixels" "glRect" "glRenderMode" "glResetHistogram" "glResetMinmax" "glRotate" "glSampleCoverage" "glScale" "glScissor" "glSecondaryColor" "glSecondaryColorPointer" "glSelectBuffer" "glSeparableFilter2D" "glShadeModel" "glShaderSource" "glStencilFunc" "glStencilFuncSeparate" "glStencilMask" "glStencilMaskSeparate" "glStencilOp" "glStencilOpSeparate" "glTexCoord" "glTexCoordPointer" "glTexEnv" "glTexGen" "glTexImage1D" "glTexImage2D" "glTexImage3D" "glTexParameter" "glTexSubImage1D" "glTexSubImage2D" "glTexSubImage3D" "glTranslate" "glUniform" "glUnmapBuffer" "glUseProgram" "glValidateProgram" "glVertex" "glVertexAttrib" "glVertexAttribPointer" "glVertexPointer" "glViewport" "glWindowPos" "glXChooseFBConfig" "glXChooseVisual" "glXCopyContext" "glXCreateContext" "glXCreateGLXPixmap" "glXCreateNewContext" "glXCreatePbuffer" "glXCreatePixmap" "glXCreateWindow" "glXDestroyContext" "glXDestroyGLXPixmap" "glXDestroyPbuffer" "glXDestroyPixmap" "glXDestroyWindow" "glXFreeContextEXT" "glXGetClientString" "glXGetConfig" "glXGetContextIDEXT" "glXGetCurrentContext" "glXGetCurrentDisplay" "glXGetCurrentDrawable" "glXGetCurrentReadDrawable" "glXGetFBConfigAttrib" "glXGetFBConfigs" "glXGetProcAddress" "glXGetSelectedEvent" "glXGetVisualFromFBConfig" "glXImportContextEXT" "glXIntro" "glXIsDirect" "glXMakeContextCurrent" "glXMakeCurrent" "glXQueryContext" "glXQueryContextInfoEXT" "glXQueryDrawable" "glXQueryExtension" "glXQueryExtensionsString" "glXQueryServerString" "glXQueryVersion" "glXSelectEvent" "glXSwapBuffers" "glXUseXFont" "glXWaitGL" "glXWaitX" "gluBeginCurve" "gluBeginPolygon" "gluBeginSurface" "gluBeginTrim" "gluBuild1DMipmapLevels" "gluBuild1DMipmaps" "gluBuild2DMipmapLevels" "gluBuild2DMipmaps" "gluBuild3DMipmapLevels" "gluBuild3DMipmaps" "gluCheckExtension" "gluCylinder" "gluDeleteNurbsRenderer" "gluDeleteQuadric" "gluDeleteTess" "gluDisk" "gluEndCurve" "gluEndPolygon" "gluEndSurface" "gluEndTrim" "gluErrorString" "gluGetNurbsProperty" "gluGetString" "gluGetTessProperty" "gluLoadSamplingMatrices" "gluLookAt" "gluNewNurbsRenderer" "gluNewQuadric" "gluNewTess" "gluNextContour" "gluNurbsCallback" "gluNurbsCallbackData" "gluNurbsCallbackDataEXT" "gluNurbsCurve" "gluNurbsProperty" "gluNurbsSurface" "gluOrtho2D" "gluPartialDisk" "gluPerspective" "gluPickMatrix" "gluProject" "gluPwlCurve" "gluQuadricCallback" "gluQuadricDrawStyle" "gluQuadricNormals" "gluQuadricOrientation" "gluQuadricTexture" "gluScaleImage" "gluSphere" "gluTessBeginContour" "gluTessBeginPolygon" "gluTessCallback" "gluTessEndContour" "gluTessEndPolygon" "gluTessNormal" "gluTessProperty" "gluTessVertex" "gluUnProject" "gluUnProject4")
  "list of all the OpenGL functions")

(defvar opengl-function-name-regexp
  (concat "\\_<" (regexp-opt opengl-function-name-list) "\\_>")
  "regexp matching all of the OpenGL functions.")

;; "\\_<\\(gl\\(?:ut?\\)?[A-Z][a-zA-Z]+[^234sifd]\\)\\([234]?[sifd]v?\\)?\\_>"
;; "\\_<\\(gl[A-Z][a-zA-Z]+\\)\\([234]?[sifd]\\)"
(defvar opengl-keywords
  `((,(concat "\\(" (substring opengl-function-name-regexp 0 -3) "\\)"
              "\\([1234]?\\(Integer\\|Boolean\\|Double\\|Float\\|[sifd]\\)?v?\\)\\_>")
     (1 font-lock-builtin-face)
     (2 font-lock-type-face))))

(defvar opengl-mode-map (make-sparse-keymap))
(define-key opengl-mode-map (kbd "C-c C-g C-g") 'describe-opengl-function)
(define-key opengl-mode-map (kbd "C-c C-g C-r") 'opengl-view-redbook)

(defun opengl-function-basename (symbol)
  "Return the base name of an opengl function, or nil if there is none.
For example, glVertex3fv -> glVertex."
  (if (not (stringp symbol))
      nil
    (let ((end 0)
          return sym)
      (while (> end (- 4 (length symbol)))
        (setq sym (substring symbol 0 (if (zerop end) nil end))
              end (- end 1))
        (when (string-match-p opengl-function-name-regexp sym)
          (setq end -999
                return sym)))
      return)))

(defun opengl-function-at-point ()
  (opengl-function-basename (thing-at-point 'symbol)))

(defun describe-opengl-function (symbol &optional arg)
  (interactive
   (list (let* ((sym (opengl-function-at-point))
                (prompt "Describe OpenGL function"))
           (completing-read
            (if sym
                (format "%s (default %s): " prompt sym)
              (concat prompt ": "))
            opengl-function-name-list nil t nil nil sym))
         current-prefix-arg))
  (browse-url (concat opengl-url symbol opengl-ext)))

(defun opengl-view-redbook ()
  "Browse `opengl-redbook-url'"
  (browse-url opengl-redbook-url))

(define-minor-mode opengl-mode
    "Toggle OpenGL minor mode.
With arg, turn OpenGL minor mode on if arg is positive, off otherwise.

OpenGL minor mode for editing OpenGL programs."
    :group 'opengl
    :lighter " GL"
    :keymap opengl-mode-map
    (if opengl-mode
      (progn
        (font-lock-add-keywords nil opengl-keywords)
        (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil opengl-keywords)
    (font-lock-fontify-buffer)))

(defun turn-on-opengl-mode () (interactive) (opengl-mode 1))

(provide 'opengl)
