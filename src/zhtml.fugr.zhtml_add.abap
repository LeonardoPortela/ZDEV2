FUNCTION zhtml_add.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TEXTO) TYPE  STRING
*"  TABLES
*"      IT_HTML STRUCTURE  W3HTML
*"----------------------------------------------------------------------

  DATA: wa_html   TYPE w3html,
        vg_linhas TYPE i,
        vg_qtdtxt TYPE i,
        vg_tabix  TYPE sy-tabix,
        p_texto   TYPE string.

  CHECK NOT i_texto IS INITIAL.

  p_texto = i_texto.

  DESCRIBE TABLE it_html LINES vg_linhas.

  vg_tabix = 0.

  IF vg_linhas GT 0.
    READ TABLE it_html INDEX vg_linhas INTO wa_html.
    CONCATENATE wa_html-line p_texto INTO p_texto.
    vg_tabix = sy-tabix.
  ENDIF.

  CALL FUNCTION 'STRING_LENGTH'
    EXPORTING
      string = p_texto
    IMPORTING
      length = vg_qtdtxt.

  IF vg_qtdtxt LE 255.
    wa_html-line = p_texto(vg_qtdtxt).
    IF vg_tabix EQ 0.
      APPEND wa_html TO it_html.
    ELSE.
      MODIFY it_html FROM wa_html INDEX vg_tabix TRANSPORTING line.
    ENDIF.
  ELSE.

    WHILE vg_qtdtxt GT 0.

      IF vg_qtdtxt LE 255.
        IF vg_tabix EQ 0.
          APPEND wa_html TO it_html.
        ELSE.
          wa_html-line = p_texto(vg_qtdtxt).
          MODIFY it_html FROM wa_html INDEX vg_tabix TRANSPORTING line.
        ENDIF.
        vg_linhas = 0.
        CLEAR: p_texto.
      ELSE.
        wa_html-line = p_texto(255).
        IF vg_tabix EQ 0.
          APPEND wa_html TO it_html.
        ELSE.
          MODIFY it_html FROM wa_html INDEX vg_tabix TRANSPORTING line.
        ENDIF.
        vg_qtdtxt = vg_qtdtxt - 255.
        p_texto = p_texto+255(vg_qtdtxt).
      ENDIF.

      CALL FUNCTION 'STRING_LENGTH'
        EXPORTING
          string = p_texto
        IMPORTING
          length = vg_qtdtxt.

      IF vg_qtdtxt GT 0.
        CLEAR wa_html-line.
        APPEND wa_html TO it_html.
        DESCRIBE TABLE it_html LINES vg_linhas.
        vg_tabix = 0.
        IF vg_linhas GT 0.
          READ TABLE it_html INDEX vg_linhas INTO wa_html.
          vg_tabix = sy-tabix.
        ENDIF.
      ENDIF.

    ENDWHILE.
  ENDIF.


ENDFUNCTION.
