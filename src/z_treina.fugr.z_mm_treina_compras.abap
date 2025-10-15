FUNCTION z_mm_treina_compras.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(I_EBELN) TYPE  EBELN OPTIONAL
*"     REFERENCE(I_EBELP) TYPE  EBELP
*"     REFERENCE(I_KOSTL) TYPE  KOSTL OPTIONAL
*"     REFERENCE(I_SAKNR) TYPE  SAKNR OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_BTN) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(I_TIPO) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(I_ERRO) TYPE  CHAR1
*"----------------------------------------------------------------------
  TYPES: ty_rg_bukrs  TYPE RANGE OF zglt081-bukrs.

  DATA: tabix       TYPE sy-tabix.


  p_lifnr = i_lifnr.
  p_ebeln = i_ebeln.
  p_ebelp = i_ebelp.
  p_saknr = i_saknr.
  p_matnr = i_matnr.
  p_btn   = i_btn.
  p_tipo  = i_tipo.

  CLEAR: w_answer, i_erro.

  IF ( i_kostl IS NOT INITIAL ).

    "Criar range das empresas que não entram na regra do treinamento:
    DATA(rg_bukrs_exc) = VALUE ty_rg_bukrs(
        ( sign = 'I' option = 'EQ' low = '0035' )
        ( sign = 'I' option = 'EQ' low = '0038' )
        ( sign = 'I' option = 'EQ' low = '0039' )
        ( sign = 'I' option = 'EQ' low = '0043' )   ).

    SELECT SINGLE * FROM csks INTO @DATA(lwa_csks)
      WHERE kostl = @i_kostl
        AND datbi >= @sy-datum.
    IF ( sy-subrc = 0 ) AND ( lwa_csks-bukrs IN rg_bukrs_exc[] ).
      EXIT.
    ENDIF.

  ENDIF.

  SELECT SINGLE bukrs INTO @DATA(lva_bukrs_pedido)
    FROM ekko WHERE ebeln = @p_ebeln.
  IF ( sy-subrc = 0 ).
    IF ( line_exists( rg_bukrs_exc[ low = lva_bukrs_pedido ] ) ).
      EXIT.
    ENDIF.
  ENDIF.


  IF i_btn = 'X'. "and 1 = 2.

    SELECT SINGLE *
      FROM zmmt0103
      INTO @DATA(wa_zmmt0103)
      WHERE saknr = @p_saknr
      OR    matnr = @p_matnr.

    CHECK sy-subrc = 0.

    IF wa_zmmt0103-fg_atencao = 'X'.
      SELECT SINGLE *
        FROM zmmt0105
        INTO  @DATA(w105)
          WHERE ebeln = @i_ebeln
          AND   ebelp = @i_ebelp
          AND   lifnr = @i_lifnr
          AND   tipo  = @i_tipo.
      IF sy-subrc NE 0.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Esse lançamento refere-se a treinamento ?'
            text_button_1         = 'Sim'(100)
            icon_button_1         = 'ICON_OKAY '
            text_button_2         = 'Não'(101)
            icon_button_2         = 'ICON_CANCEL'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 25
            start_row             = 6
          IMPORTING
            answer                = w_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF w_answer = '2'. "não
          REFRESH tg_treina.
          PERFORM f_grava.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT  *
    FROM zmmt0104 "
    INTO CORRESPONDING FIELDS OF TABLE tg_treina.
    "WHERE C_CUSTO = I_KOSTL. Removido filtro de centro de custo 03/09/2020 - IR037908

    SELECT *
      FROM zmmt0105
      INTO TABLE @DATA(it_zmmt0105)
        WHERE ebeln = @i_ebeln
        AND   ebelp = @i_ebelp
        AND   lifnr = @i_lifnr
        AND   tipo  = @i_tipo.

    SORT it_zmmt0105 BY ebeln lifnr cpf id_lms tipo usuario data.

    LOOP AT tg_treina.
      tabix = sy-tabix.
      IF i_ebeln IS NOT INITIAL.
        READ TABLE it_zmmt0105 INTO DATA(wa_treina) WITH KEY ebeln   = i_ebeln
                                                             lifnr   = i_lifnr
                                                             cpf     = tg_treina-cpf
                                                             id_lms  = tg_treina-id_lms
                                                             tipo    = i_tipo
                                                             BINARY SEARCH.
      ELSE.
        READ TABLE it_zmmt0105 INTO DATA(wa_treina2) WITH KEY ebeln   = i_ebeln
                                                              lifnr   = i_lifnr
                                                              cpf     = tg_treina-cpf
                                                              id_lms  = tg_treina-id_lms
                                                              tipo    = i_tipo
                                                              usuario = sy-uname
                                                              data    = sy-datum
                                                              BINARY SEARCH.
      ENDIF.
      IF sy-subrc = 0.
        tg_treina-icon = icon_checked.
        MODIFY tg_treina INDEX tabix TRANSPORTING icon.
      ENDIF.
    ENDLOOP.
    CALL SCREEN 100 ENDING AT 140 23 STARTING AT 20 3.
  ELSE. "Grava pedido
    SELECT  *
       FROM zmmt0105
       INTO CORRESPONDING FIELDS OF TABLE tg_treina
       WHERE ebeln   = ' '
       AND   lifnr   = i_lifnr
       AND   data    = sy-datum
       AND   usuario = sy-uname
       AND   tipo    = i_tipo.

    IF tg_treina[] IS NOT INITIAL.
      PERFORM f_grava.
    ENDIF.
  ENDIF.



ENDFUNCTION.
