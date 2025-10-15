REPORT  zmmr0003_v2.
*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: zmmt0126, j_1bbranch.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&--------------------------------------------------------------------&*
*& Declaração de Constantes                                           &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_s TYPE c VALUE 'S',
           c_0 TYPE c VALUE '0',
           c_1 TYPE c VALUE '1',
           c_x TYPE c VALUE 'X'.

*&--------------------------------------------------------------------&*
*& Declaração de Tabelas/Workareas                                    &*
*&--------------------------------------------------------------------&*
DATA: v_init     TYPE c,
      v_name(30),
      v_value,
      v_input,
      v_required,
      v_request,
      git_saida  TYPE zsds091_t.



*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: gwa_event     TYPE slis_alv_event,
      git_events    TYPE slis_t_event,
      gwa_print     TYPE slis_print_alv,
      git_estrutura TYPE TABLE OF ty_estrutura,
      lwa_estrutura TYPE ty_estrutura,
      gva_report    LIKE sy-repid,
      gva_roman_ant LIKE zmmt0008-nr_romaneio,
      gva_layout    TYPE slis_layout_alv,
      git_top       TYPE slis_t_listheader.

*&--------------------------------------------------------------------&*
*& Tela de Seleção                                                    &*
*&--------------------------------------------------------------------&*
DATA teste TYPE char8.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_um   RADIOBUTTON GROUP a1 USER-COMMAND asnum,
              p_dois RADIOBUTTON GROUP a1,
              p_tres RADIOBUTTON GROUP a1.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(50) TEXT-005 FOR FIELD p_quatro .
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_quatro RADIOBUTTON GROUP a1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_roman     TYPE zmmt0008-nr_romaneio MODIF ID bbb,
              p_safra     TYPE zsdt0001-nr_safra MODIF ID bbb OBLIGATORY DEFAULT sy-datum+0(4),
              p_werks     TYPE zmmt0008-werks MODIF ID bbb,
              p_bloco     TYPE zmmt0008-lgort MODIF ID bbb,
              p_motor(50) MODIF ID aaa,
              p_placa     TYPE zmmt0008-placa_cav  MODIF ID aaa,
              p_kunnr     TYPE kna1-kunnr MODIF ID aaa.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) TEXT-008 FOR FIELD p_local  MODIF ID ddd.
    SELECTION-SCREEN POSITION 33.
    SELECT-OPTIONS: p_local  FOR zmmt0126-kunnr NO INTERVALS NO-EXTENSION  MODIF ID ddd.
    SELECTION-SCREEN COMMENT 50(40) v_descr  MODIF ID ddd. "TEXT-008.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN.
  PERFORM f_carrega_dados_selecao.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kunnr.

INITIALIZATION.

  v_name = 'AAA'.
  v_input = '1'.

  IF p_um IS INITIAL     AND
     p_dois IS INITIAL   AND
     p_tres IS INITIAL   AND
     p_quatro IS INITIAL.

    IMPORT v_init FROM MEMORY ID 'SEL'.
    FREE MEMORY ID 'SEL'.

    IF v_init = 4.
      p_quatro = 'X'.
    ELSE.
      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.
    ENDIF.

  ENDIF.

  IF p_quatro IS NOT INITIAL.

    PERFORM f_valida_set USING v_value.

    IF v_value IS NOT INITIAL.

      v_name = 'DDD'.
      v_input = '1'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.


  CASE 'X'.
    WHEN p_um.

      v_name = 'AAA'.
      v_input = '1'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

    WHEN p_dois OR p_tres.

*      V_NAME = 'P_MOTOR'.
      v_name = 'AAA'.
      v_input = '0'.
      CLEAR: p_placa, p_motor.
      PERFORM f_modificar_tela USING v_input
                                v_required
                                v_request.

      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

    WHEN  p_quatro.

      v_name = 'AAA'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      v_name = 'BBB'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      PERFORM f_valida_set USING v_value.

      IF v_value IS NOT INITIAL.

        v_name = 'DDD'.
        v_input = '1'.
        PERFORM f_modificar_tela USING v_input
                                 v_required
                                 v_request.

      ELSE.

        v_name = 'DDD'.
        v_input = '0'.
        PERFORM f_modificar_tela USING v_input
                                 v_required
                                 v_request.

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  IF p_local IS NOT INITIAL.

    CLEAR v_descr.
    SELECT SINGLE name1 FROM kna1
      INTO v_descr
      WHERE kunnr IN p_local.

  ENDIF.

FORM f_modificar_tela USING input
                            required
                            request  .

  LOOP AT SCREEN.

    IF screen-group1 = v_name.
      screen-active   = input.                              "'0'.
      screen-required = required.                           "'0'.
      screen-request  = request.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_MODIFICAR_TELA
*&--------------------------------------------------------------------&*
*& Start-Of-Selection                                                 &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.


  CASE abap_true.
    WHEN p_um.     "Gerar Lotes Vendas

      zcl_comercializacao_algodao=>processar_espelho_romaneio(
        EXPORTING
          i_werks             = CONV #( p_werks )
          i_lgort             = CONV #( p_bloco )
          i_safra             = CONV #( p_safra )
          i_nr_romaneio       = CONV #( p_roman )
          i_motorista         = CONV #( p_motor )
          i_placa             = CONV #( p_placa )
          i_kunnr             = CONV #( p_kunnr )
         IMPORTING
           e_msg_error         = DATA(_msg_error)
           e_fardos_processar  = DATA(_lit_fardos_processar) ).


      IF _msg_error IS NOT INITIAL.
        MESSAGE _msg_error TYPE 'I'.
        STOP.
      ENDIF.

      MOVE-CORRESPONDING _lit_fardos_processar[] TO git_saida[].

      PERFORM f_iniciar_variaves.
      PERFORM f_imprime_dados.

    WHEN p_dois.   "Impressao Relatorio

      _msg_error = zcl_comercializacao_algodao=>imprimir_espelho_romaneio(  i_werks       = CONV #( p_werks )
                                                                            i_lgort       = CONV #( p_bloco )
                                                                            i_safra       = CONV #( p_safra )
                                                                            i_nr_romaneio = CONV #( p_roman ) ).


      IF _msg_error IS NOT INITIAL.
        MESSAGE _msg_error TYPE 'I'.
        STOP.
      ENDIF.

    WHEN p_tres.   "Cancelamento

      _msg_error = zcl_comercializacao_algodao=>cancelar_espelho_romaneio(  i_werks       = CONV #( p_werks )
                                                                            i_lgort       = CONV #( p_bloco )
                                                                            i_safra       = CONV #( p_safra )
                                                                            i_nr_romaneio = CONV #( p_roman ) ).

      IF _msg_error IS NOT INITIAL.
        MESSAGE _msg_error TYPE 'I'.
        STOP.
      ENDIF.

    WHEN p_quatro. "Cadastrar E-Mail Parceiro "Local Entrega"

      zcl_comercializacao_algodao=>cadastro_email_espelho_rom( i_local_entrega = p_local-low ).

  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_DADOS_SELECAO.
*&---------------------------------------------------------------------*
FORM f_carrega_dados_selecao.

  CHECK sy-ucomm <> 'ONLI'.

  IF p_placa IS INITIAL OR ( p_roman <> gva_roman_ant ).
    SELECT vbeln, id_cli_dest, placa_cav, motorista
      INTO TABLE @DATA(t_0001)
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = @p_roman
       AND nr_safra    = @p_safra
       AND branch      = @p_werks.

    IF sy-subrc = 0.
      READ TABLE t_0001 INTO DATA(w_0001) INDEX 1.
      IF sy-subrc = 0.
        p_placa = w_0001-placa_cav.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_motor IS INITIAL OR ( p_roman <> gva_roman_ant ).
    SELECT vbeln id_cli_dest placa_cav motorista
      INTO TABLE t_0001
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = p_roman
       AND nr_safra    = p_safra
       AND branch      = p_werks.

    IF sy-subrc = 0.
      CLEAR w_0001.
      READ TABLE t_0001 INTO w_0001 INDEX 1.
      SELECT SINGLE name1
        INTO @DATA(l_name1)
        FROM lfa1
       WHERE lifnr = @w_0001-motorista.
      IF sy-subrc = 0.
        p_motor = l_name1.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_kunnr IS INITIAL OR ( p_roman <> gva_roman_ant ).
    SELECT vbeln id_cli_dest placa_cav motorista
      INTO TABLE t_0001
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = p_roman
       AND nr_safra    = p_safra
       AND branch      = p_werks.

    IF sy-subrc = 0.
      LOOP AT t_0001 INTO w_0001.
        SELECT SINGLE auart
          INTO @DATA(l_auart)
          FROM vbak
         WHERE vbeln = @w_0001-vbeln.
        IF sy-subrc = 0 AND l_auart <> 'ZRFL'.
          p_kunnr = w_0001-id_cli_dest.
          EXIT.
        ELSE.
          CLEAR p_kunnr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  gva_roman_ant = p_roman.

ENDFORM.


FORM f_imprime_dados.

  gva_layout-box_fieldname = 'MARK'. " fieldname for checkbox
  gva_layout-box_tabname   = 'GIT_SAIDA'." tabname for checkbox

  PERFORM f_definir_eventos.
  PERFORM f_montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gva_report
      i_callback_user_command  = 'XUSER_COMMAND'
      i_callback_pf_status_set = 'XPF_STATUS'
      it_fieldcat              = git_estrutura[]
      i_save                   = 'A'
      is_layout                = gva_layout
      it_events                = GIT_events
      is_print                 = GWA_print
    TABLES
      t_outtab                 = git_saida.

ENDFORM.                    " IMPRIMIR_DADOS

FORM f_definir_eventos.
  PERFORM f_carregar_eventos USING:
                                 slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS

FORM f_carregar_eventos USING    name form.
  CLEAR gwa_event.
  gwa_event-name = name.
  gwa_event-form = form.
  APPEND gwa_event TO GIT_events.
ENDFORM.                    " f_carregar_eventos

FORM xpf_status USING o_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.

ENDFORM.                    " XPF_STATUS

FORM xuser_command USING ucomm LIKE sy-ucomm
                         selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN '&PROC'.
      PERFORM f_gerar_espelho_romaneio.

  ENDCASE.
ENDFORM.                    " XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_montar_layout.
  PERFORM f_montar_estrutura USING:
        1 'ZMMT0008'  'LGORT' 'TG_SAIDA' 'BLOCO'  ' '  ' ' ,
        2 'ZMMT0008'  'CHARG' 'TG_SAIDA' 'FARDO'  ' '  ' ' ,
        3 'VBRP'      'MATNR' 'TG_SAIDA' 'MATNR'  ' '  ' ' ,
        4 'ZMMT0008'  'MENGE' 'TG_SAIDA' 'MENGE'  ' '  ' ' .


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR lwa_estrutura.
  lwa_estrutura-fieldname     = p_field.
  lwa_estrutura-tabname       = p_tabname.
  lwa_estrutura-ref_tabname   = p_ref_tabname.
  lwa_estrutura-ref_fieldname = p_ref_fieldname.
  lwa_estrutura-key           = ' '.
  lwa_estrutura-key_sel       = 'X'.
  lwa_estrutura-col_pos       = p_col_pos.
  lwa_estrutura-no_out        = ' '.
  lwa_estrutura-seltext_s     = p_scrtext_l.
  lwa_estrutura-seltext_m     = p_scrtext_l.
  lwa_estrutura-seltext_l     = p_scrtext_l.

  APPEND lwa_estrutura TO git_estrutura.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = GIt_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  gva_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'H' space TEXT-002.
  PERFORM f_construir_cabecalho USING 'S' 'Romaneio:' p_roman.

  SELECT SINGLE *
    FROM j_1bbranch
   WHERE branch EQ p_werks.

  PERFORM f_construir_cabecalho USING 'S' 'Remetente:' j_1bbranch-name.

  READ TABLE git_saida INTO DATA(lwa_saida_tmp) INDEX 1.
  PERFORM f_construir_cabecalho USING 'S' 'Destinatario:' lwa_saida_tmp-name1_destino.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ key text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-key = key.
  ls_line-info = text.
  APPEND ls_line TO GIt_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ZMMT0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_espelho_romaneio .

  DATA: lit_fardos_espelho TYPE zsds091_t.

  DATA: lit_rows     TYPE lvc_t_row,
        lva_resposta TYPE c,
        lva_msg(150) TYPE c.

  CLEAR: lit_fardos_espelho[].


  LOOP AT git_saida INTO DATA(lwa_saida)   WHERE mark  = c_x.
    APPEND lwa_saida TO lit_fardos_espelho.
  ENDLOOP.

  zcl_comercializacao_algodao=>gerar_espelho_romaneio(
    EXPORTING
      i_werks             = CONV #( p_werks )
      i_lgort             = CONV #( p_bloco )
      i_safra             = CONV #( p_safra )
      i_nr_romaneio       = CONV #( p_roman )
      i_motorista         = CONV #( p_motor )
      i_placa             = CONV #( p_placa )
      i_kunnr             = CONV #( p_kunnr )
      i_fardos_espelho    = lit_fardos_espelho
    IMPORTING
      e_msg_error         = DATA(lva_msg_error)
      e_msg_sucesso       = DATA(lva_msg_sucesso) ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'S' DISPLAY LIKE 'E'.
  ELSEIF lva_msg_sucesso IS NOT INITIAL.
    MESSAGE lva_msg_sucesso TYPE 'S' .
  ENDIF.

ENDFORM.                    " ATUALIZA_ZMMT0008


FORM f_valida_set USING p_value.

  DATA: t_set  TYPE TABLE OF rgsb4,
        wa_set TYPE rgsb4.
  DATA: v_check TYPE c VALUE ''.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'ZMM0027_USER'
      table         = 'ZMMT0005'
      class         = '0000'
      fieldname     = 'USNAM'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  READ TABLE t_set INTO DATA(w_set) WITH KEY from = sy-uname.
  IF sy-subrc IS INITIAL.
    p_value = 'X'.
  ENDIF.

ENDFORM.
