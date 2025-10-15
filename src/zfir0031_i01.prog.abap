*----------------------------------------------------------------------*
***INCLUDE ZFIR0031_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: w_answer,
        w_msg(50),
        wl_zfit0045 TYPE zfit0045.

  CASE ok-code.
    WHEN 'LIB_INS'.
      PERFORM f_libera_ins.
    WHEN 'SELDROP'.
      IF wg_cadlan-orig_pgt = 'E'.
        vg_habilitar = 'S'.
        IF wg_cadlan-form_pgt = 'T'.
          vg_habilitar_e = 'S'.
        ELSE.
          vg_habilitar_e = 'N'.
          CLEAR:  wg_cadlan-hbkid_e.
        ENDIF.
      ELSE.
        vg_habilitar = 'N'.
        CLEAR: wg_cadlan-form_pgt, wg_cadlan-hbkid_e.
      ENDIF.

*      IF WG_CADLAN-ORIG_PGT EQ 'E'.
*        PERFORM F_TRATA_CAMPOS USING  SPACE
*                                  'GR4'
*                                  C_0       "INPUT 1     NO INPUT 0
*                                  C_0.      "INVISIBLE 1 VISIBLE 0
*
*

*     ZCL_INT_SE=>CRIAR_WORKFLOW_SOFTEXPERT( ).
*      ZCL_INT_SE=>CREATE_WORKFLOW_SOFTEXPERT_SFC( ).

    WHEN 'ANEXAR'.
      PERFORM habilitar_workflow_documentos.
    WHEN 'PICK'.
      GET CURSOR FIELD cursorfield LINE cursorline VALUE cursorvalue.
      IF  NOT cursorvalue IS INITIAL AND cursorfield = 'WG_CADLAN-EBELN'.
        SET PARAMETER ID 'BES' FIELD cursorvalue.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'BLODOC'.
      CHECK wg_cadlan-nro_sol IS NOT INITIAL.

      SELECT  SINGLE *
      FROM zfit0045
      INTO wl_zfit0045
       WHERE nro_sol EQ wg_cadlan-nro_sol.

      IF wl_zfit0045-loekz = 'X'.
        MESSAGE s836(sd) WITH 'O documento está eliminado, impossível operação!'.
        EXIT.
      ELSEIF wl_zfit0045-status = 'B'.
        w_msg = 'Confirma Desbloqueio do lançamento?'.
      ELSE.
        w_msg = 'Confirma Bloqueio do lançamento?'.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
          text_question         = w_msg
          text_button_1         = 'Sim'(001)
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Não'(002)
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
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF w_answer = '1'.
        PERFORM f_bloqueio_lancamento.
      ENDIF.
    WHEN c_deldoc.
      CHECK wg_cadlan-nro_sol IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
          text_question         = 'Confirma a exclusão do lançamento?'
          text_button_1         = 'Sim'(001)
          icon_button_1         = 'ICON_OKAY'
          text_button_2         = 'Não'(002)
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
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF w_answer = '1'.
        PERFORM f_eliminar_lancamento.
      ENDIF.
    WHEN c_search.
      PERFORM f_busca_dados.
    WHEN c_displa.
      wg_acao = c_displa.
      PERFORM f_limpa_campos.
      REFRESH: tg_fields.
      PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM f_trata_campos USING  space
                                      'GR3'
                                       c_0       "INPUT 1     NO INPUT 0
                                       c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

*      PERFORM F_TRATA_CAMPOS USING  SPACE
*                                      'GR4'
*                                      C_0       "INPUT 1     NO INPUT 0
*                                      C_0.      "INVISIBLE 1 VISIBLE 0


      "IF GRID1->IS_READY_FOR_INPUT( ) EQ 1.
      CALL METHOD grid1->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
      "ENDIF.

    WHEN c_save.
      IF wg_cadlan-belnr IS NOT INITIAL.
        MESSAGE 'Lançamento não pode ser alterado, documento contábil gerado' TYPE 'I'.
        EXIT.
      ENDIF.
      CALL METHOD grid1->check_changed_data.
      PERFORM f_verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
*        IF WG_CADLAN-NRO_SOL IS INITIAL.
*          PERFORM  F_OBTEM_PROXIMO.
*        ENDIF.
        PERFORM f_grava_dados.

        REFRESH tg_fields.
        PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM f_trata_campos USING  space
                                      'GR3'
                                       c_0       "INPUT 1     NO INPUT 0
                                       c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

*        PERFORM F_TRATA_CAMPOS USING  SPACE
*                                      'GR4'
*                                      C_0       "INPUT 1     NO INPUT 0
*                                      C_0.      "INVISIBLE 1 VISIBLE 0

        IF grid1->is_ready_for_input( ) EQ 1.
          CALL METHOD grid1->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.
        ENDIF.

      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Verificar erro(s) no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = space   "c_x
            i_repid       = sy-repid
            i_pressed_tab = ' '
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.


    WHEN c_add.
      CHECK wg_acao <> c_add.

      wg_acao = c_add.  "c_modif.

      PERFORM:  f_limpa_campos.

      IF wg_cadlan-nro_sol IS INITIAL. "Alterado a posição de criar a sequencia.
        PERFORM  f_obtem_proximo.
      ENDIF.

      REFRESH: tg_fields.
      PERFORM f_trata_campos USING  space
                                    'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM f_trata_campos USING  space
                                    'GR3'
                                     c_1       "INPUT 1     NO INPUT 0
                                     c_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM f_trata_campos USING  space
                                    'GR1'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0

*      PERFORM F_TRATA_CAMPOS USING  SPACE
*                                      'GR4'
*                                      C_0       "INPUT 1     NO INPUT 0
*                                      C_0.      "INVISIBLE 1 VISIBLE 0
      IF grid1->is_ready_for_input( ) EQ 0.
        CALL METHOD grid1->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      ENDIF.


    WHEN c_atuali.

    WHEN c_modif.
      IF wg_acao = c_modif.
        CLEAR wg_acao.
        REFRESH: tg_fields.
        PERFORM f_trata_campos USING  space
                                      'GR2'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM f_trata_campos USING  space
                                      'GR3'
                                       c_0       "INPUT 1     NO INPUT 0
                                       c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0

*        PERFORM F_TRATA_CAMPOS USING  SPACE
*                                      'GR4'
*                                      C_0       "INPUT 1     NO INPUT 0
*                                      C_0.      "INVISIBLE 1 VISIBLE 0
        IF grid1->is_ready_for_input( ) EQ 1.
          CALL METHOD grid1->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.
        ENDIF.
      ELSE.
        IF wg_cadlan-belnr IS NOT INITIAL.
          MESSAGE 'Lançamento não pode ser alterado, documento contábil gerado' TYPE 'I'.
          EXIT.
        ENDIF.
        IF wg_cadlan-descricao IS INITIAL.
          MESSAGE 'Preencha o Depto Responsável para permissão de modificação' TYPE 'I'.
          EXIT.
        ENDIF.
        wg_acao = c_modif.
        IF wg_cadlan-status = 'A'.
          MESSAGE 'Documento já aprovado, permitida a alteração somente de data' TYPE 'I'.
          PERFORM f_trata_campos USING  space
                             'GR2'
                             c_0       "INPUT 1     NO INPUT 0
                             c_0.      "INVISIBLE 1 VISIBLE 0
          IF grid1->is_ready_for_input( ) EQ 1.
            CALL METHOD grid1->set_ready_for_input
              EXPORTING
                i_ready_for_input = 0.
          ENDIF.

        ELSE.
          PERFORM f_trata_campos USING  space
                              'GR2'
                              c_1       "INPUT 1     NO INPUT 0
                              c_0.      "INVISIBLE 1 VISIBLE 0
          IF grid1->is_ready_for_input( ) EQ 0.
            CALL METHOD grid1->set_ready_for_input
              EXPORTING
                i_ready_for_input = 1.
          ENDIF.

        ENDIF.
        PERFORM f_trata_campos USING  space
                             'GR3'
                              c_1       "INPUT 1     NO INPUT 0
                              c_0.      "INVISIBLE 1 VISIBLE 0

        PERFORM f_trata_campos USING  space
                                      'GR1'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0


      ENDIF.

      CLEAR: ok-code.
    WHEN c_show_msgre.
      PERFORM f_verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_popup       = 0
            i_pressed_tab = ' '
            i_set_field   = 'X_FIELD'
            i_set_cell    = 'WG_CELL'
            i_set_obj     = 'WG_OBJ'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.
    WHEN c_cancel.
      CLEAR wg_acao.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_depto INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_dep OCCURS 0,
          dep_resp TYPE zfit0045-dep_resp,
          text1    TYPE t012t-text1,
        END OF tl_dep.
  REFRESH tl_dep.
  CLEAR tl_dep.

  SELECT dep_resp dep_resp_desc
    FROM zimp_cad_depto
    INTO TABLE tl_dep.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DEP_RESP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0045-DEP_RESP'
      value_org       = 'S'
    TABLES
      value_tab       = tl_dep
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_APROV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_aprov INPUT.
  DATA: tl_return_tab2 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc2      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_usr OCCURS 0,
          bname     TYPE v_usr_name-bname,
          name_text TYPE v_usr_name-name_text,
        END OF tl_usr.

  SELECT bname  name_text
     FROM  v_usr_name INTO TABLE tl_usr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BNAME'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0045-RESP_NEG'
      value_org       = 'S'
    TABLES
      value_tab       = tl_usr
      return_tab      = tl_return_tab2
      dynpfld_mapping = tl_dselc2.
ENDMODULE.                 " SEARCH_APROV  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_ADTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_adto INPUT.
  DATA: tl_return_tab3 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc3      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_adt OCCURS 0,
          nro_sol     TYPE zfit0045-nro_sol,
          ebeln       TYPE zfit0045-ebeln,
          lifnr       TYPE zfit0045-lifnr,
          dt_pgto     TYPE zfit0045-dt_pgto,
          dt_prev_liq TYPE zfit0045-dt_prev_liq,
          moeda_pgto  TYPE zfit0045-moeda_pgto,
          motivo      TYPE zfit0045-motivo,
          usnam       TYPE zfit0045-usnam,
          belnr       TYPE zfit0045-belnr,
          status      TYPE zfit0045-status,
        END OF tl_adt.

  SELECT  nro_sol ebeln lifnr dt_pgto  dt_prev_liq  moeda_pgto  motivo  usnam  belnr  status
     FROM  zfit0045 INTO TABLE tl_adt.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NRO_SOL'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0045-NRO_SOL'
      value_org       = 'S'
    TABLES
      value_tab       = tl_adt
      return_tab      = tl_return_tab3
      dynpfld_mapping = tl_dselc3.
ENDMODULE.                 " SEARCH_ADTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FORMA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_forma INPUT.
  DATA: tl_return_tab4 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc4      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_forma OCCURS 0,
          zlsch TYPE t042z-zlsch,
          text1 TYPE t042z-text1,
        END OF tl_forma.

  SELECT  zlsch text1
     FROM  t042z INTO TABLE tl_forma
    WHERE land1 = 'BR'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZLSCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIT0045-ZLSCH'
      value_org       = 'S'
    TABLES
      value_tab       = tl_forma
      return_tab      = tl_return_tab4
      dynpfld_mapping = tl_dselc4.
ENDMODULE.                 " SEARCH_FORMA  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_CONTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_conta INPUT.
  DATA: tl_return_tab5 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc5      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_conta OCCURS 0,
          bvtyp TYPE bvtyp,
          banks TYPE banks,
          bankl TYPE bankl,
          bankn TYPE bankn,
        END OF tl_conta.

  DATA: l_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  REFRESH l_dynpfields.
  CLEAR   l_dynpfields.
  CLEAR vg_check_banco.

  l_dynpfields-fieldname  = 'WG_CADLAN-TP_OPER'.
  APPEND l_dynpfields.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = l_dynpfields.
  READ TABLE l_dynpfields INDEX 1.
  MOVE l_dynpfields-fieldvalue TO wg_cadlan-tp_oper.

  REFRESH l_dynpfields.
  IF wg_cadlan-orig_pgt EQ 'E'  AND
     wg_cadlan-form_pgt EQ 'T'  AND
     wg_cadlan-tp_oper  EQ '01' AND
    ( wg_acao EQ 'ADD'  OR wg_acao EQ'MODIF' ).
*    REFRESH: tg_conta.
    CALL SCREEN 0120 STARTING AT 1 1.
  ELSE.
    IF wg_cadlan-bukrs IS  INITIAL.
      l_dynpfields-fieldname  = 'WG_CADLAN-LIFNR'.
      APPEND l_dynpfields.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = l_dynpfields.
      READ TABLE l_dynpfields INDEX 1.
      MOVE l_dynpfields-fieldvalue TO wg_cadlan-lifnr.
    ENDIF.

    IF wg_cadlan-lifnr IS NOT INITIAL.
      SELECT  bvtyp banks bankl bankn
         FROM  lfbk INTO TABLE tl_conta
        WHERE lifnr = wg_cadlan-lifnr.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BVTYP'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'ZFIT0045-BVTYP'
        value_org       = 'S'
      TABLES
        value_tab       = tl_conta
        return_tab      = tl_return_tab5
        dynpfld_mapping = tl_dselc5.

  ENDIF.

ENDMODULE.                 " SEARCH_CONTA  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0120 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL' OR 'ECAN'.

    WHEN 'CONFIRM'.
      IF vg_check_banco EQ 'E'.
        REFRESH tg_conta.
      ENDIF.
      DATA(_lines) = lines( tg_conta ).
      IF tg_conta[] IS NOT INITIAL.
        READ TABLE tg_conta INTO DATA(wa_conta) INDEX 1.
        IF sy-subrc EQ 0.
          IF _lines GT 1.
            READ TABLE tg_conta INTO DATA(wa_conta2) INDEX 2.
            IF wa_conta-bvtyp = wa_conta2-bvtyp.
              MESSAGE 'Este Banco é o intermediário!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.
          wg_cadlan-bvtyp = wa_conta-bvtyp.
          vg_check_banco = 'X'.
          wg_acao = c_modif.
        ENDIF.
      ENDIF.
  ENDCASE.

  SET SCREEN 0.

ENDMODULE.
