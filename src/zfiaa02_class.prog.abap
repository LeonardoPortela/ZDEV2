

*----------------------------------------------------------------------*
*       CLASS ZUTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zutils DEFINITION.
  PUBLIC SECTION.
    METHODS tratar_campos IMPORTING
                            group1    TYPE char3
                            group2    TYPE char3
                            value     TYPE char1
                            invisible TYPE char1.

    METHODS checar_dia_util IMPORTING
                              i_data TYPE erdat
                            EXPORTING
                              e_data TYPE erdat.

    METHODS z_style_disable_edit IMPORTING
                                   fieldname TYPE any
                                   style     TYPE any.

    METHODS buscar_status_zib IMPORTING
                                i_doc_imposto TYPE zdoc_imposto
                                i_bukrs       TYPE bukrs
                                i_gjahr       TYPE gjahr
                              EXPORTING
                                e_zibchv      TYPE zib_contabil_chv
                                e_ziberr      TYPE zib_contabil_err.

    DATA: at_objkey TYPE awkey.


    METHODS show_splitter_error IMPORTING
                                  i_show  TYPE c
                                  i_popup TYPE i.


    METHODS criar_mensagem_erro IMPORTING
                                  text1 TYPE bapi_msg
                                  text2 TYPE itex132
                                  field TYPE char30
                                  index TYPE sy-tabix.

    METHODS validar_screen_0110.
    METHODS validar_screen_0140.

    METHODS criar_mathcode.
ENDCLASS.                    "ZUTILS DEFINITION

*----------------------------------------------------------------------*
*       CLASS ZUTILS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS zutils IMPLEMENTATION.
  METHOD tratar_campos.

    wl_fields-group1    = group1.
    wl_fields-group2    = group2.
    wl_fields-value     = value.
    wl_fields-invisible = invisible.

    APPEND wl_fields TO gt_fields.
  ENDMETHOD.                    "Z_TRATAR_CAMPOS

  METHOD checar_dia_util.
    DATA: gt_holidays TYPE TABLE OF iscal_day,
          wl_holidays TYPE iscal_day,
          at_day.

    e_data      = i_data.
*    E_DATA+6(2) = 10.

    DO 3 TIMES.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          holiday_calendar = 'BR'
          date_from        = sy-datum
          date_to          = e_data
        TABLES
          holidays         = gt_holidays.

      READ TABLE gt_holidays INTO wl_holidays WITH KEY date = e_data.
      IF ( sy-subrc IS INITIAL ).
        e_data = e_data + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = e_data
      IMPORTING
        day  = at_day.

    IF ( at_day = 6 ).
      e_data = e_data + 2.

    ELSEIF at_day = 7.
      e_data = e_data + 1.
    ENDIF.
  ENDMETHOD.                    "CHECAR_DIA_UTIL

  METHOD z_style_disable_edit.

    wl_estilo-fieldname = fieldname.
    wl_estilo-style     = style.

    APPEND wl_estilo TO gt_estilo.
  ENDMETHOD.                    "Z_STYLE_DISABLE_EDIT

  METHOD buscar_status_zib.
    CLEAR: at_objkey, e_zibchv, e_ziberr.

    CONCATENATE 'ZP' i_bukrs i_doc_imposto i_gjahr
    INTO at_objkey.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO e_zibchv
     WHERE obj_key EQ at_objkey.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO e_ziberr
       WHERE obj_key EQ at_objkey
         AND nr_item EQ '1'.
    ENDIF.

  ENDMETHOD.                    "buscar_status_zib

  METHOD show_splitter_error.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = i_show
        i_popup    = i_popup
        i_repid    = sy-repid
        i_set_cell = 'WL_CELL'
        i_set_obj  = 'WL_OBJ'
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.
  ENDMETHOD.                    "Z_SHOW_SPLITTER

  METHOD criar_mensagem_erro.

    wl_msg_return-field = field.
    wl_msg_return-tabix = index.
    wl_msg_return-aba   = screen_principal.

    CONCATENATE icon_message_error_small text1 text2
           INTO wl_msg_return-msg SEPARATED BY space.

    APPEND wl_msg_return TO gt_msg_return.
    CLEAR wl_msg_return.
  ENDMETHOD.                    "Z_CRIAR_MENSAGEM_ERRO

  METHOD validar_screen_0110.
    SELECT SINGLE *
      FROM j_1bbranch
      INTO wl_j_1bbranch
     WHERE bukrs  = s_bukrs
       AND branch = s_werks.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) WITH TEXT-e05 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "VALIDAR_SCREEN_0110

  METHOD validar_screen_0140.
    DATA: msg_erro    TYPE bapi_msg,
          imobilizado TYPE anln1,
          v_index     TYPE sy-tabix.

    LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = x.
      v_index = sy-tabix.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_saida_0140-anln1
        IMPORTING
          output = imobilizado.

      IF wl_saida_0140-ano_vcto IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e09 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'ANO_VCTO'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-dt_venc IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e10 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'DT_VENC'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-dep_resp IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e11 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'DEP_RESP'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-waers IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e12 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'WAERS'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-vlr_total IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e08 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'VLR_TOTAL'
                             index = v_index ).
      ENDIF.

*    IF WL_SAIDA_0140-VLR_PRINC IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Principal' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_CORRE IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Correção' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_MULTA IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Multa' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_JUROS IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Juros' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_TSE IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr TSE' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-COD_BARRAS IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Cód barras' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_TOTAL IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Cód barras' ).
*    ENDIF.

    ENDLOOP.

    CHECK gt_msg_return IS NOT INITIAL.
    show_splitter_error( i_show  =  x
                         i_popup =  0 ).
*    X = SPACE.

  ENDMETHOD.                    "VALIDAR_SCREEN_0140

  METHOD criar_mathcode.

    TYPES: BEGIN OF ty_mathcode,
             kfzkz TYPE anlz-kfzkz,
           END OF ty_mathcode.

    DATA: gt_t005u    TYPE TABLE OF t005u,
          wl_t005u    TYPE t005u,
          gt_mathcode TYPE TABLE OF ty_mathcode,
          wl_mathcode TYPE ty_mathcode,
          it_return   TYPE STANDARD TABLE OF ddshretval.

    SELECT *
      FROM t005u
      INTO TABLE gt_t005u
     WHERE spras EQ 'PT'
       AND land1 EQ 'BR'.

    LOOP AT gt_t005u INTO wl_t005u.
      MOVE wl_t005u-bland TO wl_mathcode-kfzkz.
      APPEND wl_mathcode  TO gt_mathcode.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*       DDIC_STRUCTURE  = 'EKKO'
        retfield        = 'KFZKZ'
*       PVALKEY         = ' '
*       DYNPPROG        = sy-repid
*       DYNPNR          = sy-dynnr
*       DYNPROFIELD     = 'EBELN'
*       STEPL           = 0
        window_title    = 'VKBUR Records'
*       VALUE           = ' '
        value_org       = 'S'
*       MULTIPLE_CHOICE = 'X'  "allows you select multiple entries from the popup
*       DISPLAY         = ' '
*       CALLBACK_PROGRAM       = ' '
*       CALLBACK_FORM   = ' '
*       MARK_TAB        =
* IMPORTING
*       USER_RESET      = ld_ret
      TABLES
        value_tab       = gt_mathcode
*       FIELD_TAB       = lt_field
        return_tab      = it_return
*       DYNPFLD_MAPPING =
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ENDMETHOD.                    "CRIAR_MATHCODE
ENDCLASS.                    "ZUTILS IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS GERAR_CONTAS_PAGAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gerar_contas_pagar DEFINITION.
  PUBLIC SECTION.

    METHODS seleciona_imposto IMPORTING
                                i_cod_imposto TYPE zcod_imposto.


    METHODS gera_lote IMPORTING
                        i_bukrs    TYPE bukrs
                        i_descr    TYPE char100
                        i_depto    TYPE char2
                        i_venci    TYPE erdat
                      EXPORTING
                        e_num_lote TYPE numc10
                        e_dt_venci TYPE erdat.

    METHODS gera_doc_imposto
      IMPORTING
        i_num_lote  TYPE numc10
        i_bukrs     TYPE bukrs
        i_dt_vcto   TYPE erdat
        i_mes_vcto  TYPE numc2
        i_ano_vcto  TYPE numc4
        i_placa     TYPE kfzkz
        i_filial    TYPE gsber
        i_kostl     TYPE kostl
        i_c_imposto TYPE zcod_imposto
        i_c_barras  TYPE zcod_barras
        i_lifnr     TYPE lifnr
        i_vlr_princ TYPE dmbtr
        i_vlr_corre TYPE dmbtr
        i_vlr_multa TYPE dmbtr
        i_vlr_juros TYPE dmbtr
        i_vlr_tse   TYPE dmbtr
        i_vlr_total TYPE dmbtr
      EXPORTING
        e_num_docu  TYPE numc10.

    DATA: gt_zimp_lanc_imp_ct TYPE TABLE OF zimp_lanc_imp_ct,
          wl_zimp_cad_imposto TYPE zimp_cad_imposto, "Cadastro dos Imposto
          wl_zimp_lanc_imp_ct TYPE zimp_lanc_imp_ct,
          wl_zimp_cad_lote    TYPE zimp_cad_lote,
          wl_zaa002           TYPE zaa002.
ENDCLASS.                    "GERAR_CONTAS_PAGAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS GERAR_CONTAS_PAGAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gerar_contas_pagar IMPLEMENTATION.
  METHOD seleciona_imposto.
    CLEAR: gt_zimp_cad_imp_con.

    SELECT SINGLE *
      FROM zimp_cad_imposto
      INTO wl_zimp_cad_imposto
     WHERE cod_imposto = i_cod_imposto.

    SELECT *
      FROM zimp_cad_imp_con
      INTO TABLE gt_zimp_cad_imp_con
     WHERE cod_imposto = i_cod_imposto.
  ENDMETHOD.                    "SELECIONA_IMPOSTO

  METHOD gera_lote.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZID_LOT'
      IMPORTING
        number      = e_num_lote.

    MOVE:
    sy-mandt   TO wl_zimp_cad_lote-mandt,
    e_num_lote TO wl_zimp_cad_lote-lote,
    i_bukrs    TO wl_zimp_cad_lote-bukrs,
    i_descr    TO wl_zimp_cad_lote-descr_lote,
    sy-uname   TO wl_zimp_cad_lote-usnam,
    i_depto    TO wl_zimp_cad_lote-dep_resp,
    i_venci    TO wl_zimp_cad_lote-dt_venc,
    sy-uname   TO wl_zimp_cad_lote-usuario,
    sy-datum   TO wl_zimp_cad_lote-data_atual,
    sy-uzeit   TO wl_zimp_cad_lote-hora_atual.

*   Retorna data de vencimento para o chamador.
    e_dt_venci = i_venci.

    INSERT zimp_cad_lote FROM wl_zimp_cad_lote.
    COMMIT WORK.

  ENDMETHOD.                    "GERA_LOTE

  METHOD gera_doc_imposto.
    DATA: r_utils  TYPE REF TO zutils,
          wl_cskb  TYPE cskb,
          wl_tka02 TYPE tka02.

    CLEAR:gt_zimp_lanc_imp_ct,
          wl_zimp_lanc_imp_ct.

    CREATE OBJECT r_utils.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = 'ZT'
        object      = 'RF_BELEG'
        subobject   = i_bukrs
      IMPORTING
        number      = e_num_docu.

    r_utils->checar_dia_util( EXPORTING i_data = i_dt_vcto
                              IMPORTING e_data = wl_zimp_lanc_impost-dt_apuracao ).

    wl_zimp_lanc_impost-bukrs        = i_bukrs.
    wl_zimp_lanc_impost-lote         = i_num_lote.
    wl_zimp_lanc_impost-doc_imposto  = e_num_docu.
    wl_zimp_lanc_impost-dt_venc      = i_dt_vcto .
    wl_zimp_lanc_impost-mes_apuracao = i_mes_vcto.
    wl_zimp_lanc_impost-ano_apuracao = i_ano_vcto.
    wl_zimp_lanc_impost-observacao   = i_placa.
    wl_zimp_lanc_impost-cod_imposto  = i_c_imposto.
    wl_zimp_lanc_impost-ref_imposto  = wl_zimp_cad_imposto-descr_imposto.
    wl_zimp_lanc_impost-tp_imposto   = wl_zimp_cad_imposto-tp_imposto.
    wl_zimp_lanc_impost-cod_pgto     = wl_zimp_cad_imposto-cod_pgto.
    wl_zimp_lanc_impost-conv_banco   = wl_zimp_cad_imposto-conv_banco.
    wl_zimp_lanc_impost-hbkid        = wl_zimp_cad_imposto-hbkid.
    wl_zimp_lanc_impost-gsber        = i_filial.
    wl_zimp_lanc_impost-waers        = wl_zimp_cad_imposto-waers.
    wl_zimp_lanc_impost-cod_barras   = i_c_barras.
    wl_zimp_lanc_impost-data_atual   = wl_zimp_cad_lote-data_atual.
    wl_zimp_lanc_impost-hora_atual   = wl_zimp_cad_lote-hora_atual.
    wl_zimp_lanc_impost-usuario      = wl_zimp_cad_lote-usuario.
    INSERT zimp_lanc_impost FROM wl_zimp_lanc_impost.
    COMMIT WORK.

    SORT gt_zimp_cad_imp_con BY cod_abertura.
    LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

      wl_zimp_lanc_imp_ct-seqitem      = sy-tabix.

      CASE wl_zimp_cad_imp_con-cod_abertura.
*---> 07/06/2023 - Migração S4 - JS
*        WHEN 01. "Principal
*            WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_PRINC.
*        WHEN 02. "Correção
*            WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_CORRE.
*        when 04. "Multas
*          WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_MULTA.
*        when 05. "Juros
*           WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_JUROS.
*        when 06. "Tse
*           WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_TSE.
        WHEN 01. "Principal
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_princ ).
        WHEN 02. "Correção
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_corre ).
        WHEN 04. "Multas
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_multa ).
        WHEN 05. "Juros
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_juros ).
        WHEN 06. "Tse
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_tse ).
*<--- 07/06/2023 - Migração S4 - JS
        WHEN 11. "Fornecedor
          wl_zimp_lanc_imp_ct-lifnr     = i_lifnr.
          wl_zimp_lanc_imp_ct-valor_imp = ( i_vlr_total * -1 ).
        WHEN OTHERS.
      ENDCASE.

*   Seleciona área de contabilidade de custos
      SELECT SINGLE *
        FROM tka02
        INTO wl_tka02
       WHERE bukrs  = i_bukrs.


* ---> S4 Migration - 09/07/2023 - JP
*      select single *
*        from CSKB
*        into WL_CSKB
*       where KOKRS  = WL_TKA02-KOKRS
*         and KSTAR  = WL_ZIMP_CAD_IMP_CON-HKONT
*         and DATAB  <= SY-DATUM
*         and DATBI  >= SY-DATUM.
*

      DATA: lt_returns TYPE TABLE OF bapiret2,
            ls_coeldes TYPE bapi1030_ceoutputlist,
            vkokrs     TYPE tka02-kokrs.

      DATA: lv_controllingarea TYPE  bapi1030_gen-co_area,
            lv_costelement     TYPE  bapi1030_gen-cost_elem,
            lv_keydate         TYPE  bapi1030_gen-some_date.


      MOVE wl_tka02-kokrs TO vkokrs.

      lv_controllingarea  = vkokrs.
      "        lv_costelement      = wg_criaadt-saknrz. ver
      lv_keydate          = sy-datum.

      CLEAR: lt_returns[], ls_coeldes.

      CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
        EXPORTING
          controllingarea   = lv_controllingarea
          costelement       = lv_costelement
          keydate           = lv_keydate
        IMPORTING
          costelementdetail = ls_coeldes
        TABLES
          return            = lt_returns.

      READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.

      IF sy-subrc NE 0.
        wl_cskb-kokrs = lv_controllingarea.
        wl_cskb-kstar = lv_costelement .
        wl_cskb-datbi = ls_coeldes-valid_to.
        wl_cskb-katyp = ls_coeldes-celem_category.
      ENDIF.

      CLEAR sy-subrc.
      IF wl_cskb IS NOT INITIAL.
        sy-subrc = 99.  "MARCAR ERRO PARA FASE SEGUINTE FICAR COMO ESTÁ..
      ENDIF.

* <--- S4 Migration - 09/07/2023 - JP

      IF ( sy-subrc IS INITIAL ).
        wl_zimp_lanc_imp_ct-kostl = i_kostl.
      ENDIF.

      wl_zimp_lanc_imp_ct-doc_imposto  = e_num_docu.
      wl_zimp_lanc_imp_ct-cod_imposto  = wl_zimp_cad_imp_con-cod_imposto.
      wl_zimp_lanc_imp_ct-cod_abertura = wl_zimp_cad_imp_con-cod_abertura.
      wl_zimp_lanc_imp_ct-bukrs        = i_bukrs.
      wl_zimp_lanc_imp_ct-bschl        = wl_zimp_cad_imp_con-bschl.
      wl_zimp_lanc_imp_ct-hkont        = wl_zimp_cad_imp_con-hkont.
      wl_zimp_lanc_imp_ct-gsber        = i_filial.
      wl_zimp_lanc_imp_ct-kostl        = i_kostl.
      wl_zimp_lanc_imp_ct-data_atual   = sy-datum.
      wl_zimp_lanc_imp_ct-hora_atual   = sy-uzeit.
      wl_zimp_lanc_imp_ct-usuario      = sy-uname.
      APPEND wl_zimp_lanc_imp_ct TO gt_zimp_lanc_imp_ct.
      CLEAR: wl_zimp_lanc_imp_ct.
    ENDLOOP.

    INSERT zimp_lanc_imp_ct FROM TABLE gt_zimp_lanc_imp_ct.
    COMMIT WORK.
  ENDMETHOD.                    "GERA_DOC_IMPOSTO
ENDCLASS.                    "GERAR_CONTAS_PAGAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TIPO_OPERACAO_0110 DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_tipo_operacao DEFINITION.
  PUBLIC SECTION.

    METHODS salvar_dados_0140.
    METHODS salvar_dados_0200.
    METHODS editar_dados_0200.

    DATA: r_utils TYPE REF TO zutils,
          cont    TYPE n VALUE 1.
ENDCLASS.                    "LCL_TIPO_OPERACAO_0110 DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TIPO_OPERACAO_0110 IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_tipo_operacao IMPLEMENTATION.

  METHOD salvar_dados_0140.
    IF ( lines IS NOT INITIAL ).

      LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = 'X'.
        MOVE-CORRESPONDING wl_saida_0140 TO wl_zaa003.
        APPEND wl_zaa003 TO gt_zaa003.
      ENDLOOP.

      MODIFY zaa003 FROM TABLE gt_zaa003.
      COMMIT WORK.

      MESSAGE TEXT-s02 TYPE 'I' DISPLAY LIKE 'S'.

    ELSE.
      MESSAGE TEXT-e02 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "SALVAR_DADOS_0140

  METHOD salvar_dados_0200.
    CLEAR wl_zaa001-observacao.
    CREATE OBJECT r_utils.

    return_status = 1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_saida_0110-anln1
      IMPORTING
        output = wl_saida_0110-anln1.

    CALL METHOD obj_custom_editor->get_text_as_r3table
      IMPORTING
        table = gt_editor.

    LOOP AT gt_editor INTO wl_editor.
      CONCATENATE wl_zaa001-observacao wl_editor
      INTO wl_zaa001-observacao.
    ENDLOOP.

    IF ( modo_operacao EQ c_edit ).
      wl_zaa001-dt_modif     = sy-datum.
      wl_zaa001-hr_modif     = sy-uzeit.
      wl_zaa001-user_modif   = sy-uname.
    ELSE.
      wl_zaa001-dt_criacao   = sy-datum.
      wl_zaa001-hr_criacao   = sy-uzeit.
      wl_zaa001-user_criacao = sy-uname.
    ENDIF.

    MOVE:
    wl_saida_0110-bukrs TO wl_zaa001-bukrs,
    wl_saida_0110-anln1 TO wl_zaa001-anln1,
    wl_saida_0110-anln2 TO wl_zaa001-anln2,
    wl_saida_0110-kfzkz TO wl_zaa001-kfzkz.
    MODIFY zaa001 FROM wl_zaa001.

*   Exibe as informações inseridas nos dados complementares na ALV principal.
    MOVE-CORRESPONDING wl_zaa001 TO wl_saida_0110.
    MODIFY gt_saida_0110 FROM wl_saida_0110 INDEX wl_selected_rows-index.
*   --

    r_utils->tratar_campos( group1    = 'GR1'
                            group2    = space
                            value     = '0'
                            invisible = '0').

    r_utils->tratar_campos( group1    = 'GR2'
                            group2    = space
                            value     = '0'
                            invisible = '0').

    MESSAGE TEXT-s01 TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.                    "SALVAR_DADOS

  METHOD editar_dados_0200.
    CREATE OBJECT r_utils.

    modo_operacao = c_edit.
    return_status = 0.

    r_utils->tratar_campos( group1    = 'GR1'
                            group2    = space
                            value     = '1'
                            invisible = '0').

  ENDMETHOD.                    "EDITAR_DADOS
ENDCLASS.                    "LCL_TIPO_OPERACAO_0110 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_seleciona_dados DEFINITION.
  PUBLIC SECTION.

    METHODS seleciona_dados_0110.
    METHODS seleciona_dados_0140.
    METHODS seleciona_dados_0170.
    METHODS seleciona_dados_0180.
    METHODS seleciona_dados_0200.

    DATA: gt_anla   TYPE TABLE OF anla,
          gt_anlz   TYPE TABLE OF anlz,
          wl_anla   TYPE anla,
          wl_anla_c TYPE anla,
          wl_anlz   TYPE anlz,
          at_cont   TYPE c.

    CONSTANTS:
      c_ipva   TYPE char04 VALUE 'Ipva',
      c_dpvat  TYPE char05 VALUE 'Dpvat',
      c_licenc TYPE char20 VALUE 'Licenciamento'.
ENDCLASS.                    "LCL_SELECIONA_DADOS DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_seleciona_dados IMPLEMENTATION.
  METHOD seleciona_dados_0110.
    CLEAR: gt_anla, gt_anlz, gt_saida_0110.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_anla
      FROM anla AS a
INNER JOIN anlz AS b ON a~bukrs = b~bukrs AND
                        a~anln1 = b~anln1 AND
                        a~anln2 = b~anln2
     WHERE a~anln1 IN s_anln1
       AND a~bukrs IN s_bukrs
       AND a~aktiv IN s_incor
       AND a~deakt IN s_desat
       AND a~anlkl IN ('00010701', '00010702', '00010703', '00081010')
       AND b~kfzkz IN s_kfzkz
       AND b~werks IN s_werks
       AND b~bdatu EQ '99991231'.

    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2
       AND bdatu EQ '99991231'.

    SORT: gt_anlz BY bukrs
                     anln1.

    LOOP AT gt_anla INTO wl_anla.
      CLEAR wl_zaa001.

      READ TABLE gt_anlz INTO wl_anlz WITH KEY bukrs = wl_anla-bukrs
                                               anln1 = wl_anla-anln1 BINARY SEARCH.

      SELECT SINGLE *
        FROM zaa001
        INTO wl_zaa001
       WHERE bukrs = wl_anla-bukrs
         AND anln1 = wl_anla-anln1
         AND anln2 = wl_anla-anln2.
      "AND KFZKZ = WL_ANLZ-KFZKZ.

      wl_saida_0110-dt_incor    = wl_anla-aktiv.
      wl_saida_0110-dt_desat    = wl_anla-deakt.
      wl_saida_0110-anln1       = wl_anla-anln1.
      wl_saida_0110-anln2       = wl_anla-anln2.
      wl_saida_0110-bukrs       = wl_anla-bukrs.
      wl_saida_0110-filial      = wl_anlz-werks.
      wl_saida_0110-kfzkz       = wl_anlz-kfzkz.
      wl_saida_0110-centro      = wl_anlz-kostl.
      wl_saida_0110-nr_chassi   = wl_anla-invnr.
      wl_saida_0110-txt_princ   = wl_anla-txt50.
      wl_saida_0110-porte_obrig = wl_zaa001-porte_obrig.    "CS2019001264 28.05.2020
      wl_saida_0110-pais        = wl_zaa001-cod_pais.
      wl_saida_0110-regiao      = wl_zaa001-cod_regi.
      wl_saida_0110-ano_fabr    = wl_zaa001-ano_fabr.
      wl_saida_0110-ano_mod     = wl_zaa001-ano_mode.
      wl_saida_0110-potencia    = wl_zaa001-potencia.
      wl_saida_0110-cor         = wl_zaa001-cor.
      wl_saida_0110-pg_arq      = wl_zaa001-pg_arq.
      wl_saida_0110-resp_veic   = wl_zaa001-resp_veic.
      wl_saida_0110-cod_renavan = wl_zaa001-cod_registro.
      wl_saida_0110-mes_ipva    = wl_zaa001-mes_ipva.
      wl_saida_0110-mes_licenc  = wl_zaa001-mes_licenc.
      wl_saida_0110-mes_dpvat   = wl_zaa001-mes_dpvat.
      wl_saida_0110-dt_criacao  = wl_zaa001-dt_criacao.
      wl_saida_0110-hr_criacao  = wl_zaa001-hr_criacao.
      wl_saida_0110-user_criac  =	wl_zaa001-user_criacao.
      wl_saida_0110-dt_modif    = wl_zaa001-dt_modif.
      wl_saida_0110-user_modif  = wl_zaa001-user_modif.
      wl_saida_0110-observacao  = wl_zaa001-observacao.

      APPEND wl_saida_0110 TO gt_saida_0110.
      CLEAR wl_saida_0110.
    ENDLOOP.
  ENDMETHOD.                    "SELECIONA_DADOS_0110

  METHOD seleciona_dados_0140.
    CLEAR: gt_zaa001, gt_saida_0140, gt_saida_0140_aux2.

    DATA: r_utils         TYPE REF TO zutils,
          at_dia_util     TYPE sy-datum,
          at_tp_obrigacao TYPE c,
          at_objkey       TYPE awkey,
          return_status   LIKE abap_true,
          t_zimp_consulta TYPE TABLE OF  zimp_lanc_impost,
          w_zimp_consulta TYPE zimp_lanc_impost,
          w_zimp_imp_ct   TYPE zimp_lanc_imp_ct,
          t_estorno       TYPE TABLE OF bkpf,
          w_estorno       TYPE bkpf.

    CREATE OBJECT r_utils.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_zaa001
      FROM zaa001 AS a
INNER JOIN anlz   AS b ON a~bukrs = b~bukrs AND
                          a~anln1 = b~anln1 AND
                          a~anln2 = b~anln2
     WHERE a~anln1      IN s_imobil
       AND a~bukrs      IN s_empres
       AND b~kfzkz      IN s_placa
       AND b~werks      IN s_filial
       AND b~bdatu      EQ '99991231'.

    IF gt_zaa001[] IS NOT INITIAL.
      SELECT *
        FROM anla
        INTO TABLE gt_anla
         FOR ALL ENTRIES IN gt_zaa001
       WHERE bukrs = gt_zaa001-bukrs
         AND anln1 = gt_zaa001-anln1
         AND anln2 = gt_zaa001-anln2.
    ENDIF.

    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_zaa001
     WHERE bukrs = gt_zaa001-bukrs
       AND anln1 = gt_zaa001-anln1
       AND kfzkz = gt_zaa001-kfzkz.

**********02.05.2018
**********Ajuste de Placas comparando as tabelas ANLZ e ZAA001

    SELECT *
      FROM zaa001
      INTO TABLE @DATA(tl_zaa001)
      FOR ALL ENTRIES IN @gt_zaa001
      WHERE bukrs       = @gt_zaa001-bukrs AND
            anln1       = @gt_zaa001-anln1.

    LOOP AT gt_zaa001 INTO wl_zaa001.

      READ TABLE tl_zaa001 INTO wl_zaa001_aux WITH KEY anln1 = wl_zaa001-anln1 bukrs = wl_zaa001-bukrs.

      IF wl_zaa001_aux-kfzkz NE wl_zaa001-kfzkz.
        wl_zaa001_aux-kfzkz = wl_zaa001-kfzkz.
        MODIFY zaa001 FROM wl_zaa001.
      ENDIF.

    ENDLOOP.

************
************************************************************


    SORT: gt_anlz BY bukrs
                     anln1.

    LOOP AT gt_zaa001 INTO wl_zaa001.

      CLEAR:  wl_anlz.

      IF wl_zaa001-kfzkz IS NOT INITIAL.

        READ TABLE gt_anlz INTO wl_anlz WITH KEY anln1 = wl_zaa001-anln1
                                                 anln2 = wl_zaa001-anln2. "BINARY SEARCH.
        SELECT SINGLE *
          FROM anla
          INTO wl_anla_c
          WHERE anln1 = wl_zaa001-anln1
            AND bukrs = wl_zaa001-bukrs.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: wl_anla_c.
        ENDIF.

        IF wl_anla_c-deakt IS NOT INITIAL.
          DELETE TABLE gt_zaa001 FROM wl_zaa001." WHERE ANLN1 = WL_ZAA001-ANLN1.
          CLEAR wl_zaa001.
          CONTINUE.
        ENDIF.

        SELECT SINGLE *
          FROM t001
          INTO wl_t001
         WHERE bukrs = wl_zaa001-bukrs.

        at_cont = 1.

*     1 = Ipva | 2 = Dpvat | 3 = Licenciamento

        WHILE ( at_cont <= 3 ).
          CLEAR: wl_saida_0140,  wl_saida_0140_aux2, gt_estilo[], wl_zaa003, return_status, wl_zaa002, gt_zimp_cad_imp_con,
                 gt_fields_style, w_estorno, w_zimp_consulta.

          at_strlen       = ( strlen( wl_zaa001-kfzkz ) ).
          at_strlen = at_strlen - 1.
*        at_tp_obrigacao = at_cont.

          SELECT SINGLE *
            FROM zaa002
            INTO wl_zaa002
           WHERE land1       = wl_zaa001-cod_pais
             AND rg_placa    = wl_zaa001-cod_regi
             AND final_placa = wl_zaa001-kfzkz+at_strlen(1)
             AND tp_obrig    = at_cont.

          sy-datum+6(2) = 10.
          sy-datum+0(4) = s_ano-low.


          CASE at_cont.
            WHEN '1'.

              SELECT  SINGLE *
                 FROM zaa003
                 INTO wl_zaa003
                WHERE anln1    EQ wl_zaa001-anln1
                  AND tp_obrig EQ c_ipva
                  AND ano_vcto EQ s_ano-low.


              IF ( sy-subrc IS INITIAL ).
                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT  SINGLE *
                   FROM zaa003
                   INTO wl_zaa003
                  WHERE anln1    EQ wl_zaa001-anln1
                    AND tp_obrig EQ c_ipva
                    AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_ipva.
                  wl_zaa003-tp_obrig = c_ipva.

                ELSE.

                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status       = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_ipva.
                wl_zaa003-tp_obrig = c_ipva.
              ENDIF.



              IF return_status IS INITIAL.
                IF wl_zaa001-mes_ipva <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_ipva = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.
                ENDIF.

                IF ( wl_zaa001-mes_ipva IS NOT INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_ipva.
              ELSE.

                IF ( wl_zaa001-mes_ipva IS NOT INITIAL ).
                  sy-datum+4(2) = wl_zaa001-mes_ipva.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.




            WHEN '2'.

              SELECT SINGLE *
                FROM zaa003
                INTO wl_zaa003

               WHERE anln1    EQ wl_zaa001-anln1
                 AND tp_obrig EQ c_dpvat
                 AND ano_vcto EQ s_ano-low.

              IF ( sy-subrc IS INITIAL ).

                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT SINGLE *
                    FROM zaa003
                    INTO wl_zaa003

                   WHERE anln1    EQ wl_zaa001-anln1
                     AND tp_obrig EQ c_dpvat
                     AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_dpvat.
                  wl_zaa003-tp_obrig = c_dpvat.

                ELSE.
                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_dpvat.
                wl_zaa003-tp_obrig = c_dpvat.
              ENDIF.


              IF return_status IS INITIAL.

                IF wl_zaa001-mes_dpvat <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_dpvat = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.
                ENDIF.

                IF ( NOT wl_zaa001-mes_dpvat IS INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_dpvat.

              ELSE.

                IF ( NOT wl_zaa001-mes_dpvat IS INITIAL ).
*                SY-DATUM+6(2) = 10.
                  sy-datum+4(2) = wl_zaa001-mes_dpvat.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.


            WHEN '3'.


              SELECT SINGLE *
                FROM zaa003
                INTO wl_zaa003
               WHERE anln1    EQ wl_zaa001-anln1
                 AND tp_obrig EQ c_licenc
                 AND ano_vcto EQ s_ano-low.


              IF ( sy-subrc IS INITIAL ).

                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT SINGLE *
                    FROM zaa003
                    INTO wl_zaa003
                   WHERE anln1    EQ wl_zaa001-anln1
                     AND tp_obrig EQ c_licenc
                     AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_licenc.
                  wl_zaa003-tp_obrig = c_licenc.

                ELSE.

                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_licenc.
                wl_zaa003-tp_obrig = c_licenc.
              ENDIF.


              IF return_status IS INITIAL.

                IF wl_zaa001-mes_licenc <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_licenc = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.

                ENDIF.

                IF ( NOT wl_zaa001-mes_licenc IS INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_licenc.

              ELSE.

                IF ( NOT wl_zaa001-mes_licenc IS INITIAL ).

                  sy-datum+4(2) = wl_zaa001-mes_licenc.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.


          ENDCASE.

          SELECT SINGLE *
            FROM zimp_lanc_impost
            INTO  w_zimp_consulta
          WHERE bukrs EQ wl_zaa003-bukrs
            AND lote  EQ wl_zaa003-lote
            AND doc_imposto EQ wl_zaa003-doc_imposto.

          IF w_zimp_consulta-doc_imposto <> '0000000000' .

            SELECT SINGLE *
              FROM zimp_lanc_imp_ct
               INTO w_zimp_imp_ct
             WHERE doc_imposto EQ w_zimp_consulta-doc_imposto
               AND cod_imposto EQ w_zimp_consulta-cod_imposto
               AND cod_abertura EQ w_zimp_consulta-tp_imposto
               AND bukrs        EQ wl_zaa003-bukrs. " acrescentado empresa pois numeração doc_imposto pode repetir

            IF sy-subrc = 0.
              wl_saida_0140_aux2-filial_pg         = w_zimp_imp_ct-gsber.
              wl_saida_0140_aux2-kostl_pg          = w_zimp_imp_ct-kostl.
            ENDIF.

          ENDIF.

          wl_saida_0140_aux2-anln1        = wl_zaa001-anln1.
          wl_saida_0140_aux2-anln2        = wl_zaa001-anln2.
          wl_saida_0140_aux2-txt50        = wl_anla_c-txt50.
          wl_saida_0140_aux2-ano_vcto     = wl_zaa003-ano_vcto.
          wl_saida_0140_aux2-bukrs        = wl_zaa001-bukrs.
          wl_saida_0140_aux2-cod_barras   = w_zimp_consulta-cod_barras.    "WL_ZAA003-COD_BARRAS.
          wl_saida_0140_aux2-cod_imposto  = wl_zaa003-cod_imposto.
          wl_saida_0140_aux2-conv_banc    = wl_zaa003-conv_banc.
          wl_saida_0140_aux2-cod_regi     = wl_zaa001-cod_regi.
          wl_saida_0140_aux2-cod_registro = wl_zaa001-cod_registro.
          IF return_status IS NOT INITIAL.
            wl_saida_0140_aux2-dep_resp     = wl_zaa003-dep_resp.
          ELSE.
            wl_saida_0140_aux2-dep_resp     = '76'.
          ENDIF.
          wl_saida_0140_aux2-doc_imposto  = wl_zaa003-doc_imposto.
          wl_saida_0140_aux2-dt_venc      = wl_zaa003-dt_venc.
          wl_saida_0140_aux2-erdat        = wl_zaa003-erdat.
          wl_saida_0140_aux2-ernam        = wl_zaa003-ernam.
          wl_saida_0140_aux2-kfzkz        = wl_zaa001-kfzkz.
          wl_saida_0140_aux2-lifnr        = wl_zaa003-lifnr.
          wl_saida_0140_aux2-lote         = wl_zaa003-lote.
          wl_saida_0140_aux2-mes_vcto     = wl_zaa003-mes_vcto.
          wl_saida_0140_aux2-tp_obrig     = wl_zaa003-tp_obrig.
          wl_saida_0140_aux2-vlr_corre    = wl_zaa003-vlr_corre.
          wl_saida_0140_aux2-vlr_juros    = wl_zaa003-vlr_juros.
          wl_saida_0140_aux2-vlr_multa    = wl_zaa003-vlr_multa.
          wl_saida_0140_aux2-vlr_princ    = wl_zaa003-vlr_princ.
          wl_saida_0140_aux2-vlr_total    = wl_zaa003-vlr_total.
          wl_saida_0140_aux2-vlr_tse      = wl_zaa003-vlr_tse.
          wl_saida_0140_aux2-waers        = wl_zaa003-waers.
          wl_saida_0140_aux2-werks        = wl_zaa003-werks.
          wl_saida_0140_aux2-kostl        = wl_anlz-kostl.



          IF ( return_status IS INITIAL ).

            wl_saida_0140_aux2-status      = icon_light_out.
            wl_saida_0140_aux2-werks       = wl_anlz-werks.
            wl_saida_0140_aux2-ano_vcto    = s_ano-low.
            wl_saida_0140_aux2-dt_venc     = at_dia_util.
            wl_saida_0140_aux2-waers       = wl_t001-waers.
            wl_saida_0140_aux2-lifnr       = wl_zaa002-lifnr.
            wl_saida_0140_aux2-cod_imposto = wl_zaa002-cod_imposto.

            SELECT *
              FROM zimp_cad_imp_con
              INTO TABLE gt_zimp_cad_imp_con
            WHERE cod_imposto = wl_zaa002-cod_imposto.


            SORT gt_zimp_cad_imp_con BY cod_abertura.
            LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

              CASE wl_zimp_cad_imp_con-cod_abertura.
                WHEN 01. "Principal
                  wl_fields_style-fieldname = 'VLR_PRINC'.
                WHEN 02. "Correção
                  wl_fields_style-fieldname = 'VLR_CORRE'.
                WHEN 04. "Multas
                  wl_fields_style-fieldname = 'VLR_MULTA'.
                WHEN 05. "Juros
                  wl_fields_style-fieldname = 'VLR_JUROS'.
                WHEN 06. "Tse
                  wl_fields_style-fieldname = 'VLR_TSE'.
                WHEN OTHERS.
                  EXIT.
              ENDCASE.

              APPEND wl_fields_style TO gt_fields_style.
              CLEAR wl_fields_style.
            ENDLOOP.

            SORT gt_fields_style BY fieldname.
            LOOP AT gt_fields_style INTO wl_fields_style.
              r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                             style     = cl_gui_alv_grid=>mc_style_enabled ).
            ENDLOOP.

          ELSE.

            IF wl_saida_0140_aux2-doc_contabil IS INITIAL.

              SELECT *
                FROM zimp_cad_imp_con
                INTO TABLE gt_zimp_cad_imp_con
              WHERE cod_imposto = wl_zaa002-cod_imposto.

              SORT gt_zimp_cad_imp_con BY cod_abertura.
              LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

                CASE wl_zimp_cad_imp_con-cod_abertura.
                  WHEN 01. "Principal
                    wl_fields_style-fieldname = 'VLR_PRINC'.
                  WHEN 02. "Correção
                    wl_fields_style-fieldname = 'VLR_CORRE'.
                  WHEN 04. "Multas
                    wl_fields_style-fieldname = 'VLR_MULTA'.
                  WHEN 05. "Juros
                    wl_fields_style-fieldname = 'VLR_JUROS'.
                  WHEN 06. "Tse
                    wl_fields_style-fieldname = 'VLR_TSE'.
                  WHEN OTHERS.
                    EXIT.
                ENDCASE.

                APPEND wl_fields_style TO gt_fields_style.
                CLEAR wl_fields_style.
              ENDLOOP.

              SORT gt_fields_style BY fieldname.
              LOOP AT gt_fields_style INTO wl_fields_style.
                r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                               style     = cl_gui_alv_grid=>mc_style_enabled ).
              ENDLOOP.

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140_aux2-estilo.

              CLEAR:  gt_estilo[], gt_fields_style.

            ENDIF.
            "BREAK-POINT.
            r_utils->z_style_disable_edit( fieldname = 'ANO_VCTO'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'COD_IMPOSTO'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'CONV_BANC'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'DEP_RESP'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'FILIAL_PG'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'KOSTL_PG'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'WAERS'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).

          ENDIF.

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140_aux2-estilo.
          APPEND wl_saida_0140_aux2 TO gt_saida_0140_aux2.
          ADD 1 TO at_cont.
        ENDWHILE.

      ENDIF.

    ENDLOOP.

    IF s_mesv IS NOT INITIAL.

      LOOP AT gt_saida_0140_aux2 INTO wl_saida_0140_aux2  WHERE mes_vcto =  s_mesv-low.

        wl_saida_0140-status         =     wl_saida_0140_aux2-status.
        wl_saida_0140-check          =     wl_saida_0140_aux2-check.
        wl_saida_0140-kfzkz          =     wl_saida_0140_aux2-kfzkz.
        wl_saida_0140-bukrs          =     wl_saida_0140_aux2-bukrs.
        wl_saida_0140-werks          =     wl_saida_0140_aux2-werks.
        wl_saida_0140-anln1          =     wl_saida_0140_aux2-anln1.
        wl_saida_0140-anln2          =     wl_saida_0140_aux2-anln2.
        wl_saida_0140-txt50          =     wl_saida_0140_aux2-txt50.
        wl_saida_0140-mes_vcto       =     wl_saida_0140_aux2-mes_vcto.
        wl_saida_0140-cod_regi       =     wl_saida_0140_aux2-cod_regi.
        wl_saida_0140-cod_registro   =     wl_saida_0140_aux2-cod_registro.
        wl_saida_0140-tp_obrig       =     wl_saida_0140_aux2-tp_obrig.
        wl_saida_0140-ano_vcto       =     wl_saida_0140_aux2-ano_vcto.
        wl_saida_0140-dt_venc        =     wl_saida_0140_aux2-dt_venc.
        wl_saida_0140-dep_resp       =     wl_saida_0140_aux2-dep_resp.
        wl_saida_0140-cod_imposto    =     wl_saida_0140_aux2-cod_imposto.
        wl_saida_0140-waers          =     wl_saida_0140_aux2-waers.
        wl_saida_0140-kostl          =     wl_saida_0140_aux2-kostl.
        wl_saida_0140-kostl_pg       =     wl_saida_0140_aux2-kostl_pg.
        wl_saida_0140-filial_pg      =     wl_saida_0140_aux2-filial_pg.
        wl_saida_0140-lifnr          =     wl_saida_0140_aux2-lifnr.
        wl_saida_0140-vlr_princ      =     wl_saida_0140_aux2-vlr_princ.
        wl_saida_0140-vlr_corre      =     wl_saida_0140_aux2-vlr_corre.
        wl_saida_0140-vlr_multa      =     wl_saida_0140_aux2-vlr_multa.
        wl_saida_0140-vlr_juros      =     wl_saida_0140_aux2-vlr_juros.
        wl_saida_0140-vlr_tse        =     wl_saida_0140_aux2-vlr_tse.
        wl_saida_0140-vlr_total      =     wl_saida_0140_aux2-vlr_total.
        wl_saida_0140-conv_banc      =     wl_saida_0140_aux2-conv_banc.
        wl_saida_0140-cod_barras     =     wl_saida_0140_aux2-cod_barras.
        wl_saida_0140-estilo         =     wl_saida_0140_aux2-estilo.
        wl_saida_0140-erdat          =     wl_saida_0140_aux2-erdat.
        wl_saida_0140-ernam          =     wl_saida_0140_aux2-ernam.
        wl_saida_0140-lote           =     wl_saida_0140_aux2-lote.
        wl_saida_0140-doc_contabil   =     wl_saida_0140_aux2-doc_contabil.
        wl_saida_0140-doc_imposto    =     wl_saida_0140_aux2-doc_imposto.

        APPEND wl_saida_0140 TO gt_saida_0140.

        CLEAR: wl_saida_0140, wl_saida_0140_aux2.
      ENDLOOP.
    ELSE.
      MOVE-CORRESPONDING gt_saida_0140_aux2 TO gt_saida_0140.
    ENDIF.

  ENDMETHOD.

  METHOD seleciona_dados_0170.
    CLEAR: gt_anla, gt_anlz, gt_saida_0170,
           gt_zaa001.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_anla
      FROM anla AS a
INNER JOIN anlz AS b ON a~bukrs = b~bukrs AND
                        a~anln1 = b~anln1 AND
                        a~anln2 = b~anln2
     WHERE a~anln1 IN s1_anln1
       AND a~bukrs IN s1_bukrs
       AND a~aktiv IN s1_incor
       AND a~deakt IN s1_desat
       AND b~kfzkz IN s1_kfzkz
       AND a~anlkl IN ('00010701', '00010702', '00010703', '00081010')
       AND b~werks IN s1_werks
       AND b~bdatu EQ '99991231'.

    IF s1_descr-low IS NOT INITIAL.
      DELETE gt_anla WHERE txt50 NS s1_descr-low.
    ENDIF.


    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2
       AND bdatu EQ '99991231'.

    SELECT *
      FROM zaa001
      INTO TABLE gt_zaa001
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2.

    LOOP AT gt_anla INTO wl_anla.
      READ TABLE: gt_anlz   INTO wl_anlz   WITH KEY anln1 = wl_anla-anln1,
                  gt_zaa001 INTO wl_zaa001 WITH KEY anln1 = wl_anla-anln1.

      wl_saida_0170-dt_incor    = wl_anla-aktiv.
      wl_saida_0170-dt_desat    = wl_anla-deakt.
      wl_saida_0170-anln1          = wl_anla-anln1.
      wl_saida_0170-anln2          = wl_anla-anln2.
      wl_saida_0170-bukrs          = wl_anla-bukrs.
      wl_saida_0170-nr_chassi      = wl_anla-invnr.
*      wl_saida_0170-txt_princ      = wl_anla-mcoa1.                         "CS2019001264 28.05.2020
      CONCATENATE wl_anla-txt50 wl_anla-txa50 INTO wl_saida_0170-txt_princ
                                              SEPARATED BY space.
      wl_saida_0170-filial         = wl_anlz-werks.
      wl_saida_0170-kfzkz          = wl_anlz-kfzkz.
      wl_saida_0170-centro         = wl_anlz-kostl.
      wl_saida_0170-pais           = wl_zaa001-cod_pais.
      wl_saida_0170-regiao         = wl_zaa001-cod_regi.
      wl_saida_0170-ano_fabr       = wl_zaa001-ano_fabr.
      wl_saida_0170-ano_mod        = wl_zaa001-ano_mode.
      wl_saida_0170-potencia       = wl_zaa001-potencia.
      wl_saida_0170-cor            = wl_zaa001-cor.
      wl_saida_0170-pg_arq         = wl_zaa001-pg_arq.
      wl_saida_0170-resp_veic      = wl_zaa001-resp_veic.
      wl_saida_0170-cod_renavan    = wl_zaa001-cod_registro.
      wl_saida_0170-mes_ipva       = wl_zaa001-mes_ipva.
      wl_saida_0170-mes_licenc     = wl_zaa001-mes_licenc.
      wl_saida_0170-mes_dpvat	     = wl_zaa001-mes_dpvat.
      wl_saida_0170-dt_criacao     = wl_zaa001-dt_criacao.
      wl_saida_0170-hr_criacao     = wl_zaa001-hr_criacao.
      wl_saida_0170-user_criac     = wl_zaa001-user_criacao.
      wl_saida_0170-dt_modif       = wl_zaa001-dt_modif.
      wl_saida_0170-user_modif     = wl_zaa001-user_modif.
      wl_saida_0170-obs            = wl_zaa001-observacao.
      wl_saida_0170-dut            = wl_zaa001-dut.
      wl_saida_0170-alienacao      = wl_zaa001-alienacao.
      wl_saida_0170-porte_obrig    = wl_zaa001-porte_obrig.

      APPEND wl_saida_0170 TO gt_saida_0170.

      CLEAR: wl_saida_0170,
             wl_zaa001,
             wl_anlz.
    ENDLOOP.
  ENDMETHOD.                    "seleciona_dados_0160

  METHOD seleciona_dados_0180.
    CLEAR: tp_obrigacao, gt_saida_0180, gt_zaa003.

    IF ( s2_ipva IS NOT INITIAL ).
      tp_obrigacao = 'Ipva'.
    ELSEIF ( s2_dpvat IS NOT INITIAL ).
      tp_obrigacao = 'Dpvat'.
    ELSEIF ( s2_licen IS NOT INITIAL ).
      tp_obrigacao = 'Licenciamento'.
    ELSE.
      tp_obrigacao = 'Todos'.
    ENDIF.

    IF ( tp_obrigacao = 'Todos' ).
      SELECT *
        FROM zaa003
        INTO TABLE gt_zaa003
       WHERE bukrs    IN s2_bukrs
         AND werks    IN s2_werks
         AND anln1    IN s2_anln1
         AND kfzkz    IN s2_kfzkz
         AND mes_vcto IN s2_mes_v
         AND ano_vcto IN s2_ano_v.

    ELSE.
      SELECT *
        FROM zaa003
        INTO TABLE gt_zaa003
       WHERE bukrs    IN s2_bukrs
         AND werks    IN s2_werks
         AND anln1    IN s2_anln1
         AND kfzkz    IN s2_kfzkz
         AND mes_vcto IN s2_mes_v
         AND ano_vcto IN s2_ano_v
         AND tp_obrig EQ tp_obrigacao.
    ENDIF.

    LOOP AT gt_zaa003 INTO wl_zaa003.

      wl_saida_0180-bukrs       = wl_zaa003-bukrs.
      wl_saida_0180-anln1       = wl_zaa003-anln1.
      wl_saida_0180-anln2       = wl_zaa003-anln2.
      wl_saida_0180-kfzkz       = wl_zaa003-kfzkz.
      wl_saida_0180-werks       = wl_zaa003-werks.
      wl_saida_0180-tp_obrig    = wl_zaa003-tp_obrig.
      wl_saida_0180-ano_vcto    = wl_zaa003-ano_vcto.
      wl_saida_0180-mes_vcto    = wl_zaa003-mes_vcto.
      wl_saida_0180-waers       = wl_zaa003-waers.
      wl_saida_0180-vlr_princ   = wl_zaa003-vlr_princ.
      wl_saida_0180-vlr_corre   = wl_zaa003-vlr_corre.
      wl_saida_0180-vlr_multa   = wl_zaa003-vlr_multa.
      wl_saida_0180-vlr_juros   = wl_zaa003-vlr_juros.
      wl_saida_0180-vlr_tse     = wl_zaa003-vlr_tse.
      wl_saida_0180-vlr_total   = wl_zaa003-vlr_total.
      wl_saida_0180-cod_barras  = wl_zaa003-cod_barras.
      wl_saida_0180-dt_venc     = wl_zaa003-dt_venc.
      wl_saida_0180-dep_resp    = wl_zaa003-dep_resp.
      wl_saida_0180-erdat       = wl_zaa003-erdat.
      wl_saida_0180-ernam       = wl_zaa003-ernam.
      wl_saida_0180-lote        = wl_zaa003-lote.
      wl_saida_0180-cod_imposto = wl_zaa003-cod_imposto.
      wl_saida_0180-doc_imposto = wl_zaa003-doc_imposto.
      APPEND wl_saida_0180 TO gt_saida_0180.
      CLEAR wl_saida_0180.
    ENDLOOP.
  ENDMETHOD.                    "seleciona_dados_0180

  METHOD seleciona_dados_0200.
    DATA: r_utils TYPE REF TO zutils,
          cont    TYPE n.

    CONSTANTS:
      c_licenciamento TYPE char20 VALUE 'LICENCIAMENTO',
      c_ipva          TYPE char20 VALUE 'IPVA',
      c_dpvat         TYPE char20 VALUE 'DPVAT'.

    CREATE OBJECT r_utils.
    REFRESH gt_msg_return.

    IF ( wl_saida_0110-kfzkz IS NOT INITIAL ).
      CONDENSE wl_saida_0110-kfzkz NO-GAPS.
      at_cont = ( strlen( wl_saida_0110-kfzkz ) - 1 ).
    ENDIF.

    CALL METHOD obj_custom_editor->get_text_as_r3table
      IMPORTING
        table = gt_editor.

    cont = 1.
    WHILE ( cont <= 3 ).

      tp_obrigacao = cont. CONDENSE tp_obrigacao NO-GAPS.
      CLEAR wl_zaa002.

      SELECT SINGLE *
        FROM zaa002
        INTO wl_zaa002
       WHERE land1       = wl_zaa001-cod_pais
         AND rg_placa    = wl_zaa001-cod_regi
         AND final_placa = wl_saida_0110-kfzkz+at_cont(1)
         AND tp_obrig    = tp_obrigacao.

      CASE tp_obrigacao.
        WHEN 1.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e13
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_ipva.
          ENDIF.

        WHEN 2.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e14
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_dpvat.
          ENDIF.

        WHEN 3.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e15
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_licenc.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      cont = cont + 1.
    ENDWHILE.

    CHECK ( gt_msg_return IS NOT INITIAL ).
    r_utils->show_splitter_error( i_show  = x
                                  i_popup = 1 ).

**   PEGAR VENCIMENTO IPVA
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '1'.
*
*    if ( wl_zaa002-mes_vcto is initial ).
*
**     call method criar_mensagem_erro( text1 = msg_erro
**                                      text2 = space
**                                      field = 'ANO_VCTO'
**                                      index = v_index ).
*    endif.
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_ipva.
*    clear wl_zaa002.
*
**   PEGAR VENCIMENTO DPVAT
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '2'.
*
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_dpvat.
*    clear wl_zaa002.
*
**   PEGAR VENCIMENTO LICENCIAMENTO
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '3'.
*
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_licenc.
*    clear wl_zaa002.
  ENDMETHOD.                    "SELECIONA_DADOS_0200
ENDCLASS.                    "LCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.

*    ON_DOUBLE_CLICK_ALV FOR EVENT DOUBLE_CLICK OF CL_GUI_CONTAINER
*                        IMPORTING E_ROW E_COLUMN.

    CONSTANTS:
      tree_filho_01 TYPE n VALUE 2,
      tree_filho_02 TYPE n VALUE 4,
      tree_filho_03 TYPE n VALUE 6,
      tree_filho_04 TYPE n VALUE 7.
ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_rECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD  handle_double_click.
    CASE node_key.
      WHEN tree_filho_01.
        screen_principal = c_screen_0110.
      WHEN tree_filho_02.
        screen_principal = c_screen_0140.
      WHEN tree_filho_03.
        screen_principal = c_screen_0160.
        screen_item      = c_screen_0170.
      WHEN tree_filho_04.
        screen_principal = c_screen_0160.
        screen_item      = c_screen_0180.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

*  METHOD ON_DOUBLE_CLICK_ALV.
*
*    BREAK-POINT.
*
*  ENDMETHOD.                    "DOUBLE_CLICK_ALV
ENDCLASS.                    "LCL_TREE_EVENT_rECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

    CONSTANTS:
      c_btn_add_complemento  TYPE char20 VALUE 'BTN_ADD_COMPLEMENTO',
      c_btn_reset_lancamento TYPE char20 VALUE 'BTN_RESET_LANCAMENTO',
      c_btn_gerar_ctas_pgar  TYPE char20 VALUE 'BTN_GERAR_CTAS_PGAR',
      c_btn_salvar           TYPE char20 VALUE 'BTN_SALVAR',
      c_btn_atualizar        TYPE char20 VALUE 'BTN_ATUALIZAR'.

ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: ls_good   TYPE lvc_s_modi,
          r_utils   TYPE REF TO zutils,
          vlr_total TYPE dmbtr.

    CLEAR: wl_zaa001, return_status, gt_fields, lines,
           gt_fields_style, gt_estilo[]. "GT_TDLINE,


    SORT er_data_changed->mt_good_cells BY tabix DESCENDING.


    CREATE OBJECT r_utils.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      IF ( ls_good-fieldname(3) = 'VLR').
        DELETE gt_msg_return WHERE field = 'VLR_TOTAL'
                               AND aba   = screen_principal
                               AND tabix = ls_good-row_id.
      ELSE.
        DELETE gt_msg_return WHERE field = ls_good-fieldname
                               AND aba   = screen_principal
                               AND tabix = ls_good-row_id.
      ENDIF.

      CASE screen_principal.
*        WHEN 0110.
*          DELETE ER_DATA_CHANGED->MT_GOOD_CELLS WHERE VALUE NE 'X'.
*          DESCRIBE TABLE ER_DATA_CHANGED->MT_GOOD_CELLS LINES LINES.
*
*          READ TABLE GT_SAIDA_0110 INTO WL_SAIDA_0110 INDEX LS_GOOD-ROW_ID.
*          CASE LS_GOOD-FIELDNAME.
*            WHEN 'CHECK'.
*
**             Seleciona a descrição dos campos, para o cabeçalho
*              SELECT SINGLE *
*                FROM T001
*                INTO WL_T001
*               WHERE BUKRS EQ WL_SAIDA_0110-BUKRS.
*
*              SELECT SINGLE *
*                FROM J_1BBRANCH
*                INTO WL_J_1BBRANCH
*               WHERE BRANCH EQ WL_SAIDA_0110-FILIAL.
*
*              SELECT SINGLE *
*                FROM CSKT
*                INTO WL_CSKT
*               WHERE KOSTL EQ WL_SAIDA_0110-CENTRO.
*
*              MOVE:
*              WL_T001-BUTXT      TO WL_DESCR_HEADER-EMPRESA,
*              WL_J_1BBRANCH-NAME TO WL_DESCR_HEADER-FILIAL,
*              WL_CSKT-MCTXT      TO WL_DESCR_HEADER-CENTRO,
*
**             SUGERE PAIS DA EMPRESA NOS DADOS COMPLEMENTARES.
*
*              WL_T001-LAND1      TO WL_ZAA001-COD_PAIS.
**         -
*
**             Verifica se existe um registro de dados complementares
**             ja cadastrado, e carrega-os na tela.
*              SELECT SINGLE *
*                FROM ZAA001
*                INTO WL_ZAA001
*               WHERE ANLN1 EQ WL_SAIDA_0110-ANLN1
*                 AND ANLN2 EQ WL_SAIDA_0110-ANLN2.
**         --
*
*              IF SY-SUBRC IS INITIAL.
*                WL_TDLINE = WL_ZAA001-OBSERVACAO.
*                APPEND WL_TDLINE TO GT_TDLINE.
*
*                SHIFT WL_SAIDA_0110-ANLN1 LEFT DELETING LEADING '0'.
*                RETURN_STATUS = 1.
*
*                R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
*                                        GROUP2    = SPACE
*                                        VALUE     = '0'
*                                        INVISIBLE = '0').
*
*                R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
*                                        GROUP2    = SPACE
*                                        VALUE     = '0'
*                                        INVISIBLE = '0').
*              ENDIF.
*            WHEN OTHERS.
*          ENDCASE.

        WHEN 0140.
          CLEAR wl_saida_0140-vlr_total.

          READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX ls_good-row_id.
          CASE ls_good-fieldname.
            WHEN 'CHECK'.
              wl_saida_0140-check     = ls_good-value.

              IF ( ls_good-value IS INITIAL ).
                DELETE tl_selected_rows WHERE index = ls_good-row_id.
              ELSE.
                wl_selected_rows = ls_good-row_id.
                APPEND wl_selected_rows TO tl_selected_rows.
              ENDIF.

*              CALL METHOD OBJ_ALV_0140->SET_SELECTED_ROWS
*                EXPORTING
*                  IT_INDEX_ROWS = TL_SELECTED_ROWS.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING check.
            WHEN 'CONV_BANC'.
              CLEAR: wl_saida_0140-estilo, wl_zaa001, wl_zaa002.

              wl_saida_0140-conv_banc  = ls_good-value.
              at_strlen = ( strlen( wl_saida_0140-kfzkz ) - 1 ).

              CASE wl_saida_0140-tp_obrig.
                WHEN 'Ipva'.
                  tp_obrigacao = 1.
                WHEN 'Dpvat'.
                  tp_obrigacao = 2.
                WHEN 'Licenciamento'.
                  tp_obrigacao = 3.
                WHEN OTHERS.
              ENDCASE.

              CONDENSE tp_obrigacao NO-GAPS.

              SELECT SINGLE *
                FROM zaa001
                INTO wl_zaa001
               WHERE bukrs = wl_saida_0140-bukrs
                 AND anln1 = wl_saida_0140-anln1
                 AND kfzkz = wl_saida_0140-kfzkz.

              SELECT SINGLE *
                FROM zaa002
                INTO wl_zaa002
               WHERE land1       = wl_zaa001-cod_pais
                 AND rg_placa    = wl_zaa001-cod_regi
                 AND final_placa = wl_zaa001-kfzkz+at_strlen(1)
                 AND mes_vcto    = wl_saida_0140-mes_vcto
                 AND tp_obrig    = tp_obrigacao.

              IF ( ls_good-value EQ 'X' ).
                wl_saida_0140-cod_imposto = wl_zaa002-conv_banc.
              ELSE.
                wl_saida_0140-cod_imposto = wl_zaa002-cod_imposto.
              ENDIF.

              SELECT *
                FROM zimp_cad_imp_con
                INTO TABLE gt_zimp_cad_imp_con
               WHERE cod_imposto = wl_saida_0140-cod_imposto.

              SORT gt_zimp_cad_imp_con BY cod_abertura.
              LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

                CASE wl_zimp_cad_imp_con-cod_abertura.
                  WHEN 01. "Principal
                    wl_fields_style-fieldname = 'VLR_PRINC'.
                  WHEN 02. "Correção
                    wl_fields_style-fieldname = 'VLR_CORRE'.
                  WHEN 04. "Multas
                    wl_fields_style-fieldname = 'VLR_MULTA'.
                  WHEN 05. "Juros
                    wl_fields_style-fieldname = 'VLR_JUROS'.
                  WHEN 06. "Tse
                    wl_fields_style-fieldname = 'VLR_TSE'.
                  WHEN OTHERS.
                    EXIT.
                ENDCASE.

                APPEND wl_fields_style TO gt_fields_style.
                CLEAR wl_fields_style.
              ENDLOOP.

              SORT gt_fields_style BY fieldname.
              LOOP AT gt_fields_style INTO wl_fields_style.
                r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                               style     = cl_gui_alv_grid=>mc_style_enabled ).
              ENDLOOP.

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id.

            WHEN 'COD_IMPOSTO'.
              wl_saida_0140-cod_imposto = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING cod_imposto.

            WHEN 'COD_BARRAS'.
              wl_saida_0140-cod_barras = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING cod_barras.
            WHEN 'DEP_RESP'.
              wl_saida_0140-dep_resp  = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING dep_resp.

            WHEN 'DT_VENC'.
              wl_saida_0140-dt_venc  = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING dt_venc.
            WHEN 'VLR_PRINC'.

              wl_saida_0140-vlr_princ = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

*              SELECT *
*                FROM ZIMP_CAD_IMP_CON
*                INTO TABLE GT_ZIMP_CAD_IMP_CON
*               WHERE COD_IMPOSTO = WL_SAIDA_0140-COD_IMPOSTO.

              wl_saida_0140_aux-cod_imposto = wl_saida_0140-cod_imposto.
              wl_saida_0140_aux-vlr_total   = wl_saida_0140-vlr_total.
              wl_saida_0140_aux-vlr_princ   = wl_saida_0140-vlr_princ.
              wl_saida_0140_aux-bukrs       = wl_saida_0140-bukrs.
              wl_saida_0140_aux-anln1       = wl_saida_0140-anln1.
              wl_saida_0140_aux-doc_imposto = wl_saida_0140-doc_imposto.


              APPEND wl_saida_0140_aux  TO gt_saida_0140_aux .


              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_princ vlr_total.


*              SORT GT_ZIMP_CAD_IMP_CON BY COD_ABERTURA.
*              LOOP AT GT_ZIMP_CAD_IMP_CON INTO WL_ZIMP_CAD_IMP_CON.
*
**              IF WL_ZIMP_CAD_IMP_CON-COD_ABERTURA = 01.
**
**                UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WL_SAIDA_0140-VLR_TOTAL
**                 WHERE BUKRS EQ WL_SAIDA_0140-BUKRS
**                 AND   COD_ABERTURA EQ WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
**
**                UPDATE  ZAA003 SET  VLR_PRINC = WL_SAIDA_0140-VLR_PRINC
**                                    VLR_TOTAL = WL_SAIDA_0140-VLR_TOTAL
**
**                 WHERE BUKRS       = WL_SAIDA_0140-BUKRS
**                 AND   ANLN1       = WL_SAIDA_0140-ANLN1
**                 AND   DOC_IMPOSTO = WL_SAIDA_0140-DOC_IMPOSTO.
**
**              ENDIF.
*
*                IF WL_ZIMP_CAD_IMP_CON-COD_ABERTURA = 11.
*                  VLR_TOTAL = WL_SAIDA_0140-VLR_TOTAL  * - 1.
*                  UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = VLR_TOTAL
*                  WHERE BUKRS EQ WL_SAIDA_0140-BUKRS
*                  AND   COD_ABERTURA EQ WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
*                ENDIF.
*              ENDLOOP.



            WHEN 'VLR_CORRE'.
              wl_saida_0140-vlr_corre = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_corre vlr_total.

            WHEN 'VLR_MULTA'.
              wl_saida_0140-vlr_multa = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_multa vlr_total.

            WHEN 'VLR_JUROS'.
              wl_saida_0140-vlr_juros = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_juros vlr_total.

            WHEN 'VLR_TSE'.
              wl_saida_0140-vlr_tse = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_tse vlr_total.
            WHEN 'FILIAL_PG'.

              wl_saida_0140-filial_pg = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING filial_pg.

            WHEN 'KOSTL_PG'.

              wl_saida_0140-kostl_pg = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING kostl_pg.

            WHEN OTHERS.
          ENDCASE.

          r_utils->show_splitter_error( i_show  = space
                                        i_popup = space ).

          IF ( ls_good-fieldname NE 'CHECK' ).
            CALL METHOD obj_alv_0140->refresh_table_display
              EXPORTING
                is_stable = wl_stable.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    "SORT ER_DATA_CHANGED->MT_GOOD_CELLS BY TABIX DESCENDING.
    CLEAR: ls_good, er_data_changed->mt_good_cells.
  ENDMETHOD.                    "DATA_CHANGED_0110

  METHOD on_data_changed_finished.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
*       I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_click.
    DATA: r_utils             TYPE REF TO zutils,
          txt_erro            TYPE itex132,
          at_objkey           TYPE awkey,
          wa_zimp_lanc_impost TYPE zimp_lanc_impost.

    DATA: obj_dados TYPE REF TO lcl_seleciona_dados.
    CREATE OBJECT obj_dados.

    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX e_row_id.

    CASE e_column_id.
      WHEN 'LOTE'.
        CHECK ( wl_saida_0140-lote IS NOT INITIAL ).

        PERFORM f_preencher_dynpro USING:
        'X' 'ZIMP54'     '1000',
        ' ' 'P_BUKRS'     wl_saida_0140-bukrs,
        ' ' 'P_LOTE'      wl_saida_0140-lote,
        ' ' 'BDC_OKCODE' '/00',
        ' ' 'BDC_OKCODE' '=ONLI'.

        opt-dismode  = 'E'.
        CALL TRANSACTION 'ZIMP54' USING gt_bdc OPTIONS FROM opt.

      WHEN 'DOC_IMPOSTO'.
        CHECK ( wl_saida_0140-doc_imposto IS NOT INITIAL ).

        SET PARAMETER ID 'BUK' FIELD wl_saida_0140-bukrs.
        SET PARAMETER ID 'BLN' FIELD wl_saida_0140-doc_imposto.
        CALL TRANSACTION 'ZIMP53' AND SKIP FIRST SCREEN.

        SELECT SINGLE *
          INTO wa_zimp_lanc_impost
          FROM zimp_lanc_impost
          WHERE doc_imposto EQ wl_saida_0140-doc_imposto
            AND bukrs EQ wl_saida_0140-bukrs
            AND loekz EQ 'X'.

        IF sy-subrc IS INITIAL.
          DELETE FROM zaa003
              WHERE bukrs EQ wl_saida_0140-bukrs
                AND anln1 EQ wl_saida_0140-anln1
                AND anln2 EQ wl_saida_0140-anln2
                AND kfzkz EQ wl_saida_0140-kfzkz
                AND werks EQ wl_saida_0140-werks
                AND tp_obrig EQ wl_saida_0140-tp_obrig
                AND ano_vcto EQ wl_saida_0140-ano_vcto
                AND mes_vcto EQ wl_saida_0140-mes_vcto
                AND doc_imposto EQ wl_saida_0140-doc_imposto.

          obj_dados->seleciona_dados_0140( ).
          CALL METHOD obj_alv_0140->refresh_table_display.

        ENDIF.

      WHEN 'DOC_CONTABIL'.
        CHECK ( wl_saida_0140-doc_contabil IS NOT INITIAL ).

        SET PARAMETER ID 'BLN' FIELD wl_saida_0140-doc_contabil.
        SET PARAMETER ID 'BUK' FIELD wl_saida_0140-bukrs.
        SET PARAMETER ID 'GJR' FIELD wl_saida_0140-ano_vcto.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN 'STATUS'.
        CHECK ( wl_saida_0140-status = icon_red_light ).
        CREATE OBJECT r_utils.

        CLEAR gt_saida_0300.

*       Verifica se o lançamento retornou erro, e busca o mesmo na ZIB
*       em seguida grava a mensagem junto do index na tabela de saída, para que o erro possa
*       ser tratado futuramente.

        r_utils->buscar_status_zib( EXPORTING
                                    i_bukrs       = wl_saida_0140-bukrs
                                    i_doc_imposto = wl_saida_0140-doc_imposto
                                    i_gjahr       = wl_saida_0140-ano_vcto
                                    IMPORTING
                                    e_zibchv      = wl_zib_chave
                                    e_ziberr      = wl_zib_erro ).

        wl_saida_0300-status     = icon_led_red.
        wl_saida_0300-msg_erro   = wl_zib_erro-message.
        wl_saida_0300-index_erro = e_row_id.
        APPEND wl_saida_0300 TO gt_saida_0300.

        CALL SCREEN 0300 STARTING AT 5 10
                         ENDING AT 78 14.

        CLEAR: wl_mensagem, gt_msg_return.
    ENDCASE.
  ENDMETHOD.                    "ON_CLICK

  METHOD constructor.
    CLEAR: obj_toolbar_manager.
    CREATE OBJECT obj_toolbar_manager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD set_toolbar.
    DATA wl_toolbar TYPE stb_button.

    CLEAR: wl_toolbar.

    CASE sy-dynnr.
      WHEN 0300.
        wl_toolbar-function     = c_btn_reset_lancamento.
        wl_toolbar-icon         = icon_modification_reset.
        wl_toolbar-quickinfo    = space.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Reiniciar Lançamento'.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

      WHEN OTHERS.

        CASE screen_principal.
          WHEN 0110.
            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


            wl_toolbar-function     = c_btn_add_complemento.
            wl_toolbar-icon         = icon_car.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Dados Complementares'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.

          WHEN 0140.
            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-function     = c_btn_gerar_ctas_pgar.
            wl_toolbar-icon         = icon_pm_insert.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Gerar Contas a Pagar →'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-function     = c_btn_atualizar.
            wl_toolbar-icon         = icon_refresh.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Atualizar'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


            wl_toolbar-function     = c_btn_salvar.
            wl_toolbar-icon         = icon_system_save.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Atualizar Valor'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.
        ENDCASE.
    ENDCASE.

  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.

    DATA: r_utils           TYPE REF TO zutils,
          r_contas_pagar    TYPE REF TO gerar_contas_pagar,
          r_tipo_operacao   TYPE REF TO lcl_tipo_operacao,
          r_seleciona_dados TYPE REF TO lcl_seleciona_dados,
          i_qtd_str         TYPE i,
          lc_observacao	    TYPE zaa001-observacao,
          wa_linha          TYPE ty_editor,
          vlr_total         TYPE dmbtr.


    CREATE OBJECT: r_seleciona_dados,
                   r_contas_pagar,
                   r_tipo_operacao,
                   r_utils.


    CASE e_ucomm.
      WHEN c_btn_add_complemento.
        CLEAR: tl_selected_rows, wl_zaa001, gt_fields, return_status,
               gt_code, gt_editor.

        CALL METHOD obj_alv_0110->get_selected_rows
          IMPORTING
            et_index_rows = tl_selected_rows.

        DESCRIBE TABLE tl_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e02 TYPE 'I' DISPLAY LIKE 'E'.
        ELSEIF ( lines >= 2 ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.

          LOOP AT tl_selected_rows INTO wl_selected_rows.
            READ TABLE gt_saida_0110 INTO wl_saida_0110 INDEX wl_selected_rows-index.

*           Seleciona a descrição dos campos, para o cabeçalho
            SELECT SINGLE *
              FROM t001
              INTO wl_t001
             WHERE bukrs EQ wl_saida_0110-bukrs.

            SELECT SINGLE *
              FROM j_1bbranch
              INTO wl_j_1bbranch
             WHERE branch EQ wl_saida_0110-filial.

            SELECT SINGLE *
              FROM cskt
              INTO wl_cskt
             WHERE kostl EQ wl_saida_0110-centro.

            MOVE:
            wl_t001-butxt      TO wl_descr_header-empresa,
            wl_j_1bbranch-name TO wl_descr_header-filial,
            wl_cskt-mctxt      TO wl_descr_header-centro,

*           Sugere país da empresa nos dados complementares.
            wl_t001-land1      TO wl_zaa001-cod_pais.

*           Verifica se existe um registro de dados complementares
*           ja cadastrado, e carrega-os na tela.
            SELECT SINGLE *
              FROM zaa001
              INTO wl_zaa001
             WHERE bukrs EQ wl_saida_0110-bukrs
               AND anln1 EQ wl_saida_0110-anln1.
            "AND KFZKZ EQ WL_SAIDA_0110-KFZKZ.
*         --

            IF sy-subrc IS INITIAL.
              APPEND wl_zaa001-observacao TO gt_editor.
              return_status = 1.

              r_utils->tratar_campos( group1    = 'GR1'
                                      group2    = space
                                      value     = '0'
                                      invisible = '0').

              r_utils->tratar_campos( group1    = 'GR2'
                                      group2    = space
                                      value     = '0'
                                      invisible = '0').
            ELSE.
              APPEND c_edit TO gt_code.
            ENDIF.

          ENDLOOP.

          r_utils->show_splitter_error( i_show  = space
                                        i_popup = space ).

          CALL SCREEN 0200 STARTING AT 01 01
                             ENDING AT 95 25.
        ENDIF.

      WHEN c_btn_gerar_ctas_pgar.
        DATA:
          at_descr_lote TYPE char100,
          at_mes_pgto   TYPE char100,
          at_num_lote   TYPE numc10,
          at_dt_venc    TYPE erdat,
          at_num_doc    TYPE numc10,
          at_index      TYPE sy-tabix.

        REFRESH gt_msg_return.

        r_utils->validar_screen_0140( ).

        CHECK ( gt_msg_return IS INITIAL ).

        LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = x.
          at_index = sy-tabix.

          CLEAR: at_descr_lote, at_mes_pgto,
                 wl_saida_0140-estilo,
                 gt_estilo[].

          CONCATENATE wl_saida_0140-mes_vcto '.' wl_saida_0140-ano_vcto INTO at_mes_pgto.
          CONCATENATE TEXT-i01 at_mes_pgto INTO at_descr_lote SEPARATED BY space .

*                              ___________________________
*_____________________________/SELECIONA OS DADOS IMPOSTOS\__________________________


          r_contas_pagar->seleciona_imposto(
                                     EXPORTING
                                     i_cod_imposto = wl_saida_0140-cod_imposto ).

*                              _______________________
*_____________________________/CRIAR LOTE DE PAGAMENTO\_____________________________

          IF ( at_num_lote IS INITIAL
          OR   at_dt_venc  NE wl_saida_0140-dt_venc ).

            r_contas_pagar->gera_lote( EXPORTING
                                       i_bukrs = wl_saida_0140-bukrs
                                       i_descr = at_descr_lote
                                       i_depto = wl_saida_0140-dep_resp
                                       i_venci = wl_saida_0140-dt_venc
                                       IMPORTING
                                       e_num_lote = at_num_lote
                                       e_dt_venci = at_dt_venc ).
          ENDIF.

*                              _________________________
*_____________________________/CRIA DOCUMENTO DE IMPOSTO\__________________________

          IF wl_saida_0140-kostl_pg IS NOT INITIAL.
            DATA(_kostl) =  wl_saida_0140-kostl_pg.
          ELSE.
            _kostl = wl_saida_0140-kostl.
          ENDIF.

          IF wl_saida_0140-filial_pg IS NOT INITIAL.
            DATA(_filial) = wl_saida_0140-filial_pg.
          ELSE.
            _filial = wl_saida_0140-werks.
          ENDIF.

          r_contas_pagar->gera_doc_imposto(
                                     EXPORTING
                                     i_num_lote  = at_num_lote
                                     i_bukrs     = wl_saida_0140-bukrs
                                     i_dt_vcto   = wl_saida_0140-dt_venc
                                     i_mes_vcto  = wl_saida_0140-mes_vcto
                                     i_ano_vcto  = wl_saida_0140-ano_vcto
                                     i_placa     = wl_saida_0140-kfzkz
                                     i_filial    = _filial
                                     i_kostl     = _kostl
                                     i_c_imposto = wl_saida_0140-cod_imposto
                                     i_c_barras  = wl_saida_0140-cod_barras
                                     i_lifnr     = wl_saida_0140-lifnr
                                     i_vlr_princ = wl_saida_0140-vlr_princ
                                     i_vlr_corre = wl_saida_0140-vlr_corre
                                     i_vlr_multa = wl_saida_0140-vlr_multa
                                     i_vlr_juros = wl_saida_0140-vlr_juros
                                     i_vlr_tse   = wl_saida_0140-vlr_tse
                                     i_vlr_total = wl_saida_0140-vlr_total
                                     IMPORTING
                                     e_num_docu = at_num_doc  ).


          wl_saida_0140-check       = space.
          wl_saida_0140-status      = icon_yellow_light.
          wl_saida_0140-erdat       = sy-datum.
          wl_saida_0140-ernam       = sy-uname.
          wl_saida_0140-lote        = at_num_lote.
          wl_saida_0140-doc_imposto = at_num_doc.

          r_utils->z_style_disable_edit( fieldname = 'ANO_VCTO'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'COD_IMPOSTO'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'CONV_BANC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'DEP_RESP'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'FILIAL_PG'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'KOSTL_PG'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_CORRE'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_JUROS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_MULTA'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_PRINC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_TSE'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'WAERS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX at_index.

          MOVE-CORRESPONDING wl_saida_0140 TO wl_zaa003.
          INSERT zaa003 FROM wl_zaa003.
          COMMIT WORK.

        ENDLOOP.

        IF sy-subrc IS INITIAL.
          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable = wl_stable_aux.
        ELSE.
          MESSAGE TEXT-e07 TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN c_btn_reset_lancamento.
        LOOP AT gt_saida_0300 INTO wl_saida_0300.
          READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX wl_saida_0300-index_erro.

          CLEAR: gt_estilo[], gt_fields_style, wl_saida_0140-estilo.

          r_utils->buscar_status_zib( EXPORTING
                                      i_bukrs       = wl_saida_0140-bukrs
                                      i_doc_imposto = wl_saida_0140-doc_imposto
                                      i_gjahr       = wl_saida_0140-ano_vcto
                                      IMPORTING
                                      e_zibchv      = wl_zib_chave
                                      e_ziberr      = wl_zib_erro ).

          DELETE FROM zib_contabil_err WHERE obj_key     = wl_zib_erro-obj_key.
          DELETE FROM zib_contabil     WHERE obj_key     = wl_zib_erro-obj_key.

          DELETE FROM zimp_lanc_impost WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.

          DELETE FROM zimp_lanc_imp_ct WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.

          DELETE FROM zimp_cad_lote    WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   lote        = wl_saida_0140-lote.

          DELETE FROM zaa003           WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   anln1       = wl_saida_0140-anln1
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.
          COMMIT WORK.

          wl_saida_0140-status       = icon_light_out.
          wl_saida_0140-doc_imposto  = ''.
          wl_saida_0140-lote         = ''.

*         Seleciona o imposto referênte ao tipo de obrigação, para definir quais
*         campos devem ser habilitados para o usuário.

          SELECT *
            FROM zimp_cad_imp_con
            INTO TABLE gt_zimp_cad_imp_con
           WHERE cod_imposto = wl_saida_0140-cod_imposto.

          SORT gt_zimp_cad_imp_con BY cod_abertura.
          LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.
            CASE wl_zimp_cad_imp_con-cod_abertura.
              WHEN 01. wl_fields_style-fieldname = 'VLR_PRINC'.
              WHEN 02. wl_fields_style-fieldname = 'VLR_CORRE'.
              WHEN 04. wl_fields_style-fieldname = 'VLR_MULTA'.
              WHEN 05. wl_fields_style-fieldname = 'VLR_JUROS'.
              WHEN 06. wl_fields_style-fieldname = 'VLR_TSE'.
              WHEN OTHERS.
                EXIT.
            ENDCASE.
            APPEND wl_fields_style TO gt_fields_style.
          ENDLOOP.

          wl_fields_style-fieldname = 'ANO_VCTO'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'CHECK'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'COD_BARRAS'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'COD_IMPOSTO'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'CONV_BANC'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'DEP_RESP'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'DT_VENC'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'WAERS'.
          APPEND wl_fields_style TO gt_fields_style.

*         Primeiro eu coloco os campos em uma tabela e ordeno-a, e depois habilito-os
*         usando um método que pega o campo ordenado e coloca na tabela GT_ESTILO.

          SORT gt_fields_style BY fieldname.
          LOOP AT gt_fields_style INTO wl_fields_style.
            r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                           style     = cl_gui_alv_grid=>mc_style_enabled ).
          ENDLOOP.

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX wl_saida_0300-index_erro.
        ENDLOOP.

        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

        LEAVE TO SCREEN 0.

      WHEN c_btn_atualizar.
        DATA: wl_zimp_cad_lote TYPE zimp_cad_lote.

        LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE status = icon_yellow_light
                                                    OR status = icon_green_light.
          at_index = sy-tabix.

          CASE wl_saida_0140-status.
            WHEN icon_yellow_light.
*                                     ________________________________
*            ________________________/VERIFICA SE O DOC FOI PROCESSADO\_________________________

              SELECT SINGLE *
                FROM zimp_cad_lote
                INTO wl_zimp_cad_lote
               WHERE bukrs = wl_saida_0140-bukrs
                 AND lote = wl_saida_0140-lote.

*                 Verifica se o lote foi aprovado para buscar o doc contábil (belnr).
              CHECK ( wl_zimp_cad_lote-status_lote = 'A' ).
              r_utils->buscar_status_zib( EXPORTING
                                          i_bukrs       = wl_saida_0140-bukrs
                                          i_doc_imposto = wl_saida_0140-doc_imposto
                                          i_gjahr       = wl_saida_0140-ano_vcto
                                          IMPORTING
                                          e_zibchv      = wl_zib_chave
                                          e_ziberr      = wl_zib_erro ).

              IF ( wl_zib_chave IS NOT INITIAL ).
                wl_saida_0140-status       = icon_green_light.
                wl_saida_0140-doc_contabil = wl_zib_chave-belnr.
              ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                wl_saida_0140-status = icon_red_light.
              ENDIF.
*                                     _______________________________
*            ________________________/VERIFICA SE O DOC FOI ESTORNADO\_________________________

*            WHEN ICON_GREEN_LIGHT.
*              CLEAR: GT_ESTILO[], GT_FIELDS_STYLE, WL_SAIDA_0140-ESTILO, GT_ZIMP_CAD_IMP_CON.
*
*              SELECT SINGLE *
*                FROM ZIMP_LANC_IMPOST
*                INTO WL_ZIMP_LANC_IMPOST
*               WHERE BUKRS       EQ WL_SAIDA_0140-BUKRS
*                 AND DOC_IMPOSTO EQ WL_SAIDA_0140-DOC_IMPOSTO.
*
**             Verifica se o documento foi marcado para eliminação e deleta o documento
**             da tabela de Impostos para o mesmo poder ser gerado novamente.
**
**             Obs.: O processo para marcar p/ exclusão é feito via (ZIMP53).
*
*              IF ( WL_ZIMP_LANC_IMPOST-LOEKZ = 'X' ).
*                DELETE FROM ZAA003 WHERE BUKRS       = WL_SAIDA_0140-BUKRS
*                                     AND ANLN1       = WL_SAIDA_0140-ANLN1
*                                     AND DOC_IMPOSTO = WL_SAIDA_0140-DOC_IMPOSTO.
*                COMMIT WORK.
*
*                WL_SAIDA_0140-STATUS       = ICON_LIGHT_OUT.
*                WL_SAIDA_0140-DOC_IMPOSTO  = ''.
*                WL_SAIDA_0140-DOC_CONTABIL = ''.
*                WL_SAIDA_0140-LOTE         = ''.
*
**               Seleciona o imposto referênte ao tipo de obrigação, para definir quais
**               campos devem ser habilitados para o usuário.
*
*                SELECT *
*                  FROM ZIMP_CAD_IMP_CON
*                  INTO TABLE GT_ZIMP_CAD_IMP_CON
*                 WHERE COD_IMPOSTO = WL_SAIDA_0140-COD_IMPOSTO.
*
*                SORT GT_ZIMP_CAD_IMP_CON BY COD_ABERTURA.
*                LOOP AT GT_ZIMP_CAD_IMP_CON INTO WL_ZIMP_CAD_IMP_CON.
*                  CASE WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
*                    WHEN 01. WL_FIELDS_STYLE-FIELDNAME = 'VLR_PRINC'.
*                    WHEN 02. WL_FIELDS_STYLE-FIELDNAME = 'VLR_CORRE'.
*                    WHEN 04. WL_FIELDS_STYLE-FIELDNAME = 'VLR_MULTA'.
*                    WHEN 05. WL_FIELDS_STYLE-FIELDNAME = 'VLR_JUROS'.
*                    WHEN 06. WL_FIELDS_STYLE-FIELDNAME = 'VLR_TSE'.
*                    WHEN OTHERS.
*                      EXIT.
*                  ENDCASE.
*                  APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                ENDLOOP.
*
*                WL_FIELDS_STYLE-FIELDNAME = 'ANO_VCTO'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'CHECK'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'COD_BARRAS'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'COD_IMPOSTO'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'CONV_BANC'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'DEP_RESP'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'DT_VENC'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'WAERS'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*
**               Primeiro eu coloco os campos em uma tabela e ordeno-a, e depois habilito-os
**               usando um método que pega o campo ordenado e coloca na tabela GT_ESTILO.
*
*                SORT GT_FIELDS_STYLE BY FIELDNAME.
*                LOOP AT GT_FIELDS_STYLE INTO WL_FIELDS_STYLE.
*                  R_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = WL_FIELDS_STYLE-FIELDNAME
*                                                 STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED ).
*                ENDLOOP.
*
*                INSERT LINES OF GT_ESTILO INTO TABLE WL_SAIDA_0140-ESTILO.
*              ENDIF.
          ENDCASE.

          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX at_index.
        ENDLOOP.

        IF ( sy-subrc IS INITIAL ).
          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable = wl_stable.
        ENDIF.


      WHEN c_btn_salvar.

        LOOP AT gt_saida_0140_aux INTO wl_saida_0140_aux.

          SELECT *
            FROM zimp_cad_imp_con
            INTO TABLE gt_zimp_cad_imp_con
           WHERE cod_imposto = wl_saida_0140_aux-cod_imposto.


          SORT gt_zimp_cad_imp_con BY cod_abertura.
          LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

            UPDATE  zaa003 SET  vlr_princ = wl_saida_0140_aux-vlr_princ
                                vlr_total = wl_saida_0140_aux-vlr_total

             WHERE bukrs       = wl_saida_0140_aux-bukrs
             AND   anln1       = wl_saida_0140_aux-anln1
             AND   doc_imposto = wl_saida_0140_aux-doc_imposto.



            IF wl_zimp_cad_imp_con-cod_abertura = 01.

              UPDATE zimp_lanc_imp_ct SET valor_imp = wl_saida_0140_aux-vlr_total
               WHERE bukrs EQ wl_saida_0140_aux-bukrs
               AND   cod_abertura EQ wl_zimp_cad_imp_con-cod_abertura
               AND   doc_imposto  EQ wl_saida_0140_aux-doc_imposto.
            ENDIF.

            IF wl_zimp_cad_imp_con-cod_abertura = 11.
              vlr_total = wl_saida_0140_aux-vlr_total  * - 1.

              UPDATE zimp_lanc_imp_ct SET valor_imp = vlr_total
              WHERE bukrs EQ wl_saida_0140-bukrs
              AND   cod_abertura EQ wl_zimp_cad_imp_con-cod_abertura
              AND   doc_imposto  EQ wl_saida_0140_aux-doc_imposto.

            ENDIF.
          ENDLOOP.

          CLEAR wl_saida_0140_aux.

        ENDLOOP.

        MESSAGE TEXT-s02 TYPE 'I'.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM

  METHOD handle_user_command.

  ENDMETHOD.                    "user_command

  METHOD on_onf4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,
             fieldname  TYPE dd03l-fieldname,
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50)  ,
          END OF wl_valuetab,

          gt_field         TYPE TABLE OF ty_field,
          gt_value         TYPE TABLE OF ty_value,
          gt_valuetab      LIKE TABLE OF wl_valuetab,
          gt_t500w         TYPE TABLE OF t500w,
          gt_depto         TYPE TABLE OF zimp_cad_depto,

          wl_value         TYPE ty_value,
          wl_field         TYPE ty_field,
          wl_index         TYPE sy-tabix,
          wl_t500w         TYPE t500w,
          wl_depto         TYPE zimp_cad_depto,

          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

*--------------------------------------------------------------------------------------------------*
* SET ONF4                                                                                         *
*--------------------------------------------------------------------------------------------------*
    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'MOEDA'.

        SELECT *
          FROM t500w
          INTO TABLE gt_t500w.

        wl_fieldname = 'WAERS'.
        wl_tabname   = 'T500W'.

        LOOP AT gt_t500w INTO wl_t500w.
          MOVE wl_t500w-land1 TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.

          MOVE wl_t500w-waers TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.
        ENDLOOP.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'LAND1'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'WAERS'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

      WHEN 'DEP_RESP'.
        SELECT *
          FROM zimp_cad_depto
          INTO TABLE gt_depto.

        wl_fieldname = 'DEP_RESP'.
        wl_tabname   = 'ZIMP_CAD_DEPTO'.

        LOOP AT gt_depto INTO wl_depto.
          MOVE wl_depto-dep_resp TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.

          MOVE wl_depto-dep_resp_desc TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.
        ENDLOOP.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'DEP_RESP'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'DEP_RESP_DESC'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        cucol                     = '10'
        curow                     = '5'
        fieldname                 = wl_fieldname
        tabname                   = wl_tabname
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = gt_field
        select_values             = gt_value
        valuetab                  = gt_valuetab
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

*--------------------------------------------------------------------------------------------------*
* GET ONF4                                                                                         *
*--------------------------------------------------------------------------------------------------*
    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'MOEDA'.
        READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'WAERS'.
        MOVE wl_value-char79 TO wl_saida_0140-waers.

        MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX es_row_no-row_id
          TRANSPORTING waers.

      WHEN 'DEP_RESP'.
        READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'DEP_RESP'.
        MOVE wl_value-char79 TO wl_saida_0140-dep_resp.

        MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX es_row_no-row_id
          TRANSPORTING dep_resp.
      WHEN OTHERS.
    ENDCASE.

    CALL METHOD obj_alv_0140->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "ON_ONF4
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS ZUTILS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ZUTILS_ver DEFINITION.
  PUBLIC SECTION.
    METHODS tratar_campos IMPORTING
                            group1    TYPE char3
                            group2    TYPE char3
                            value     TYPE char1
                            invisible TYPE char1.

    METHODS checar_dia_util IMPORTING
                              i_data TYPE erdat
                            EXPORTING
                              e_data TYPE erdat.

    METHODS z_style_disable_edit IMPORTING
                                   fieldname TYPE any
                                   style     TYPE any.

    METHODS buscar_status_zib IMPORTING
                                i_doc_imposto TYPE zdoc_imposto
                                i_bukrs       TYPE bukrs
                                i_gjahr       TYPE gjahr
                              EXPORTING
                                e_zibchv      TYPE zib_contabil_chv
                                e_ziberr      TYPE zib_contabil_err.

    DATA: at_objkey TYPE awkey.


    METHODS show_splitter_error IMPORTING
                                  i_show  TYPE c
                                  i_popup TYPE i.


    METHODS criar_mensagem_erro IMPORTING
                                  text1 TYPE bapi_msg
                                  text2 TYPE itex132
                                  field TYPE char30
                                  index TYPE sy-tabix.

    METHODS validar_screen_0110.
    METHODS validar_screen_0140.

    METHODS criar_mathcode.
ENDCLASS.                    "ZUTILS DEFINITION

*----------------------------------------------------------------------*
*       CLASS ZUTILS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ZUTILS_ver IMPLEMENTATION.
  METHOD tratar_campos.

    wl_fields-group1    = group1.
    wl_fields-group2    = group2.
    wl_fields-value     = value.
    wl_fields-invisible = invisible.

    APPEND wl_fields TO gt_fields.
  ENDMETHOD.                    "Z_TRATAR_CAMPOS

  METHOD checar_dia_util.
    DATA: gt_holidays TYPE TABLE OF iscal_day,
          wl_holidays TYPE iscal_day,
          at_day.

    e_data      = i_data.
*    E_DATA+6(2) = 10.

    DO 3 TIMES.
      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          holiday_calendar = 'BR'
          date_from        = sy-datum
          date_to          = e_data
        TABLES
          holidays         = gt_holidays.

      READ TABLE gt_holidays INTO wl_holidays WITH KEY date = e_data.
      IF ( sy-subrc IS INITIAL ).
        e_data = e_data + 1.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = e_data
      IMPORTING
        day  = at_day.

    IF ( at_day = 6 ).
      e_data = e_data + 2.

    ELSEIF at_day = 7.
      e_data = e_data + 1.
    ENDIF.
  ENDMETHOD.                    "CHECAR_DIA_UTIL

  METHOD z_style_disable_edit.

    wl_estilo-fieldname = fieldname.
    wl_estilo-style     = style.

    APPEND wl_estilo TO gt_estilo.
  ENDMETHOD.                    "Z_STYLE_DISABLE_EDIT

  METHOD buscar_status_zib.
    CLEAR: at_objkey, e_zibchv, e_ziberr.

    CONCATENATE 'ZP' i_bukrs i_doc_imposto i_gjahr
    INTO at_objkey.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO e_zibchv
     WHERE obj_key EQ at_objkey.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO e_ziberr
       WHERE obj_key EQ at_objkey
         AND nr_item EQ '1'.
    ENDIF.

  ENDMETHOD.                    "buscar_status_zib

  METHOD show_splitter_error.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = i_show
        i_popup    = i_popup
        i_repid    = sy-repid
        i_set_cell = 'WL_CELL'
        i_set_obj  = 'WL_OBJ'
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.
  ENDMETHOD.                    "Z_SHOW_SPLITTER

  METHOD criar_mensagem_erro.

    wl_msg_return-field = field.
    wl_msg_return-tabix = index.
    wl_msg_return-aba   = screen_principal.

    CONCATENATE icon_message_error_small text1 text2
           INTO wl_msg_return-msg SEPARATED BY space.

    APPEND wl_msg_return TO gt_msg_return.
    CLEAR wl_msg_return.
  ENDMETHOD.                    "Z_CRIAR_MENSAGEM_ERRO

  METHOD validar_screen_0110.
    SELECT SINGLE *
      FROM j_1bbranch
      INTO wl_j_1bbranch
     WHERE bukrs  = s_bukrs
       AND branch = s_werks.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) WITH TEXT-e05 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "VALIDAR_SCREEN_0110

  METHOD validar_screen_0140.
    DATA: msg_erro    TYPE bapi_msg,
          imobilizado TYPE anln1,
          v_index     TYPE sy-tabix.

    LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = x.
      v_index = sy-tabix.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wl_saida_0140-anln1
        IMPORTING
          output = imobilizado.

      IF wl_saida_0140-ano_vcto IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e09 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'ANO_VCTO'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-dt_venc IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e10 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'DT_VENC'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-dep_resp IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e11 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'DEP_RESP'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-waers IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e12 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'WAERS'
                             index = v_index ).
      ENDIF.

      IF wl_saida_0140-vlr_total IS INITIAL.
        CLEAR msg_erro.

        CONCATENATE TEXT-e08 imobilizado 'Tipo:' wl_saida_0140-tp_obrig
        INTO msg_erro SEPARATED BY space.

        criar_mensagem_erro( text1 = msg_erro
                             text2 = space
                             field = 'VLR_TOTAL'
                             index = v_index ).
      ENDIF.

*    IF WL_SAIDA_0140-VLR_PRINC IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Principal' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_CORRE IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Correção' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_MULTA IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Multa' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_JUROS IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr Juros' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_TSE IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Vlr TSE' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-COD_BARRAS IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Cód barras' ).
*    ENDIF.
*
*    IF WL_SAIDA_0140-VLR_TOTAL IS INITIAL.
*      CRIAR_MENSAGEM_ERRO( TEXT1 = TEXT-E04
*                           TEXT2 = 'Cód barras' ).
*    ENDIF.

    ENDLOOP.

    CHECK gt_msg_return IS NOT INITIAL.
    show_splitter_error( i_show  =  x
                         i_popup =  0 ).
*    X = SPACE.

  ENDMETHOD.                    "VALIDAR_SCREEN_0140

  METHOD criar_mathcode.

    TYPES: BEGIN OF ty_mathcode,
             kfzkz TYPE anlz-kfzkz,
           END OF ty_mathcode.

    DATA: gt_t005u    TYPE TABLE OF t005u,
          wl_t005u    TYPE t005u,
          gt_mathcode TYPE TABLE OF ty_mathcode,
          wl_mathcode TYPE ty_mathcode,
          it_return   TYPE STANDARD TABLE OF ddshretval.

    SELECT *
      FROM t005u
      INTO TABLE gt_t005u
     WHERE spras EQ 'PT'
       AND land1 EQ 'BR'.

    LOOP AT gt_t005u INTO wl_t005u.
      MOVE wl_t005u-bland TO wl_mathcode-kfzkz.
      APPEND wl_mathcode  TO gt_mathcode.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*       DDIC_STRUCTURE  = 'EKKO'
        retfield        = 'KFZKZ'
*       PVALKEY         = ' '
*       DYNPPROG        = sy-repid
*       DYNPNR          = sy-dynnr
*       DYNPROFIELD     = 'EBELN'
*       STEPL           = 0
        window_title    = 'VKBUR Records'
*       VALUE           = ' '
        value_org       = 'S'
*       MULTIPLE_CHOICE = 'X'  "allows you select multiple entries from the popup
*       DISPLAY         = ' '
*       CALLBACK_PROGRAM       = ' '
*       CALLBACK_FORM   = ' '
*       MARK_TAB        =
* IMPORTING
*       USER_RESET      = ld_ret
      TABLES
        value_tab       = gt_mathcode
*       FIELD_TAB       = lt_field
        return_tab      = it_return
*       DYNPFLD_MAPPING =
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

  ENDMETHOD.                    "CRIAR_MATHCODE
ENDCLASS.                    "ZUTILS IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS GERAR_CONTAS_PAGAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS GERAR_CONTAS_PAGAR_ver DEFINITION.
  PUBLIC SECTION.

    METHODS seleciona_imposto IMPORTING
                                i_cod_imposto TYPE zcod_imposto.


    METHODS gera_lote IMPORTING
                        i_bukrs    TYPE bukrs
                        i_descr    TYPE char100
                        i_depto    TYPE char2
                        i_venci    TYPE erdat
                      EXPORTING
                        e_num_lote TYPE numc10
                        e_dt_venci TYPE erdat.

    METHODS gera_doc_imposto
      IMPORTING
        i_num_lote  TYPE numc10
        i_bukrs     TYPE bukrs
        i_dt_vcto   TYPE erdat
        i_mes_vcto  TYPE numc2
        i_ano_vcto  TYPE numc4
        i_placa     TYPE kfzkz
        i_filial    TYPE gsber
        i_kostl     TYPE kostl
        i_c_imposto TYPE zcod_imposto
        i_c_barras  TYPE zcod_barras
        i_lifnr     TYPE lifnr
        i_vlr_princ TYPE dmbtr
        i_vlr_corre TYPE dmbtr
        i_vlr_multa TYPE dmbtr
        i_vlr_juros TYPE dmbtr
        i_vlr_tse   TYPE dmbtr
        i_vlr_total TYPE dmbtr
      EXPORTING
        e_num_docu  TYPE numc10.

    DATA: gt_zimp_lanc_imp_ct TYPE TABLE OF zimp_lanc_imp_ct,
          wl_zimp_cad_imposto TYPE zimp_cad_imposto, "Cadastro dos Imposto
          wl_zimp_lanc_imp_ct TYPE zimp_lanc_imp_ct,
          wl_zimp_cad_lote    TYPE zimp_cad_lote,
          wl_zaa002           TYPE zaa002.
ENDCLASS.                    "GERAR_CONTAS_PAGAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS GERAR_CONTAS_PAGAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS GERAR_CONTAS_PAGAR_ver IMPLEMENTATION.
  METHOD seleciona_imposto.
    CLEAR: gt_zimp_cad_imp_con.

    SELECT SINGLE *
      FROM zimp_cad_imposto
      INTO wl_zimp_cad_imposto
     WHERE cod_imposto = i_cod_imposto.

    SELECT *
      FROM zimp_cad_imp_con
      INTO TABLE gt_zimp_cad_imp_con
     WHERE cod_imposto = i_cod_imposto.
  ENDMETHOD.                    "SELECIONA_IMPOSTO

  METHOD gera_lote.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZID_LOT'
      IMPORTING
        number      = e_num_lote.

    MOVE:
    sy-mandt   TO wl_zimp_cad_lote-mandt,
    e_num_lote TO wl_zimp_cad_lote-lote,
    i_bukrs    TO wl_zimp_cad_lote-bukrs,
    i_descr    TO wl_zimp_cad_lote-descr_lote,
    sy-uname   TO wl_zimp_cad_lote-usnam,
    i_depto    TO wl_zimp_cad_lote-dep_resp,
    i_venci    TO wl_zimp_cad_lote-dt_venc,
    sy-uname   TO wl_zimp_cad_lote-usuario,
    sy-datum   TO wl_zimp_cad_lote-data_atual,
    sy-uzeit   TO wl_zimp_cad_lote-hora_atual.

*   Retorna data de vencimento para o chamador.
    e_dt_venci = i_venci.

    INSERT zimp_cad_lote FROM wl_zimp_cad_lote.
    COMMIT WORK.

  ENDMETHOD.                    "GERA_LOTE

  METHOD gera_doc_imposto.
* ---> S4 Migration - 17/07/2023 - CA
    DATA: lt_returns         TYPE TABLE OF bapiret2,
          ls_coeldes         TYPE bapi1030_ceoutputlist,
          lv_controllingarea TYPE bapi1030_gen-co_area,
          lv_costelement     TYPE bapi1030_gen-cost_elem,
          lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA

    DATA: r_utils  TYPE REF TO zutils,
          wl_cskb  TYPE cskb,
          wl_tka02 TYPE tka02.

    CLEAR:gt_zimp_lanc_imp_ct,
          wl_zimp_lanc_imp_ct.

    CREATE OBJECT r_utils.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = 'ZT'
        object      = 'RF_BELEG'
        subobject   = i_bukrs
      IMPORTING
        number      = e_num_docu.

    r_utils->checar_dia_util( EXPORTING i_data = i_dt_vcto
                              IMPORTING e_data = wl_zimp_lanc_impost-dt_apuracao ).

    wl_zimp_lanc_impost-bukrs        = i_bukrs.
    wl_zimp_lanc_impost-lote         = i_num_lote.
    wl_zimp_lanc_impost-doc_imposto  = e_num_docu.
    wl_zimp_lanc_impost-dt_venc      = i_dt_vcto .
    wl_zimp_lanc_impost-mes_apuracao = i_mes_vcto.
    wl_zimp_lanc_impost-ano_apuracao = i_ano_vcto.
    wl_zimp_lanc_impost-observacao   = i_placa.
    wl_zimp_lanc_impost-cod_imposto  = i_c_imposto.
    wl_zimp_lanc_impost-ref_imposto  = wl_zimp_cad_imposto-descr_imposto.
    wl_zimp_lanc_impost-tp_imposto   = wl_zimp_cad_imposto-tp_imposto.
    wl_zimp_lanc_impost-cod_pgto     = wl_zimp_cad_imposto-cod_pgto.
    wl_zimp_lanc_impost-conv_banco   = wl_zimp_cad_imposto-conv_banco.
    wl_zimp_lanc_impost-hbkid        = wl_zimp_cad_imposto-hbkid.
    wl_zimp_lanc_impost-gsber        = i_filial.
    wl_zimp_lanc_impost-waers        = wl_zimp_cad_imposto-waers.
    wl_zimp_lanc_impost-cod_barras   = i_c_barras.
    wl_zimp_lanc_impost-data_atual   = wl_zimp_cad_lote-data_atual.
    wl_zimp_lanc_impost-hora_atual   = wl_zimp_cad_lote-hora_atual.
    wl_zimp_lanc_impost-usuario      = wl_zimp_cad_lote-usuario.
    INSERT zimp_lanc_impost FROM wl_zimp_lanc_impost.
    COMMIT WORK.

    SORT gt_zimp_cad_imp_con BY cod_abertura.
    LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

      CASE wl_zimp_cad_imp_con-cod_abertura.
*---> 07/06/2023 - Migração S4 - JS
*        WHEN 01. "Principal
*            WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_PRINC.
*        WHEN 02. "Correção
*            WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_CORRE.
*        when 04. "Multas
*          WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_MULTA.
*        when 05. "Juros
*           WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_JUROS.
*        when 06. "Tse
*           WL_ZIMP_LANC_IMP_CT-VALOR_IMP = I_VLR_TSE.
        WHEN 01. "Principal
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_princ ).
        WHEN 02. "Correção
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_corre ).
        WHEN 04. "Multas
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_multa ).
        WHEN 05. "Juros
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_juros ).
        WHEN 06. "Tse
          wl_zimp_lanc_imp_ct-valor_imp = CONV #( i_vlr_tse ).
*<--- 07/06/2023 - Migração S4 - JS
        WHEN 11. "Fornecedor
          wl_zimp_lanc_imp_ct-lifnr     = i_lifnr.
          wl_zimp_lanc_imp_ct-valor_imp = ( i_vlr_total * -1 ).
        WHEN OTHERS.
      ENDCASE.

*   Seleciona área de contabilidade de custos
      SELECT SINGLE *
        FROM tka02
        INTO wl_tka02
       WHERE bukrs  = i_bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*      select single *
*        from CSKB
*        into WL_CSKB
*       where KOKRS  = WL_TKA02-KOKRS
*         and KSTAR  = WL_ZIMP_CAD_IMP_CON-HKONT
*         and DATAB  <= SY-DATUM
*         and DATBI  >= SY-DATUM.
*
*      if ( SY-SUBRC is initial ).
*        WL_ZIMP_LANC_IMP_CT-KOSTL = I_KOSTL.
*      endif.
      lv_controllingarea  = wl_tka02-kokrs.
      lv_costelement      = wl_zimp_cad_imp_con-hkont.
      lv_keydate          = sy-datum.

      CLEAR: lt_returns[], ls_coeldes.

      CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
        EXPORTING
          controllingarea   = lv_controllingarea
          costelement       = lv_costelement
          keydate           = lv_keydate
        IMPORTING
          costelementdetail = ls_coeldes
        TABLES
          return            = lt_returns.

      READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        wl_zimp_lanc_imp_ct-kostl = i_kostl.
      ENDIF.
* <--- S4 Migration - 17/07/2023 - CA

      wl_zimp_lanc_imp_ct-doc_imposto  = e_num_docu.
      wl_zimp_lanc_imp_ct-cod_imposto  = wl_zimp_cad_imp_con-cod_imposto.
      wl_zimp_lanc_imp_ct-cod_abertura = wl_zimp_cad_imp_con-cod_abertura.
      wl_zimp_lanc_imp_ct-bukrs        = i_bukrs.
      wl_zimp_lanc_imp_ct-seqitem      = sy-tabix.
      wl_zimp_lanc_imp_ct-bschl        = wl_zimp_cad_imp_con-bschl.
      wl_zimp_lanc_imp_ct-hkont        = wl_zimp_cad_imp_con-hkont.
      wl_zimp_lanc_imp_ct-gsber        = i_filial.
      wl_zimp_lanc_imp_ct-kostl        = i_kostl.
      wl_zimp_lanc_imp_ct-data_atual   = sy-datum.
      wl_zimp_lanc_imp_ct-hora_atual   = sy-uzeit.
      wl_zimp_lanc_imp_ct-usuario      = sy-uname.
      APPEND wl_zimp_lanc_imp_ct TO gt_zimp_lanc_imp_ct.
    ENDLOOP.

    INSERT zimp_lanc_imp_ct FROM TABLE gt_zimp_lanc_imp_ct.
    COMMIT WORK.
  ENDMETHOD.                    "GERA_DOC_IMPOSTO
ENDCLASS.                    "GERAR_CONTAS_PAGAR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TIPO_OPERACAO_0110 DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_TIPO_OPERACAO_ver DEFINITION.
  PUBLIC SECTION.

    METHODS salvar_dados_0140.
    METHODS salvar_dados_0200.
    METHODS editar_dados_0200.

    DATA: r_utils TYPE REF TO zutils,
          cont    TYPE n VALUE 1.
ENDCLASS.                    "LCL_TIPO_OPERACAO_0110 DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TIPO_OPERACAO_0110 IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_TIPO_OPERACAO_ver IMPLEMENTATION.

  METHOD salvar_dados_0140.
    IF ( lines IS NOT INITIAL ).

      LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = 'X'.
        MOVE-CORRESPONDING wl_saida_0140 TO wl_zaa003.
        APPEND wl_zaa003 TO gt_zaa003.
      ENDLOOP.

      MODIFY zaa003 FROM TABLE gt_zaa003.
      COMMIT WORK.

      MESSAGE TEXT-s02 TYPE 'I' DISPLAY LIKE 'S'.

    ELSE.
      MESSAGE TEXT-e02 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "SALVAR_DADOS_0140

  METHOD salvar_dados_0200.
    CLEAR wl_zaa001-observacao.
    CREATE OBJECT r_utils.

    return_status = 1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_saida_0110-anln1
      IMPORTING
        output = wl_saida_0110-anln1.

    CALL METHOD obj_custom_editor->get_text_as_r3table
      IMPORTING
        table = gt_editor.

    LOOP AT gt_editor INTO wl_editor.
      CONCATENATE wl_zaa001-observacao wl_editor
      INTO wl_zaa001-observacao.
    ENDLOOP.

    IF ( modo_operacao EQ c_edit ).
      wl_zaa001-dt_modif     = sy-datum.
      wl_zaa001-hr_modif     = sy-uzeit.
      wl_zaa001-user_modif   = sy-uname.
    ELSE.
      wl_zaa001-dt_criacao   = sy-datum.
      wl_zaa001-hr_criacao   = sy-uzeit.
      wl_zaa001-user_criacao = sy-uname.
    ENDIF.

    MOVE:
    wl_saida_0110-bukrs TO wl_zaa001-bukrs,
    wl_saida_0110-anln1 TO wl_zaa001-anln1,
    wl_saida_0110-anln2 TO wl_zaa001-anln2,
    wl_saida_0110-kfzkz TO wl_zaa001-kfzkz.
    MODIFY zaa001 FROM wl_zaa001.

*   Exibe as informações inseridas nos dados complementares na ALV principal.
    MOVE-CORRESPONDING wl_zaa001 TO wl_saida_0110.
    MODIFY gt_saida_0110 FROM wl_saida_0110 INDEX wl_selected_rows-index.
*   --

    r_utils->tratar_campos( group1    = 'GR1'
                            group2    = space
                            value     = '0'
                            invisible = '0').

    r_utils->tratar_campos( group1    = 'GR2'
                            group2    = space
                            value     = '0'
                            invisible = '0').

    MESSAGE TEXT-s01 TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.                    "SALVAR_DADOS

  METHOD editar_dados_0200.
    CREATE OBJECT r_utils.

    modo_operacao = c_edit.
    return_status = 0.

    r_utils->tratar_campos( group1    = 'GR1'
                            group2    = space
                            value     = '1'
                            invisible = '0').

  ENDMETHOD.                    "EDITAR_DADOS
ENDCLASS.                    "LCL_TIPO_OPERACAO_0110 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_SELECIONA_DADOS_ver DEFINITION.
  PUBLIC SECTION.

    METHODS seleciona_dados_0110.
    METHODS seleciona_dados_0140.
    METHODS seleciona_dados_0170.
    METHODS seleciona_dados_0180.
    METHODS seleciona_dados_0200.

    DATA: gt_anla   TYPE TABLE OF anla,
          gt_anlz   TYPE TABLE OF anlz,
          wl_anla   TYPE anla,
          wl_anla_c TYPE anla,
          wl_anlz   TYPE anlz,
          at_cont   TYPE c.

    CONSTANTS:
      c_ipva   TYPE char04 VALUE 'Ipva',
      c_dpvat  TYPE char05 VALUE 'Dpvat',
      c_licenc TYPE char20 VALUE 'Licenciamento'.
ENDCLASS.                    "LCL_SELECIONA_DADOS DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_SELECIONA_DADOS_ver IMPLEMENTATION.
  METHOD seleciona_dados_0110.
    CLEAR: gt_anla, gt_anlz, gt_saida_0110.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_anla
      FROM anla AS a
INNER JOIN anlz AS b ON a~bukrs = b~bukrs AND
                        a~anln1 = b~anln1 AND
                        a~anln2 = b~anln2
     WHERE a~anln1 IN s_anln1
       AND a~bukrs IN s_bukrs
       AND a~aktiv IN s_incor
       AND a~deakt IN s_desat
       AND a~anlkl IN ('00010701', '00010702', '00010703', '00081010')
       AND b~kfzkz IN s_kfzkz
       AND b~werks IN s_werks
       AND b~bdatu EQ '99991231'.

    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2
       AND bdatu EQ '99991231'.

    SORT: gt_anlz BY bukrs
                     anln1.

    LOOP AT gt_anla INTO wl_anla.
      CLEAR wl_zaa001.

      READ TABLE gt_anlz INTO wl_anlz WITH KEY bukrs = wl_anla-bukrs
                                               anln1 = wl_anla-anln1 BINARY SEARCH.

      SELECT SINGLE *
        FROM zaa001
        INTO wl_zaa001
       WHERE bukrs = wl_anla-bukrs
         AND anln1 = wl_anla-anln1
         AND anln2 = wl_anla-anln2.
      "AND KFZKZ = WL_ANLZ-KFZKZ.

      wl_saida_0110-dt_incor    = wl_anla-aktiv.
      wl_saida_0110-dt_desat    = wl_anla-deakt.
      wl_saida_0110-anln1       = wl_anla-anln1.
      wl_saida_0110-anln2       = wl_anla-anln2.
      wl_saida_0110-bukrs       = wl_anla-bukrs.
      wl_saida_0110-filial      = wl_anlz-werks.
      wl_saida_0110-kfzkz       = wl_anlz-kfzkz.
      wl_saida_0110-centro      = wl_anlz-kostl.
      wl_saida_0110-nr_chassi   = wl_anla-invnr.
      wl_saida_0110-txt_princ   = wl_anla-txt50.
      wl_saida_0110-porte_obrig = wl_zaa001-porte_obrig.    "CS2019001264 28.05.2020
      wl_saida_0110-pais        = wl_zaa001-cod_pais.
      wl_saida_0110-regiao      = wl_zaa001-cod_regi.
      wl_saida_0110-ano_fabr    = wl_zaa001-ano_fabr.
      wl_saida_0110-ano_mod     = wl_zaa001-ano_mode.
      wl_saida_0110-potencia    = wl_zaa001-potencia.
      wl_saida_0110-cor         = wl_zaa001-cor.
      wl_saida_0110-pg_arq      = wl_zaa001-pg_arq.
      wl_saida_0110-resp_veic   = wl_zaa001-resp_veic.
      wl_saida_0110-cod_renavan = wl_zaa001-cod_registro.
      wl_saida_0110-mes_ipva    = wl_zaa001-mes_ipva.
      wl_saida_0110-mes_licenc  = wl_zaa001-mes_licenc.
      wl_saida_0110-mes_dpvat   = wl_zaa001-mes_dpvat.
      wl_saida_0110-dt_criacao  = wl_zaa001-dt_criacao.
      wl_saida_0110-hr_criacao  = wl_zaa001-hr_criacao.
      wl_saida_0110-user_criac  =	wl_zaa001-user_criacao.
      wl_saida_0110-dt_modif    = wl_zaa001-dt_modif.
      wl_saida_0110-user_modif  = wl_zaa001-user_modif.
      wl_saida_0110-observacao  = wl_zaa001-observacao.

      APPEND wl_saida_0110 TO gt_saida_0110.
      CLEAR wl_saida_0110.
    ENDLOOP.
  ENDMETHOD.                    "SELECIONA_DADOS_0110

  METHOD seleciona_dados_0140.
    CLEAR: gt_zaa001, gt_saida_0140, gt_saida_0140_aux2.

    DATA: r_utils         TYPE REF TO zutils,
          at_dia_util     TYPE sy-datum,
          at_tp_obrigacao TYPE c,
          at_objkey       TYPE awkey,
          return_status   LIKE abap_true,
          t_zimp_consulta TYPE TABLE OF  zimp_lanc_impost,
          w_zimp_consulta TYPE zimp_lanc_impost,
          w_zimp_imp_ct   TYPE zimp_lanc_imp_ct,
          t_estorno       TYPE TABLE OF bkpf,
          w_estorno       TYPE bkpf.

    CREATE OBJECT r_utils.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_zaa001
      FROM zaa001 AS a
INNER JOIN anlz   AS b ON a~bukrs = b~bukrs AND
                          a~anln1 = b~anln1 AND
                          a~anln2 = b~anln2
     WHERE a~anln1      IN s_imobil
       AND a~bukrs      IN s_empres
       AND b~kfzkz      IN s_placa
       AND b~werks      IN s_filial
       AND b~bdatu      EQ '99991231'.

    IF gt_zaa001[] IS NOT INITIAL.
      SELECT *
        FROM anla
        INTO TABLE gt_anla
         FOR ALL ENTRIES IN gt_zaa001
       WHERE bukrs = gt_zaa001-bukrs
         AND anln1 = gt_zaa001-anln1
         AND anln2 = gt_zaa001-anln2.
    ENDIF.

    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_zaa001
     WHERE bukrs = gt_zaa001-bukrs
       AND anln1 = gt_zaa001-anln1
       AND kfzkz = gt_zaa001-kfzkz.

**********02.05.2018
**********Ajuste de Placas comparando as tabelas ANLZ e ZAA001

    SELECT *
      FROM zaa001
      INTO TABLE @DATA(tl_zaa001)
      FOR ALL ENTRIES IN @gt_zaa001
      WHERE bukrs       = @gt_zaa001-bukrs AND
            anln1       = @gt_zaa001-anln1.

    LOOP AT gt_zaa001 INTO wl_zaa001.

      READ TABLE tl_zaa001 INTO wl_zaa001_aux WITH KEY anln1 = wl_zaa001-anln1 bukrs = wl_zaa001-bukrs.

      IF wl_zaa001_aux-kfzkz NE wl_zaa001-kfzkz.
        wl_zaa001_aux-kfzkz = wl_zaa001-kfzkz.
        MODIFY zaa001 FROM wl_zaa001.
      ENDIF.

    ENDLOOP.

************
************************************************************


    SORT: gt_anlz BY bukrs
                     anln1.

    LOOP AT gt_zaa001 INTO wl_zaa001.

      CLEAR:  wl_anlz.

      IF wl_zaa001-kfzkz IS NOT INITIAL.

        READ TABLE gt_anlz INTO wl_anlz WITH KEY anln1 = wl_zaa001-anln1
                                                 anln2 = wl_zaa001-anln2. "BINARY SEARCH.
        SELECT SINGLE *
          FROM anla
          INTO wl_anla_c
          WHERE anln1 = wl_zaa001-anln1
            AND bukrs = wl_zaa001-bukrs.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: wl_anla_c.
        ENDIF.

        IF wl_anla_c-deakt IS NOT INITIAL.
          DELETE TABLE gt_zaa001 FROM wl_zaa001." WHERE ANLN1 = WL_ZAA001-ANLN1.
          CLEAR wl_zaa001.
          CONTINUE.
        ENDIF.

        SELECT SINGLE *
          FROM t001
          INTO wl_t001
         WHERE bukrs = wl_zaa001-bukrs.

        at_cont = 1.

*     1 = Ipva | 2 = Dpvat | 3 = Licenciamento

        WHILE ( at_cont <= 3 ).
          CLEAR: wl_saida_0140,  wl_saida_0140_aux2, gt_estilo[], wl_zaa003, return_status, wl_zaa002, gt_zimp_cad_imp_con,
                 gt_fields_style, w_estorno, w_zimp_consulta.

          at_strlen       = ( strlen( wl_zaa001-kfzkz ) - 1 ).
*        at_tp_obrigacao = at_cont.

          SELECT SINGLE *
            FROM zaa002
            INTO wl_zaa002
           WHERE land1       = wl_zaa001-cod_pais
             AND rg_placa    = wl_zaa001-cod_regi
             AND final_placa = wl_zaa001-kfzkz+at_strlen(1)
             AND tp_obrig    = at_cont.

          sy-datum+6(2) = 10.
          sy-datum+0(4) = s_ano-low.


          CASE at_cont.
            WHEN '1'.

              SELECT  SINGLE *
                 FROM zaa003
                 INTO wl_zaa003
                WHERE anln1    EQ wl_zaa001-anln1
                  AND tp_obrig EQ c_ipva
                  AND ano_vcto EQ s_ano-low.


              IF ( sy-subrc IS INITIAL ).
                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT  SINGLE *
                   FROM zaa003
                   INTO wl_zaa003
                  WHERE anln1    EQ wl_zaa001-anln1
                    AND tp_obrig EQ c_ipva
                    AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_ipva.
                  wl_zaa003-tp_obrig = c_ipva.

                ELSE.

                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status       = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_ipva.
                wl_zaa003-tp_obrig = c_ipva.
              ENDIF.



              IF return_status IS INITIAL.
                IF wl_zaa001-mes_ipva <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_ipva = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.
                ENDIF.

                IF ( wl_zaa001-mes_ipva IS NOT INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_ipva.
              ELSE.

                IF ( wl_zaa001-mes_ipva IS NOT INITIAL ).
                  sy-datum+4(2) = wl_zaa001-mes_ipva.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.




            WHEN '2'.

              SELECT SINGLE *
                FROM zaa003
                INTO wl_zaa003

               WHERE anln1    EQ wl_zaa001-anln1
                 AND tp_obrig EQ c_dpvat
                 AND ano_vcto EQ s_ano-low.

              IF ( sy-subrc IS INITIAL ).

                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT SINGLE *
                    FROM zaa003
                    INTO wl_zaa003

                   WHERE anln1    EQ wl_zaa001-anln1
                     AND tp_obrig EQ c_dpvat
                     AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_dpvat.
                  wl_zaa003-tp_obrig = c_dpvat.

                ELSE.
                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_dpvat.
                wl_zaa003-tp_obrig = c_dpvat.
              ENDIF.


              IF return_status IS INITIAL.

                IF wl_zaa001-mes_dpvat <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_dpvat = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.
                ENDIF.

                IF ( NOT wl_zaa001-mes_dpvat IS INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_dpvat.

              ELSE.

                IF ( NOT wl_zaa001-mes_dpvat IS INITIAL ).
*                SY-DATUM+6(2) = 10.
                  sy-datum+4(2) = wl_zaa001-mes_dpvat.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.


            WHEN '3'.


              SELECT SINGLE *
                FROM zaa003
                INTO wl_zaa003
               WHERE anln1    EQ wl_zaa001-anln1
                 AND tp_obrig EQ c_licenc
                 AND ano_vcto EQ s_ano-low.


              IF ( sy-subrc IS INITIAL ).

                IF ( wl_zaa003-doc_imposto IS NOT INITIAL ).
                  r_utils->buscar_status_zib( EXPORTING
                                              i_bukrs       = wl_zaa003-bukrs
                                              i_doc_imposto = wl_zaa003-doc_imposto
                                              i_gjahr       = wl_zaa003-ano_vcto
                                              IMPORTING
                                              e_zibchv      = wl_zib_chave
                                              e_ziberr      = wl_zib_erro ).
                ENDIF.

                SELECT SINGLE *
                  FROM bkpf
                  INTO w_estorno
                  WHERE bukrs EQ wl_zaa001-bukrs
                  AND   belnr EQ wl_zib_chave-belnr.


                IF w_estorno-stblg IS NOT INITIAL.
                  DELETE FROM zaa003
                   WHERE bukrs       EQ wl_zaa003-bukrs
                    AND doc_imposto  EQ wl_zaa003-doc_imposto.

                  CLEAR wl_zaa003.

                  SELECT SINGLE *
                    FROM zaa003
                    INTO wl_zaa003
                   WHERE anln1    EQ wl_zaa001-anln1
                     AND tp_obrig EQ c_licenc
                     AND ano_vcto EQ s_ano-low.

                  wl_zaa003-mes_vcto = wl_zaa001-mes_licenc.
                  wl_zaa003-tp_obrig = c_licenc.

                ELSE.

                  IF ( wl_zib_chave IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_green_light.
                    wl_saida_0140_aux2-doc_contabil = wl_zib_chave-belnr.
                  ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                    wl_saida_0140_aux2-status = icon_red_light.
                  ELSE.
                    wl_saida_0140_aux2-status = icon_yellow_light.
                  ENDIF.
                  return_status = 'X'.
                ENDIF.

              ELSE.
                wl_zaa003-mes_vcto = wl_zaa001-mes_licenc.
                wl_zaa003-tp_obrig = c_licenc.
              ENDIF.


              IF return_status IS INITIAL.

                IF wl_zaa001-mes_licenc <> wl_zaa002-mes_vcto.

                  UPDATE zaa001 SET mes_licenc = wl_zaa002-mes_vcto
                  WHERE cod_pais   = wl_zaa002-land1
                   AND  cod_regi   = wl_zaa002-rg_placa
                   AND  kfzkz      = wl_zaa001-kfzkz.

                ENDIF.

                IF ( NOT wl_zaa001-mes_licenc IS INITIAL ).
                  sy-datum+4(2) = wl_zaa002-mes_vcto.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.

                wl_zaa003-mes_vcto = wl_zaa002-mes_vcto.
                wl_zaa003-tp_obrig = c_licenc.

              ELSE.

                IF ( NOT wl_zaa001-mes_licenc IS INITIAL ).

                  sy-datum+4(2) = wl_zaa001-mes_licenc.

                  r_utils->checar_dia_util( EXPORTING i_data = sy-datum
                                            IMPORTING e_data = at_dia_util ).
                ENDIF.
              ENDIF.


          ENDCASE.

          SELECT SINGLE *
            FROM zimp_lanc_impost
            INTO  w_zimp_consulta
          WHERE bukrs EQ wl_zaa003-bukrs
            AND lote  EQ wl_zaa003-lote
            AND doc_imposto EQ wl_zaa003-doc_imposto.

          IF w_zimp_consulta-doc_imposto <> '0000000000' .

            SELECT SINGLE *
              FROM zimp_lanc_imp_ct
               INTO w_zimp_imp_ct
             WHERE doc_imposto EQ w_zimp_consulta-doc_imposto
               AND cod_imposto EQ w_zimp_consulta-cod_imposto
               AND cod_abertura EQ w_zimp_consulta-tp_imposto
               AND bukrs        EQ wl_zaa003-bukrs. " acrescentado empresa pois numeração doc_imposto pode repetir

            IF sy-subrc = 0.
              wl_saida_0140_aux2-filial_pg         = w_zimp_imp_ct-gsber.
              wl_saida_0140_aux2-kostl_pg          = w_zimp_imp_ct-kostl.
            ENDIF.

          ENDIF.

          wl_saida_0140_aux2-anln1        = wl_zaa001-anln1.
          wl_saida_0140_aux2-anln2        = wl_zaa001-anln2.
          wl_saida_0140_aux2-txt50        = wl_anla_c-txt50.
          wl_saida_0140_aux2-ano_vcto     = wl_zaa003-ano_vcto.
          wl_saida_0140_aux2-bukrs        = wl_zaa001-bukrs.
          wl_saida_0140_aux2-cod_barras   = w_zimp_consulta-cod_barras.    "WL_ZAA003-COD_BARRAS.
          wl_saida_0140_aux2-cod_imposto  = wl_zaa003-cod_imposto.
          wl_saida_0140_aux2-conv_banc    = wl_zaa003-conv_banc.
          wl_saida_0140_aux2-cod_regi     = wl_zaa001-cod_regi.
          wl_saida_0140_aux2-cod_registro = wl_zaa001-cod_registro.
          IF return_status IS NOT INITIAL.
            wl_saida_0140_aux2-dep_resp     = wl_zaa003-dep_resp.
          ELSE.
            wl_saida_0140_aux2-dep_resp     = '76'.
          ENDIF.
          wl_saida_0140_aux2-doc_imposto  = wl_zaa003-doc_imposto.
          wl_saida_0140_aux2-dt_venc      = wl_zaa003-dt_venc.
          wl_saida_0140_aux2-erdat        = wl_zaa003-erdat.
          wl_saida_0140_aux2-ernam        = wl_zaa003-ernam.
          wl_saida_0140_aux2-kfzkz        = wl_zaa001-kfzkz.
          wl_saida_0140_aux2-lifnr        = wl_zaa003-lifnr.
          wl_saida_0140_aux2-lote         = wl_zaa003-lote.
          wl_saida_0140_aux2-mes_vcto     = wl_zaa003-mes_vcto.
          wl_saida_0140_aux2-tp_obrig     = wl_zaa003-tp_obrig.
          wl_saida_0140_aux2-vlr_corre    = wl_zaa003-vlr_corre.
          wl_saida_0140_aux2-vlr_juros    = wl_zaa003-vlr_juros.
          wl_saida_0140_aux2-vlr_multa    = wl_zaa003-vlr_multa.
          wl_saida_0140_aux2-vlr_princ    = wl_zaa003-vlr_princ.
          wl_saida_0140_aux2-vlr_total    = wl_zaa003-vlr_total.
          wl_saida_0140_aux2-vlr_tse      = wl_zaa003-vlr_tse.
          wl_saida_0140_aux2-waers        = wl_zaa003-waers.
          wl_saida_0140_aux2-werks        = wl_zaa003-werks.
          wl_saida_0140_aux2-kostl        = wl_anlz-kostl.



          IF ( return_status IS INITIAL ).

            wl_saida_0140_aux2-status      = icon_light_out.
            wl_saida_0140_aux2-werks       = wl_anlz-werks.
            wl_saida_0140_aux2-ano_vcto    = s_ano-low.
            wl_saida_0140_aux2-dt_venc     = at_dia_util.
            wl_saida_0140_aux2-waers       = wl_t001-waers.
            wl_saida_0140_aux2-lifnr       = wl_zaa002-lifnr.
            wl_saida_0140_aux2-cod_imposto = wl_zaa002-cod_imposto.

            SELECT *
              FROM zimp_cad_imp_con
              INTO TABLE gt_zimp_cad_imp_con
            WHERE cod_imposto = wl_zaa002-cod_imposto.


            SORT gt_zimp_cad_imp_con BY cod_abertura.
            LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

              CASE wl_zimp_cad_imp_con-cod_abertura.
                WHEN 01. "Principal
                  wl_fields_style-fieldname = 'VLR_PRINC'.
                WHEN 02. "Correção
                  wl_fields_style-fieldname = 'VLR_CORRE'.
                WHEN 04. "Multas
                  wl_fields_style-fieldname = 'VLR_MULTA'.
                WHEN 05. "Juros
                  wl_fields_style-fieldname = 'VLR_JUROS'.
                WHEN 06. "Tse
                  wl_fields_style-fieldname = 'VLR_TSE'.
                WHEN OTHERS.
                  EXIT.
              ENDCASE.

              APPEND wl_fields_style TO gt_fields_style.
              CLEAR wl_fields_style.
            ENDLOOP.

            SORT gt_fields_style BY fieldname.
            LOOP AT gt_fields_style INTO wl_fields_style.
              r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                             style     = cl_gui_alv_grid=>mc_style_enabled ).
            ENDLOOP.

          ELSE.

            IF wl_saida_0140_aux2-doc_contabil IS INITIAL.

              SELECT *
                FROM zimp_cad_imp_con
                INTO TABLE gt_zimp_cad_imp_con
              WHERE cod_imposto = wl_zaa002-cod_imposto.

              SORT gt_zimp_cad_imp_con BY cod_abertura.
              LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

                CASE wl_zimp_cad_imp_con-cod_abertura.
                  WHEN 01. "Principal
                    wl_fields_style-fieldname = 'VLR_PRINC'.
                  WHEN 02. "Correção
                    wl_fields_style-fieldname = 'VLR_CORRE'.
                  WHEN 04. "Multas
                    wl_fields_style-fieldname = 'VLR_MULTA'.
                  WHEN 05. "Juros
                    wl_fields_style-fieldname = 'VLR_JUROS'.
                  WHEN 06. "Tse
                    wl_fields_style-fieldname = 'VLR_TSE'.
                  WHEN OTHERS.
                    EXIT.
                ENDCASE.

                APPEND wl_fields_style TO gt_fields_style.
                CLEAR wl_fields_style.
              ENDLOOP.

              SORT gt_fields_style BY fieldname.
              LOOP AT gt_fields_style INTO wl_fields_style.
                r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                               style     = cl_gui_alv_grid=>mc_style_enabled ).
              ENDLOOP.

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140_aux2-estilo.

              CLEAR:  gt_estilo[], gt_fields_style.

            ENDIF.
            "BREAK-POINT.
            r_utils->z_style_disable_edit( fieldname = 'ANO_VCTO'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'COD_IMPOSTO'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'CONV_BANC'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'DEP_RESP'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'FILIAL_PG'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'KOSTL_PG'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).
            r_utils->z_style_disable_edit( fieldname = 'WAERS'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ).

          ENDIF.

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140_aux2-estilo.
          APPEND wl_saida_0140_aux2 TO gt_saida_0140_aux2.
          ADD 1 TO at_cont.
        ENDWHILE.

      ENDIF.

    ENDLOOP.

    IF s_mesv IS NOT INITIAL.

      LOOP AT gt_saida_0140_aux2 INTO wl_saida_0140_aux2  WHERE mes_vcto =  s_mesv-low.

        wl_saida_0140-status         =     wl_saida_0140_aux2-status.
        wl_saida_0140-check          =     wl_saida_0140_aux2-check.
        wl_saida_0140-kfzkz          =     wl_saida_0140_aux2-kfzkz.
        wl_saida_0140-bukrs          =     wl_saida_0140_aux2-bukrs.
        wl_saida_0140-werks          =     wl_saida_0140_aux2-werks.
        wl_saida_0140-anln1          =     wl_saida_0140_aux2-anln1.
        wl_saida_0140-anln2          =     wl_saida_0140_aux2-anln2.
        wl_saida_0140-txt50          =     wl_saida_0140_aux2-txt50.
        wl_saida_0140-mes_vcto       =     wl_saida_0140_aux2-mes_vcto.
        wl_saida_0140-cod_regi       =     wl_saida_0140_aux2-cod_regi.
        wl_saida_0140-cod_registro   =     wl_saida_0140_aux2-cod_registro.
        wl_saida_0140-tp_obrig       =     wl_saida_0140_aux2-tp_obrig.
        wl_saida_0140-ano_vcto       =     wl_saida_0140_aux2-ano_vcto.
        wl_saida_0140-dt_venc        =     wl_saida_0140_aux2-dt_venc.
        wl_saida_0140-dep_resp       =     wl_saida_0140_aux2-dep_resp.
        wl_saida_0140-cod_imposto    =     wl_saida_0140_aux2-cod_imposto.
        wl_saida_0140-waers          =     wl_saida_0140_aux2-waers.
        wl_saida_0140-kostl          =     wl_saida_0140_aux2-kostl.
        wl_saida_0140-kostl_pg       =     wl_saida_0140_aux2-kostl_pg.
        wl_saida_0140-filial_pg      =     wl_saida_0140_aux2-filial_pg.
        wl_saida_0140-lifnr          =     wl_saida_0140_aux2-lifnr.
        wl_saida_0140-vlr_princ      =     wl_saida_0140_aux2-vlr_princ.
        wl_saida_0140-vlr_corre      =     wl_saida_0140_aux2-vlr_corre.
        wl_saida_0140-vlr_multa      =     wl_saida_0140_aux2-vlr_multa.
        wl_saida_0140-vlr_juros      =     wl_saida_0140_aux2-vlr_juros.
        wl_saida_0140-vlr_tse        =     wl_saida_0140_aux2-vlr_tse.
        wl_saida_0140-vlr_total      =     wl_saida_0140_aux2-vlr_total.
        wl_saida_0140-conv_banc      =     wl_saida_0140_aux2-conv_banc.
        wl_saida_0140-cod_barras     =     wl_saida_0140_aux2-cod_barras.
        wl_saida_0140-estilo         =     wl_saida_0140_aux2-estilo.
        wl_saida_0140-erdat          =     wl_saida_0140_aux2-erdat.
        wl_saida_0140-ernam          =     wl_saida_0140_aux2-ernam.
        wl_saida_0140-lote           =     wl_saida_0140_aux2-lote.
        wl_saida_0140-doc_contabil   =     wl_saida_0140_aux2-doc_contabil.
        wl_saida_0140-doc_imposto    =     wl_saida_0140_aux2-doc_imposto.

        APPEND wl_saida_0140 TO gt_saida_0140.

        CLEAR: wl_saida_0140, wl_saida_0140_aux2.
      ENDLOOP.
    ELSE.
      MOVE-CORRESPONDING gt_saida_0140_aux2 TO gt_saida_0140.
    ENDIF.

  ENDMETHOD.

  METHOD seleciona_dados_0170.
    CLEAR: gt_anla, gt_anlz, gt_saida_0170,
           gt_zaa001.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE gt_anla
      FROM anla AS a
INNER JOIN anlz AS b ON a~bukrs = b~bukrs AND
                        a~anln1 = b~anln1 AND
                        a~anln2 = b~anln2
     WHERE a~anln1 IN s1_anln1
       AND a~bukrs IN s1_bukrs
       AND a~aktiv IN s1_incor
       AND a~deakt IN s1_desat
       AND b~kfzkz IN s1_kfzkz
       AND a~anlkl IN ('00010701', '00010702', '00010703', '00081010')
       AND b~werks IN s1_werks
       AND b~bdatu EQ '99991231'.

    IF s1_descr-low IS NOT INITIAL.
      DELETE gt_anla WHERE txt50 NS s1_descr-low.
    ENDIF.


    SELECT *
      FROM anlz
      INTO TABLE gt_anlz
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2
       AND bdatu EQ '99991231'.

    SELECT *
      FROM zaa001
      INTO TABLE gt_zaa001
   FOR ALL ENTRIES IN gt_anla
     WHERE bukrs EQ gt_anla-bukrs
       AND anln1 EQ gt_anla-anln1
       AND anln2 EQ gt_anla-anln2.

    LOOP AT gt_anla INTO wl_anla.
      READ TABLE: gt_anlz   INTO wl_anlz   WITH KEY anln1 = wl_anla-anln1,
                  gt_zaa001 INTO wl_zaa001 WITH KEY anln1 = wl_anla-anln1.

      wl_saida_0170-dt_incor    = wl_anla-aktiv.
      wl_saida_0170-dt_desat    = wl_anla-deakt.
      wl_saida_0170-anln1          = wl_anla-anln1.
      wl_saida_0170-anln2          = wl_anla-anln2.
      wl_saida_0170-bukrs          = wl_anla-bukrs.
      wl_saida_0170-nr_chassi      = wl_anla-invnr.
*      wl_saida_0170-txt_princ      = wl_anla-mcoa1.                         "CS2019001264 28.05.2020
      CONCATENATE wl_anla-txt50 wl_anla-txa50 INTO wl_saida_0170-txt_princ
                                              SEPARATED BY space.
      wl_saida_0170-filial         = wl_anlz-werks.
      wl_saida_0170-kfzkz          = wl_anlz-kfzkz.
      wl_saida_0170-centro         = wl_anlz-kostl.
      wl_saida_0170-pais           = wl_zaa001-cod_pais.
      wl_saida_0170-regiao         = wl_zaa001-cod_regi.
      wl_saida_0170-ano_fabr       = wl_zaa001-ano_fabr.
      wl_saida_0170-ano_mod        = wl_zaa001-ano_mode.
      wl_saida_0170-potencia       = wl_zaa001-potencia.
      wl_saida_0170-cor            = wl_zaa001-cor.
      wl_saida_0170-pg_arq         = wl_zaa001-pg_arq.
      wl_saida_0170-resp_veic      = wl_zaa001-resp_veic.
      wl_saida_0170-cod_renavan    = wl_zaa001-cod_registro.
      wl_saida_0170-mes_ipva       = wl_zaa001-mes_ipva.
      wl_saida_0170-mes_licenc     = wl_zaa001-mes_licenc.
      wl_saida_0170-mes_dpvat	     = wl_zaa001-mes_dpvat.
      wl_saida_0170-dt_criacao     = wl_zaa001-dt_criacao.
      wl_saida_0170-hr_criacao     = wl_zaa001-hr_criacao.
      wl_saida_0170-user_criac     = wl_zaa001-user_criacao.
      wl_saida_0170-dt_modif       = wl_zaa001-dt_modif.
      wl_saida_0170-user_modif     = wl_zaa001-user_modif.
      wl_saida_0170-obs            = wl_zaa001-observacao.
      wl_saida_0170-dut            = wl_zaa001-dut.
      wl_saida_0170-alienacao      = wl_zaa001-alienacao.
      wl_saida_0170-porte_obrig    = wl_zaa001-porte_obrig.

      APPEND wl_saida_0170 TO gt_saida_0170.

      CLEAR: wl_saida_0170,
             wl_zaa001,
             wl_anlz.
    ENDLOOP.
  ENDMETHOD.                    "seleciona_dados_0160

  METHOD seleciona_dados_0180.
    CLEAR: tp_obrigacao, gt_saida_0180, gt_zaa003.

    IF ( s2_ipva IS NOT INITIAL ).
      tp_obrigacao = 'Ipva'.
    ELSEIF ( s2_dpvat IS NOT INITIAL ).
      tp_obrigacao = 'Dpvat'.
    ELSEIF ( s2_licen IS NOT INITIAL ).
      tp_obrigacao = 'Licenciamento'.
    ELSE.
      tp_obrigacao = 'Todos'.
    ENDIF.

    IF ( tp_obrigacao = 'Todos' ).
      SELECT *
        FROM zaa003
        INTO TABLE gt_zaa003
       WHERE bukrs    IN s2_bukrs
         AND werks    IN s2_werks
         AND anln1    IN s2_anln1
         AND kfzkz    IN s2_kfzkz
         AND mes_vcto IN s2_mes_v
         AND ano_vcto IN s2_ano_v.

    ELSE.
      SELECT *
        FROM zaa003
        INTO TABLE gt_zaa003
       WHERE bukrs    IN s2_bukrs
         AND werks    IN s2_werks
         AND anln1    IN s2_anln1
         AND kfzkz    IN s2_kfzkz
         AND mes_vcto IN s2_mes_v
         AND ano_vcto IN s2_ano_v
         AND tp_obrig EQ tp_obrigacao.
    ENDIF.

    LOOP AT gt_zaa003 INTO wl_zaa003.

      wl_saida_0180-bukrs       = wl_zaa003-bukrs.
      wl_saida_0180-anln1       = wl_zaa003-anln1.
      wl_saida_0180-anln2       = wl_zaa003-anln2.
      wl_saida_0180-kfzkz       = wl_zaa003-kfzkz.
      wl_saida_0180-werks       = wl_zaa003-werks.
      wl_saida_0180-tp_obrig    = wl_zaa003-tp_obrig.
      wl_saida_0180-ano_vcto    = wl_zaa003-ano_vcto.
      wl_saida_0180-mes_vcto    = wl_zaa003-mes_vcto.
      wl_saida_0180-waers       = wl_zaa003-waers.
      wl_saida_0180-vlr_princ   = wl_zaa003-vlr_princ.
      wl_saida_0180-vlr_corre   = wl_zaa003-vlr_corre.
      wl_saida_0180-vlr_multa   = wl_zaa003-vlr_multa.
      wl_saida_0180-vlr_juros   = wl_zaa003-vlr_juros.
      wl_saida_0180-vlr_tse     = wl_zaa003-vlr_tse.
      wl_saida_0180-vlr_total   = wl_zaa003-vlr_total.
      wl_saida_0180-cod_barras  = wl_zaa003-cod_barras.
      wl_saida_0180-dt_venc     = wl_zaa003-dt_venc.
      wl_saida_0180-dep_resp    = wl_zaa003-dep_resp.
      wl_saida_0180-erdat       = wl_zaa003-erdat.
      wl_saida_0180-ernam       = wl_zaa003-ernam.
      wl_saida_0180-lote        = wl_zaa003-lote.
      wl_saida_0180-cod_imposto = wl_zaa003-cod_imposto.
      wl_saida_0180-doc_imposto = wl_zaa003-doc_imposto.
      APPEND wl_saida_0180 TO gt_saida_0180.
      CLEAR wl_saida_0180.
    ENDLOOP.
  ENDMETHOD.                    "seleciona_dados_0180

  METHOD seleciona_dados_0200.
    DATA: r_utils TYPE REF TO zutils,
          cont    TYPE n.

    CONSTANTS:
      c_licenciamento TYPE char20 VALUE 'LICENCIAMENTO',
      c_ipva          TYPE char20 VALUE 'IPVA',
      c_dpvat         TYPE char20 VALUE 'DPVAT'.

    CREATE OBJECT r_utils.
    REFRESH gt_msg_return.

    IF ( wl_saida_0110-kfzkz IS NOT INITIAL ).
      CONDENSE wl_saida_0110-kfzkz NO-GAPS.
      at_cont = ( strlen( wl_saida_0110-kfzkz ) - 1 ).
    ENDIF.

    CALL METHOD obj_custom_editor->get_text_as_r3table
      IMPORTING
        table = gt_editor.

    cont = 1.
    WHILE ( cont <= 3 ).

      tp_obrigacao = cont. CONDENSE tp_obrigacao NO-GAPS.
      CLEAR wl_zaa002.

      SELECT SINGLE *
        FROM zaa002
        INTO wl_zaa002
       WHERE land1       = wl_zaa001-cod_pais
         AND rg_placa    = wl_zaa001-cod_regi
         AND final_placa = wl_saida_0110-kfzkz+at_cont(1)
         AND tp_obrig    = tp_obrigacao.

      CASE tp_obrigacao.
        WHEN 1.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e13
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_ipva.
          ENDIF.

        WHEN 2.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e14
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_dpvat.
          ENDIF.

        WHEN 3.
          IF ( wl_zaa002 IS INITIAL ).
            r_utils->criar_mensagem_erro( text1 = TEXT-e15
                                          text2 = space
                                          field = space
                                          index = space ).
          ELSE.
            MOVE wl_zaa002-mes_vcto TO wl_zaa001-mes_licenc.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      cont = cont + 1.
    ENDWHILE.

    CHECK ( gt_msg_return IS NOT INITIAL ).
    r_utils->show_splitter_error( i_show  = x
                                  i_popup = 1 ).

**   PEGAR VENCIMENTO IPVA
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '1'.
*
*    if ( wl_zaa002-mes_vcto is initial ).
*
**     call method criar_mensagem_erro( text1 = msg_erro
**                                      text2 = space
**                                      field = 'ANO_VCTO'
**                                      index = v_index ).
*    endif.
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_ipva.
*    clear wl_zaa002.
*
**   PEGAR VENCIMENTO DPVAT
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '2'.
*
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_dpvat.
*    clear wl_zaa002.
*
**   PEGAR VENCIMENTO LICENCIAMENTO
*    select single *
*      from zaa002
*      into wl_zaa002
*     where land1       = wl_zaa001-cod_pais
*       and rg_placa    = wl_zaa001-cod_regi
*       and final_placa = wl_saida_0110-kfzkz+at_cont(1)
*       and tp_obrig    = '3'.
*
*    move:
*    wl_zaa002-mes_vcto to wl_zaa001-mes_licenc.
*    clear wl_zaa002.
  ENDMETHOD.                    "SELECIONA_DADOS_0200
ENDCLASS.                    "LCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_TREE_EVENT_RECEIVER_ver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.

*    ON_DOUBLE_CLICK_ALV FOR EVENT DOUBLE_CLICK OF CL_GUI_CONTAINER
*                        IMPORTING E_ROW E_COLUMN.

    CONSTANTS:
      tree_filho_01 TYPE n VALUE 2,
      tree_filho_02 TYPE n VALUE 4,
      tree_filho_03 TYPE n VALUE 6,
      tree_filho_04 TYPE n VALUE 7.
ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_rECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_TREE_EVENT_RECEIVER_ver IMPLEMENTATION.
  METHOD  handle_double_click.
    CASE node_key.
      WHEN tree_filho_01.
        screen_principal = c_screen_0110.
      WHEN tree_filho_02.
        screen_principal = c_screen_0140.
      WHEN tree_filho_03.
        screen_principal = c_screen_0160.
        screen_item      = c_screen_0170.
      WHEN tree_filho_04.
        screen_principal = c_screen_0160.
        screen_item      = c_screen_0180.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

*  METHOD ON_DOUBLE_CLICK_ALV.
*
*    BREAK-POINT.
*
*  ENDMETHOD.                    "DOUBLE_CLICK_ALV
ENDCLASS.                    "LCL_TREE_EVENT_rECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_ver DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm   FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

    CONSTANTS:
      c_btn_add_complemento  TYPE char20 VALUE 'BTN_ADD_COMPLEMENTO',
      c_btn_reset_lancamento TYPE char20 VALUE 'BTN_RESET_LANCAMENTO',
      c_btn_gerar_ctas_pgar  TYPE char20 VALUE 'BTN_GERAR_CTAS_PGAR',
      c_btn_salvar           TYPE char20 VALUE 'BTN_SALVAR',
      c_btn_atualizar        TYPE char20 VALUE 'BTN_ATUALIZAR'.

ENDCLASS.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER_ver IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: ls_good   TYPE lvc_s_modi,
          r_utils   TYPE REF TO zutils,
          vlr_total TYPE dmbtr.

    CLEAR: wl_zaa001, return_status, gt_fields, lines,
           gt_fields_style, gt_estilo[]. "GT_TDLINE,


    SORT er_data_changed->mt_good_cells BY tabix DESCENDING.


    CREATE OBJECT r_utils.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      IF ( ls_good-fieldname(3) = 'VLR').
        DELETE gt_msg_return WHERE field = 'VLR_TOTAL'
                               AND aba   = screen_principal
                               AND tabix = ls_good-row_id.
      ELSE.
        DELETE gt_msg_return WHERE field = ls_good-fieldname
                               AND aba   = screen_principal
                               AND tabix = ls_good-row_id.
      ENDIF.

      CASE screen_principal.
*        WHEN 0110.
*          DELETE ER_DATA_CHANGED->MT_GOOD_CELLS WHERE VALUE NE 'X'.
*          DESCRIBE TABLE ER_DATA_CHANGED->MT_GOOD_CELLS LINES LINES.
*
*          READ TABLE GT_SAIDA_0110 INTO WL_SAIDA_0110 INDEX LS_GOOD-ROW_ID.
*          CASE LS_GOOD-FIELDNAME.
*            WHEN 'CHECK'.
*
**             Seleciona a descrição dos campos, para o cabeçalho
*              SELECT SINGLE *
*                FROM T001
*                INTO WL_T001
*               WHERE BUKRS EQ WL_SAIDA_0110-BUKRS.
*
*              SELECT SINGLE *
*                FROM J_1BBRANCH
*                INTO WL_J_1BBRANCH
*               WHERE BRANCH EQ WL_SAIDA_0110-FILIAL.
*
*              SELECT SINGLE *
*                FROM CSKT
*                INTO WL_CSKT
*               WHERE KOSTL EQ WL_SAIDA_0110-CENTRO.
*
*              MOVE:
*              WL_T001-BUTXT      TO WL_DESCR_HEADER-EMPRESA,
*              WL_J_1BBRANCH-NAME TO WL_DESCR_HEADER-FILIAL,
*              WL_CSKT-MCTXT      TO WL_DESCR_HEADER-CENTRO,
*
**             SUGERE PAIS DA EMPRESA NOS DADOS COMPLEMENTARES.
*
*              WL_T001-LAND1      TO WL_ZAA001-COD_PAIS.
**         -
*
**             Verifica se existe um registro de dados complementares
**             ja cadastrado, e carrega-os na tela.
*              SELECT SINGLE *
*                FROM ZAA001
*                INTO WL_ZAA001
*               WHERE ANLN1 EQ WL_SAIDA_0110-ANLN1
*                 AND ANLN2 EQ WL_SAIDA_0110-ANLN2.
**         --
*
*              IF SY-SUBRC IS INITIAL.
*                WL_TDLINE = WL_ZAA001-OBSERVACAO.
*                APPEND WL_TDLINE TO GT_TDLINE.
*
*                SHIFT WL_SAIDA_0110-ANLN1 LEFT DELETING LEADING '0'.
*                RETURN_STATUS = 1.
*
*                R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR1'
*                                        GROUP2    = SPACE
*                                        VALUE     = '0'
*                                        INVISIBLE = '0').
*
*                R_UTILS->TRATAR_CAMPOS( GROUP1    = 'GR2'
*                                        GROUP2    = SPACE
*                                        VALUE     = '0'
*                                        INVISIBLE = '0').
*              ENDIF.
*            WHEN OTHERS.
*          ENDCASE.

        WHEN 0140.
          CLEAR wl_saida_0140-vlr_total.

          READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX ls_good-row_id.
          CASE ls_good-fieldname.
            WHEN 'CHECK'.
              wl_saida_0140-check     = ls_good-value.

              IF ( ls_good-value IS INITIAL ).
                DELETE tl_selected_rows WHERE index = ls_good-row_id.
              ELSE.
                wl_selected_rows = ls_good-row_id.
                APPEND wl_selected_rows TO tl_selected_rows.
              ENDIF.

*              CALL METHOD OBJ_ALV_0140->SET_SELECTED_ROWS
*                EXPORTING
*                  IT_INDEX_ROWS = TL_SELECTED_ROWS.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING check.
            WHEN 'CONV_BANC'.
              CLEAR: wl_saida_0140-estilo, wl_zaa001, wl_zaa002.

              wl_saida_0140-conv_banc  = ls_good-value.
              at_strlen = ( strlen( wl_saida_0140-kfzkz ) - 1 ).

              CASE wl_saida_0140-tp_obrig.
                WHEN 'Ipva'.
                  tp_obrigacao = 1.
                WHEN 'Dpvat'.
                  tp_obrigacao = 2.
                WHEN 'Licenciamento'.
                  tp_obrigacao = 3.
                WHEN OTHERS.
              ENDCASE.

              CONDENSE tp_obrigacao NO-GAPS.

              SELECT SINGLE *
                FROM zaa001
                INTO wl_zaa001
               WHERE bukrs = wl_saida_0140-bukrs
                 AND anln1 = wl_saida_0140-anln1
                 AND kfzkz = wl_saida_0140-kfzkz.

              SELECT SINGLE *
                FROM zaa002
                INTO wl_zaa002
               WHERE land1       = wl_zaa001-cod_pais
                 AND rg_placa    = wl_zaa001-cod_regi
                 AND final_placa = wl_zaa001-kfzkz+at_strlen(1)
                 AND mes_vcto    = wl_saida_0140-mes_vcto
                 AND tp_obrig    = tp_obrigacao.

              IF ( ls_good-value EQ 'X' ).
                wl_saida_0140-cod_imposto = wl_zaa002-conv_banc.
              ELSE.
                wl_saida_0140-cod_imposto = wl_zaa002-cod_imposto.
              ENDIF.

              SELECT *
                FROM zimp_cad_imp_con
                INTO TABLE gt_zimp_cad_imp_con
               WHERE cod_imposto = wl_saida_0140-cod_imposto.

              SORT gt_zimp_cad_imp_con BY cod_abertura.
              LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

                CASE wl_zimp_cad_imp_con-cod_abertura.
                  WHEN 01. "Principal
                    wl_fields_style-fieldname = 'VLR_PRINC'.
                  WHEN 02. "Correção
                    wl_fields_style-fieldname = 'VLR_CORRE'.
                  WHEN 04. "Multas
                    wl_fields_style-fieldname = 'VLR_MULTA'.
                  WHEN 05. "Juros
                    wl_fields_style-fieldname = 'VLR_JUROS'.
                  WHEN 06. "Tse
                    wl_fields_style-fieldname = 'VLR_TSE'.
                  WHEN OTHERS.
                    EXIT.
                ENDCASE.

                APPEND wl_fields_style TO gt_fields_style.
                CLEAR wl_fields_style.
              ENDLOOP.

              SORT gt_fields_style BY fieldname.
              LOOP AT gt_fields_style INTO wl_fields_style.
                r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                               style     = cl_gui_alv_grid=>mc_style_enabled ).
              ENDLOOP.

              INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id.

            WHEN 'COD_IMPOSTO'.
              wl_saida_0140-cod_imposto = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING cod_imposto.

            WHEN 'COD_BARRAS'.
              wl_saida_0140-cod_barras = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING cod_barras.
            WHEN 'DEP_RESP'.
              wl_saida_0140-dep_resp  = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING dep_resp.

            WHEN 'DT_VENC'.
              wl_saida_0140-dt_venc  = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
                TRANSPORTING dt_venc.
            WHEN 'VLR_PRINC'.

              wl_saida_0140-vlr_princ = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

*              SELECT *
*                FROM ZIMP_CAD_IMP_CON
*                INTO TABLE GT_ZIMP_CAD_IMP_CON
*               WHERE COD_IMPOSTO = WL_SAIDA_0140-COD_IMPOSTO.

              wl_saida_0140_aux-cod_imposto = wl_saida_0140-cod_imposto.
              wl_saida_0140_aux-vlr_total   = wl_saida_0140-vlr_total.
              wl_saida_0140_aux-vlr_princ   = wl_saida_0140-vlr_princ.
              wl_saida_0140_aux-bukrs       = wl_saida_0140-bukrs.
              wl_saida_0140_aux-anln1       = wl_saida_0140-anln1.
              wl_saida_0140_aux-doc_imposto = wl_saida_0140-doc_imposto.


              APPEND wl_saida_0140_aux  TO gt_saida_0140_aux .


              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_princ vlr_total.


*              SORT GT_ZIMP_CAD_IMP_CON BY COD_ABERTURA.
*              LOOP AT GT_ZIMP_CAD_IMP_CON INTO WL_ZIMP_CAD_IMP_CON.
*
**              IF WL_ZIMP_CAD_IMP_CON-COD_ABERTURA = 01.
**
**                UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = WL_SAIDA_0140-VLR_TOTAL
**                 WHERE BUKRS EQ WL_SAIDA_0140-BUKRS
**                 AND   COD_ABERTURA EQ WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
**
**                UPDATE  ZAA003 SET  VLR_PRINC = WL_SAIDA_0140-VLR_PRINC
**                                    VLR_TOTAL = WL_SAIDA_0140-VLR_TOTAL
**
**                 WHERE BUKRS       = WL_SAIDA_0140-BUKRS
**                 AND   ANLN1       = WL_SAIDA_0140-ANLN1
**                 AND   DOC_IMPOSTO = WL_SAIDA_0140-DOC_IMPOSTO.
**
**              ENDIF.
*
*                IF WL_ZIMP_CAD_IMP_CON-COD_ABERTURA = 11.
*                  VLR_TOTAL = WL_SAIDA_0140-VLR_TOTAL  * - 1.
*                  UPDATE ZIMP_LANC_IMP_CT SET VALOR_IMP = VLR_TOTAL
*                  WHERE BUKRS EQ WL_SAIDA_0140-BUKRS
*                  AND   COD_ABERTURA EQ WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
*                ENDIF.
*              ENDLOOP.



            WHEN 'VLR_CORRE'.
              wl_saida_0140-vlr_corre = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_corre vlr_total.

            WHEN 'VLR_MULTA'.
              wl_saida_0140-vlr_multa = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_multa vlr_total.

            WHEN 'VLR_JUROS'.
              wl_saida_0140-vlr_juros = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_juros vlr_total.

            WHEN 'VLR_TSE'.
              wl_saida_0140-vlr_tse = ls_good-value.

              wl_saida_0140-vlr_total = wl_saida_0140-vlr_princ + wl_saida_0140-vlr_corre +
                                        wl_saida_0140-vlr_multa + wl_saida_0140-vlr_juros +
                                        wl_saida_0140-vlr_tse.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING vlr_tse vlr_total.
            WHEN 'FILIAL_PG'.

              wl_saida_0140-filial_pg = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING filial_pg.

            WHEN 'KOSTL_PG'.

              wl_saida_0140-kostl_pg = ls_good-value.

              MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX ls_good-row_id
              TRANSPORTING kostl_pg.

            WHEN OTHERS.
          ENDCASE.

          r_utils->show_splitter_error( i_show  = space
                                        i_popup = space ).

          IF ( ls_good-fieldname NE 'CHECK' ).
            CALL METHOD obj_alv_0140->refresh_table_display
              EXPORTING
                is_stable = wl_stable.
          ENDIF.

      ENDCASE.
    ENDLOOP.

    "SORT ER_DATA_CHANGED->MT_GOOD_CELLS BY TABIX DESCENDING.
    CLEAR: ls_good, er_data_changed->mt_good_cells.
  ENDMETHOD.                    "DATA_CHANGED_0110

  METHOD on_data_changed_finished.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = space
        i_repid    = sy-repid
*       I_PRESSED_TAB = 'G_TAB_STRIP_IMP-PRESSED_TAB'
*       I_SET_FIELD   = 'X_FIELD'
      IMPORTING
        e_messagem = wl_mensagem
      TABLES
        it_msgs    = gt_msg_return.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_click.
    DATA: r_utils             TYPE REF TO zutils,
          txt_erro            TYPE itex132,
          at_objkey           TYPE awkey,
          wa_zimp_lanc_impost TYPE zimp_lanc_impost.

    DATA: obj_dados TYPE REF TO lcl_seleciona_dados.
    CREATE OBJECT obj_dados.

    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX e_row_id.

    CASE e_column_id.
      WHEN 'LOTE'.
        CHECK ( wl_saida_0140-lote IS NOT INITIAL ).

        PERFORM f_preencher_dynpro USING:
        'X' 'ZIMP54'     '1000',
        ' ' 'P_BUKRS'     wl_saida_0140-bukrs,
        ' ' 'P_LOTE'      wl_saida_0140-lote,
        ' ' 'BDC_OKCODE' '/00',
        ' ' 'BDC_OKCODE' '=ONLI'.

        opt-dismode  = 'E'.
        CALL TRANSACTION 'ZIMP54' USING gt_bdc OPTIONS FROM opt.

      WHEN 'DOC_IMPOSTO'.
        CHECK ( wl_saida_0140-doc_imposto IS NOT INITIAL ).

        SET PARAMETER ID 'BUK' FIELD wl_saida_0140-bukrs.
        SET PARAMETER ID 'BLN' FIELD wl_saida_0140-doc_imposto.
        CALL TRANSACTION 'ZIMP53' AND SKIP FIRST SCREEN.

        SELECT SINGLE *
          INTO wa_zimp_lanc_impost
          FROM zimp_lanc_impost
          WHERE doc_imposto EQ wl_saida_0140-doc_imposto
            AND bukrs EQ wl_saida_0140-bukrs
            AND loekz EQ 'X'.

        IF sy-subrc IS INITIAL.
          DELETE FROM zaa003
              WHERE bukrs EQ wl_saida_0140-bukrs
                AND anln1 EQ wl_saida_0140-anln1
                AND anln2 EQ wl_saida_0140-anln2
                AND kfzkz EQ wl_saida_0140-kfzkz
                AND werks EQ wl_saida_0140-werks
                AND tp_obrig EQ wl_saida_0140-tp_obrig
                AND ano_vcto EQ wl_saida_0140-ano_vcto
                AND mes_vcto EQ wl_saida_0140-mes_vcto
                AND doc_imposto EQ wl_saida_0140-doc_imposto.

          obj_dados->seleciona_dados_0140( ).
          CALL METHOD obj_alv_0140->refresh_table_display.

        ENDIF.

      WHEN 'DOC_CONTABIL'.
        CHECK ( wl_saida_0140-doc_contabil IS NOT INITIAL ).

        SET PARAMETER ID 'BLN' FIELD wl_saida_0140-doc_contabil.
        SET PARAMETER ID 'BUK' FIELD wl_saida_0140-bukrs.
        SET PARAMETER ID 'GJR' FIELD wl_saida_0140-ano_vcto.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN 'STATUS'.
        CHECK ( wl_saida_0140-status = icon_red_light ).
        CREATE OBJECT r_utils.

        CLEAR gt_saida_0300.

*       Verifica se o lançamento retornou erro, e busca o mesmo na ZIB
*       em seguida grava a mensagem junto do index na tabela de saída, para que o erro possa
*       ser tratado futuramente.

        r_utils->buscar_status_zib( EXPORTING
                                    i_bukrs       = wl_saida_0140-bukrs
                                    i_doc_imposto = wl_saida_0140-doc_imposto
                                    i_gjahr       = wl_saida_0140-ano_vcto
                                    IMPORTING
                                    e_zibchv      = wl_zib_chave
                                    e_ziberr      = wl_zib_erro ).

        wl_saida_0300-status     = icon_led_red.
        wl_saida_0300-msg_erro   = wl_zib_erro-message.
        wl_saida_0300-index_erro = e_row_id.
        APPEND wl_saida_0300 TO gt_saida_0300.

        CALL SCREEN 0300 STARTING AT 5 10
                         ENDING AT 78 14.

        CLEAR: wl_mensagem, gt_msg_return.
    ENDCASE.
  ENDMETHOD.                    "ON_CLICK

  METHOD constructor.
    CLEAR: obj_toolbar_manager.
    CREATE OBJECT obj_toolbar_manager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD set_toolbar.
    DATA wl_toolbar TYPE stb_button.

    CLEAR: wl_toolbar.

    CASE sy-dynnr.
      WHEN 0300.
        wl_toolbar-function     = c_btn_reset_lancamento.
        wl_toolbar-icon         = icon_modification_reset.
        wl_toolbar-quickinfo    = space.
        wl_toolbar-butn_type    = 0.
        wl_toolbar-text         = 'Reiniciar Lançamento'.
        APPEND wl_toolbar TO e_object->mt_toolbar.
        CLEAR wl_toolbar.

      WHEN OTHERS.

        CASE screen_principal.
          WHEN 0110.
            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


            wl_toolbar-function     = c_btn_add_complemento.
            wl_toolbar-icon         = icon_car.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Dados Complementares'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.

          WHEN 0140.
            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-function     = c_btn_gerar_ctas_pgar.
            wl_toolbar-icon         = icon_pm_insert.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Gerar Contas a Pagar →'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-butn_type    = 3.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.

            wl_toolbar-function     = c_btn_atualizar.
            wl_toolbar-icon         = icon_refresh.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Atualizar'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


            wl_toolbar-function     = c_btn_salvar.
            wl_toolbar-icon         = icon_system_save.
            wl_toolbar-quickinfo    = space.
            wl_toolbar-butn_type    = 0.
            wl_toolbar-text         = 'Atualizar Valor'.
            APPEND wl_toolbar TO e_object->mt_toolbar.
            CLEAR wl_toolbar.


*        CALL METHOD OBJ_TOOLBAR_MANAGER->REORGANIZE
*          EXPORTING
*            IO_ALV_TOOLBAR = E_OBJECT.
        ENDCASE.
    ENDCASE.

  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.

    DATA: r_utils           TYPE REF TO zutils,
          r_contas_pagar    TYPE REF TO gerar_contas_pagar,
          r_tipo_operacao   TYPE REF TO lcl_tipo_operacao,
          r_seleciona_dados TYPE REF TO lcl_seleciona_dados,
          i_qtd_str         TYPE i,
          lc_observacao	    TYPE zaa001-observacao,
          wa_linha          TYPE ty_editor,
          vlr_total         TYPE dmbtr.


    CREATE OBJECT: r_seleciona_dados,
                   r_contas_pagar,
                   r_tipo_operacao,
                   r_utils.


    CASE e_ucomm.
      WHEN c_btn_add_complemento.
        CLEAR: tl_selected_rows, wl_zaa001, gt_fields, return_status,
               gt_code, gt_editor.

        CALL METHOD obj_alv_0110->get_selected_rows
          IMPORTING
            et_index_rows = tl_selected_rows.

        DESCRIBE TABLE tl_selected_rows LINES lines.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e02 TYPE 'I' DISPLAY LIKE 'E'.
        ELSEIF ( lines >= 2 ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.

          LOOP AT tl_selected_rows INTO wl_selected_rows.
            READ TABLE gt_saida_0110 INTO wl_saida_0110 INDEX wl_selected_rows-index.

*           Seleciona a descrição dos campos, para o cabeçalho
            SELECT SINGLE *
              FROM t001
              INTO wl_t001
             WHERE bukrs EQ wl_saida_0110-bukrs.

            SELECT SINGLE *
              FROM j_1bbranch
              INTO wl_j_1bbranch
             WHERE branch EQ wl_saida_0110-filial.

            SELECT SINGLE *
              FROM cskt
              INTO wl_cskt
             WHERE kostl EQ wl_saida_0110-centro.

            MOVE:
            wl_t001-butxt      TO wl_descr_header-empresa,
            wl_j_1bbranch-name TO wl_descr_header-filial,
            wl_cskt-mctxt      TO wl_descr_header-centro,

*           Sugere país da empresa nos dados complementares.
            wl_t001-land1      TO wl_zaa001-cod_pais.

*           Verifica se existe um registro de dados complementares
*           ja cadastrado, e carrega-os na tela.
            SELECT SINGLE *
              FROM zaa001
              INTO wl_zaa001
             WHERE bukrs EQ wl_saida_0110-bukrs
               AND anln1 EQ wl_saida_0110-anln1.
            "AND KFZKZ EQ WL_SAIDA_0110-KFZKZ.
*         --

            IF sy-subrc IS INITIAL.
              APPEND wl_zaa001-observacao TO gt_editor.
              return_status = 1.

              r_utils->tratar_campos( group1    = 'GR1'
                                      group2    = space
                                      value     = '0'
                                      invisible = '0').

              r_utils->tratar_campos( group1    = 'GR2'
                                      group2    = space
                                      value     = '0'
                                      invisible = '0').
            ELSE.
              APPEND c_edit TO gt_code.
            ENDIF.

          ENDLOOP.

          r_utils->show_splitter_error( i_show  = space
                                        i_popup = space ).

          CALL SCREEN 0200 STARTING AT 01 01
                             ENDING AT 95 25.
        ENDIF.

      WHEN c_btn_gerar_ctas_pgar.
        DATA:
          at_descr_lote TYPE char100,
          at_mes_pgto   TYPE char100,
          at_num_lote   TYPE numc10,
          at_dt_venc    TYPE erdat,
          at_num_doc    TYPE numc10,
          at_index      TYPE sy-tabix.

        REFRESH gt_msg_return.

        r_utils->validar_screen_0140( ).

        CHECK ( gt_msg_return IS INITIAL ).

        LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE check = x.
          at_index = sy-tabix.

          CLEAR: at_descr_lote, at_mes_pgto,
                 wl_saida_0140-estilo,
                 gt_estilo[].

          CONCATENATE wl_saida_0140-mes_vcto '.' wl_saida_0140-ano_vcto INTO at_mes_pgto.
          CONCATENATE TEXT-i01 at_mes_pgto INTO at_descr_lote SEPARATED BY space .

*                              ___________________________
*_____________________________/SELECIONA OS DADOS IMPOSTOS\__________________________


          r_contas_pagar->seleciona_imposto(
                                     EXPORTING
                                     i_cod_imposto = wl_saida_0140-cod_imposto ).

*                              _______________________
*_____________________________/CRIAR LOTE DE PAGAMENTO\_____________________________

          IF ( at_num_lote IS INITIAL
          OR   at_dt_venc  NE wl_saida_0140-dt_venc ).

            r_contas_pagar->gera_lote( EXPORTING
                                       i_bukrs = wl_saida_0140-bukrs
                                       i_descr = at_descr_lote
                                       i_depto = wl_saida_0140-dep_resp
                                       i_venci = wl_saida_0140-dt_venc
                                       IMPORTING
                                       e_num_lote = at_num_lote
                                       e_dt_venci = at_dt_venc ).
          ENDIF.

*                              _________________________
*_____________________________/CRIA DOCUMENTO DE IMPOSTO\__________________________

          IF wl_saida_0140-kostl_pg IS NOT INITIAL.
            DATA(_kostl) =  wl_saida_0140-kostl_pg.
          ELSE.
            _kostl = wl_saida_0140-kostl.
          ENDIF.

          IF wl_saida_0140-filial_pg IS NOT INITIAL.
            DATA(_filial) = wl_saida_0140-filial_pg.
          ELSE.
            _filial = wl_saida_0140-werks.
          ENDIF.

          r_contas_pagar->gera_doc_imposto(
                                     EXPORTING
                                     i_num_lote  = at_num_lote
                                     i_bukrs     = wl_saida_0140-bukrs
                                     i_dt_vcto   = wl_saida_0140-dt_venc
                                     i_mes_vcto  = wl_saida_0140-mes_vcto
                                     i_ano_vcto  = wl_saida_0140-ano_vcto
                                     i_placa     = wl_saida_0140-kfzkz
                                     i_filial    = _filial
                                     i_kostl     = _kostl
                                     i_c_imposto = wl_saida_0140-cod_imposto
                                     i_c_barras  = wl_saida_0140-cod_barras
                                     i_lifnr     = wl_saida_0140-lifnr
                                     i_vlr_princ = wl_saida_0140-vlr_princ
                                     i_vlr_corre = wl_saida_0140-vlr_corre
                                     i_vlr_multa = wl_saida_0140-vlr_multa
                                     i_vlr_juros = wl_saida_0140-vlr_juros
                                     i_vlr_tse   = wl_saida_0140-vlr_tse
                                     i_vlr_total = wl_saida_0140-vlr_total
                                     IMPORTING
                                     e_num_docu = at_num_doc  ).


          wl_saida_0140-check       = space.
          wl_saida_0140-status      = icon_yellow_light.
          wl_saida_0140-erdat       = sy-datum.
          wl_saida_0140-ernam       = sy-uname.
          wl_saida_0140-lote        = at_num_lote.
          wl_saida_0140-doc_imposto = at_num_doc.

          r_utils->z_style_disable_edit( fieldname = 'ANO_VCTO'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'CHECK'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'COD_BARRAS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'COD_IMPOSTO'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'CONV_BANC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'DEP_RESP'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'DT_VENC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'FILIAL_PG'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'KOSTL_PG'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_CORRE'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_JUROS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_MULTA'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_PRINC'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'VLR_TSE'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).
          r_utils->z_style_disable_edit( fieldname = 'WAERS'
                                         style     = cl_gui_alv_grid=>mc_style_disabled ).

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX at_index.

          MOVE-CORRESPONDING wl_saida_0140 TO wl_zaa003.
          INSERT zaa003 FROM wl_zaa003.
          COMMIT WORK.

        ENDLOOP.

        IF sy-subrc IS INITIAL.
          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable = wl_stable_aux.
        ELSE.
          MESSAGE TEXT-e07 TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN c_btn_reset_lancamento.
        LOOP AT gt_saida_0300 INTO wl_saida_0300.
          READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX wl_saida_0300-index_erro.

          CLEAR: gt_estilo[], gt_fields_style, wl_saida_0140-estilo.

          r_utils->buscar_status_zib( EXPORTING
                                      i_bukrs       = wl_saida_0140-bukrs
                                      i_doc_imposto = wl_saida_0140-doc_imposto
                                      i_gjahr       = wl_saida_0140-ano_vcto
                                      IMPORTING
                                      e_zibchv      = wl_zib_chave
                                      e_ziberr      = wl_zib_erro ).

          DELETE FROM zib_contabil_err WHERE obj_key     = wl_zib_erro-obj_key.
          DELETE FROM zib_contabil     WHERE obj_key     = wl_zib_erro-obj_key.

          DELETE FROM zimp_lanc_impost WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.

          DELETE FROM zimp_lanc_imp_ct WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.

          DELETE FROM zimp_cad_lote    WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   lote        = wl_saida_0140-lote.

          DELETE FROM zaa003           WHERE bukrs       = wl_saida_0140-bukrs
                                       AND   anln1       = wl_saida_0140-anln1
                                       AND   doc_imposto = wl_saida_0140-doc_imposto.
          COMMIT WORK.

          wl_saida_0140-status       = icon_light_out.
          wl_saida_0140-doc_imposto  = ''.
          wl_saida_0140-lote         = ''.

*         Seleciona o imposto referênte ao tipo de obrigação, para definir quais
*         campos devem ser habilitados para o usuário.

          SELECT *
            FROM zimp_cad_imp_con
            INTO TABLE gt_zimp_cad_imp_con
           WHERE cod_imposto = wl_saida_0140-cod_imposto.

          SORT gt_zimp_cad_imp_con BY cod_abertura.
          LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.
            CASE wl_zimp_cad_imp_con-cod_abertura.
              WHEN 01. wl_fields_style-fieldname = 'VLR_PRINC'.
              WHEN 02. wl_fields_style-fieldname = 'VLR_CORRE'.
              WHEN 04. wl_fields_style-fieldname = 'VLR_MULTA'.
              WHEN 05. wl_fields_style-fieldname = 'VLR_JUROS'.
              WHEN 06. wl_fields_style-fieldname = 'VLR_TSE'.
              WHEN OTHERS.
                EXIT.
            ENDCASE.
            APPEND wl_fields_style TO gt_fields_style.
          ENDLOOP.

          wl_fields_style-fieldname = 'ANO_VCTO'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'CHECK'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'COD_BARRAS'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'COD_IMPOSTO'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'CONV_BANC'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'DEP_RESP'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'DT_VENC'.
          APPEND wl_fields_style TO gt_fields_style.
          wl_fields_style-fieldname = 'WAERS'.
          APPEND wl_fields_style TO gt_fields_style.

*         Primeiro eu coloco os campos em uma tabela e ordeno-a, e depois habilito-os
*         usando um método que pega o campo ordenado e coloca na tabela GT_ESTILO.

          SORT gt_fields_style BY fieldname.
          LOOP AT gt_fields_style INTO wl_fields_style.
            r_utils->z_style_disable_edit( fieldname = wl_fields_style-fieldname
                                           style     = cl_gui_alv_grid=>mc_style_enabled ).
          ENDLOOP.

          INSERT LINES OF gt_estilo INTO TABLE wl_saida_0140-estilo.
          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX wl_saida_0300-index_erro.
        ENDLOOP.

        CALL METHOD obj_alv_0140->refresh_table_display
          EXPORTING
            is_stable = wl_stable.

        LEAVE TO SCREEN 0.

      WHEN c_btn_atualizar.
        DATA: wl_zimp_cad_lote TYPE zimp_cad_lote.

        LOOP AT gt_saida_0140 INTO wl_saida_0140 WHERE status = icon_yellow_light
                                                    OR status = icon_green_light.
          at_index = sy-tabix.

          CASE wl_saida_0140-status.
            WHEN icon_yellow_light.
*                                     ________________________________
*            ________________________/VERIFICA SE O DOC FOI PROCESSADO\_________________________

              SELECT SINGLE *
                FROM zimp_cad_lote
                INTO wl_zimp_cad_lote
               WHERE bukrs = wl_saida_0140-bukrs
                 AND lote = wl_saida_0140-lote.

*                 Verifica se o lote foi aprovado para buscar o doc contábil (belnr).
              CHECK ( wl_zimp_cad_lote-status_lote = 'A' ).
              r_utils->buscar_status_zib( EXPORTING
                                          i_bukrs       = wl_saida_0140-bukrs
                                          i_doc_imposto = wl_saida_0140-doc_imposto
                                          i_gjahr       = wl_saida_0140-ano_vcto
                                          IMPORTING
                                          e_zibchv      = wl_zib_chave
                                          e_ziberr      = wl_zib_erro ).

              IF ( wl_zib_chave IS NOT INITIAL ).
                wl_saida_0140-status       = icon_green_light.
                wl_saida_0140-doc_contabil = wl_zib_chave-belnr.
              ELSEIF ( wl_zib_erro IS NOT INITIAL ).
                wl_saida_0140-status = icon_red_light.
              ENDIF.
*                                     _______________________________
*            ________________________/VERIFICA SE O DOC FOI ESTORNADO\_________________________

*            WHEN ICON_GREEN_LIGHT.
*              CLEAR: GT_ESTILO[], GT_FIELDS_STYLE, WL_SAIDA_0140-ESTILO, GT_ZIMP_CAD_IMP_CON.
*
*              SELECT SINGLE *
*                FROM ZIMP_LANC_IMPOST
*                INTO WL_ZIMP_LANC_IMPOST
*               WHERE BUKRS       EQ WL_SAIDA_0140-BUKRS
*                 AND DOC_IMPOSTO EQ WL_SAIDA_0140-DOC_IMPOSTO.
*
**             Verifica se o documento foi marcado para eliminação e deleta o documento
**             da tabela de Impostos para o mesmo poder ser gerado novamente.
**
**             Obs.: O processo para marcar p/ exclusão é feito via (ZIMP53).
*
*              IF ( WL_ZIMP_LANC_IMPOST-LOEKZ = 'X' ).
*                DELETE FROM ZAA003 WHERE BUKRS       = WL_SAIDA_0140-BUKRS
*                                     AND ANLN1       = WL_SAIDA_0140-ANLN1
*                                     AND DOC_IMPOSTO = WL_SAIDA_0140-DOC_IMPOSTO.
*                COMMIT WORK.
*
*                WL_SAIDA_0140-STATUS       = ICON_LIGHT_OUT.
*                WL_SAIDA_0140-DOC_IMPOSTO  = ''.
*                WL_SAIDA_0140-DOC_CONTABIL = ''.
*                WL_SAIDA_0140-LOTE         = ''.
*
**               Seleciona o imposto referênte ao tipo de obrigação, para definir quais
**               campos devem ser habilitados para o usuário.
*
*                SELECT *
*                  FROM ZIMP_CAD_IMP_CON
*                  INTO TABLE GT_ZIMP_CAD_IMP_CON
*                 WHERE COD_IMPOSTO = WL_SAIDA_0140-COD_IMPOSTO.
*
*                SORT GT_ZIMP_CAD_IMP_CON BY COD_ABERTURA.
*                LOOP AT GT_ZIMP_CAD_IMP_CON INTO WL_ZIMP_CAD_IMP_CON.
*                  CASE WL_ZIMP_CAD_IMP_CON-COD_ABERTURA.
*                    WHEN 01. WL_FIELDS_STYLE-FIELDNAME = 'VLR_PRINC'.
*                    WHEN 02. WL_FIELDS_STYLE-FIELDNAME = 'VLR_CORRE'.
*                    WHEN 04. WL_FIELDS_STYLE-FIELDNAME = 'VLR_MULTA'.
*                    WHEN 05. WL_FIELDS_STYLE-FIELDNAME = 'VLR_JUROS'.
*                    WHEN 06. WL_FIELDS_STYLE-FIELDNAME = 'VLR_TSE'.
*                    WHEN OTHERS.
*                      EXIT.
*                  ENDCASE.
*                  APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                ENDLOOP.
*
*                WL_FIELDS_STYLE-FIELDNAME = 'ANO_VCTO'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'CHECK'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'COD_BARRAS'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'COD_IMPOSTO'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'CONV_BANC'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'DEP_RESP'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'DT_VENC'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*                WL_FIELDS_STYLE-FIELDNAME = 'WAERS'.
*                APPEND WL_FIELDS_STYLE TO GT_FIELDS_STYLE.
*
**               Primeiro eu coloco os campos em uma tabela e ordeno-a, e depois habilito-os
**               usando um método que pega o campo ordenado e coloca na tabela GT_ESTILO.
*
*                SORT GT_FIELDS_STYLE BY FIELDNAME.
*                LOOP AT GT_FIELDS_STYLE INTO WL_FIELDS_STYLE.
*                  R_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = WL_FIELDS_STYLE-FIELDNAME
*                                                 STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED ).
*                ENDLOOP.
*
*                INSERT LINES OF GT_ESTILO INTO TABLE WL_SAIDA_0140-ESTILO.
*              ENDIF.
          ENDCASE.

          MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX at_index.
        ENDLOOP.

        IF ( sy-subrc IS INITIAL ).
          CALL METHOD obj_alv_0140->refresh_table_display
            EXPORTING
              is_stable = wl_stable.
        ENDIF.


      WHEN c_btn_salvar.

        LOOP AT gt_saida_0140_aux INTO wl_saida_0140_aux.

          SELECT *
            FROM zimp_cad_imp_con
            INTO TABLE gt_zimp_cad_imp_con
           WHERE cod_imposto = wl_saida_0140_aux-cod_imposto.


          SORT gt_zimp_cad_imp_con BY cod_abertura.
          LOOP AT gt_zimp_cad_imp_con INTO wl_zimp_cad_imp_con.

            UPDATE  zaa003 SET  vlr_princ = wl_saida_0140_aux-vlr_princ
                                vlr_total = wl_saida_0140_aux-vlr_total

             WHERE bukrs       = wl_saida_0140_aux-bukrs
             AND   anln1       = wl_saida_0140_aux-anln1
             AND   doc_imposto = wl_saida_0140_aux-doc_imposto.



            IF wl_zimp_cad_imp_con-cod_abertura = 01.

              UPDATE zimp_lanc_imp_ct SET valor_imp = wl_saida_0140_aux-vlr_total
               WHERE bukrs EQ wl_saida_0140_aux-bukrs
               AND   cod_abertura EQ wl_zimp_cad_imp_con-cod_abertura
               AND   doc_imposto  EQ wl_saida_0140_aux-doc_imposto.
            ENDIF.

            IF wl_zimp_cad_imp_con-cod_abertura = 11.
              vlr_total = wl_saida_0140_aux-vlr_total  * - 1.

              UPDATE zimp_lanc_imp_ct SET valor_imp = vlr_total
              WHERE bukrs EQ wl_saida_0140-bukrs
              AND   cod_abertura EQ wl_zimp_cad_imp_con-cod_abertura
              AND   doc_imposto  EQ wl_saida_0140_aux-doc_imposto.

            ENDIF.
          ENDLOOP.

          CLEAR wl_saida_0140_aux.

        ENDLOOP.

        MESSAGE TEXT-s02 TYPE 'I'.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM

  METHOD handle_user_command.

  ENDMETHOD.                    "user_command

  METHOD on_onf4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,
             fieldname  TYPE dd03l-fieldname,
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50)  ,
          END OF wl_valuetab,

          gt_field         TYPE TABLE OF ty_field,
          gt_value         TYPE TABLE OF ty_value,
          gt_valuetab      LIKE TABLE OF wl_valuetab,
          gt_t500w         TYPE TABLE OF t500w,
          gt_depto         TYPE TABLE OF zimp_cad_depto,

          wl_value         TYPE ty_value,
          wl_field         TYPE ty_field,
          wl_index         TYPE sy-tabix,
          wl_t500w         TYPE t500w,
          wl_depto         TYPE zimp_cad_depto,

          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

*--------------------------------------------------------------------------------------------------*
* SET ONF4                                                                                         *
*--------------------------------------------------------------------------------------------------*
    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'MOEDA'.

        SELECT *
          FROM t500w
          INTO TABLE gt_t500w.

        wl_fieldname = 'WAERS'.
        wl_tabname   = 'T500W'.

        LOOP AT gt_t500w INTO wl_t500w.
          MOVE wl_t500w-land1 TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.

          MOVE wl_t500w-waers TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.
        ENDLOOP.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'LAND1'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'WAERS'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

      WHEN 'DEP_RESP'.
        SELECT *
          FROM zimp_cad_depto
          INTO TABLE gt_depto.

        wl_fieldname = 'DEP_RESP'.
        wl_tabname   = 'ZIMP_CAD_DEPTO'.

        LOOP AT gt_depto INTO wl_depto.
          MOVE wl_depto-dep_resp TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.

          MOVE wl_depto-dep_resp_desc TO wl_valuetab-field.
          APPEND wl_valuetab TO gt_valuetab.
        ENDLOOP.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'DEP_RESP'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.

        wl_field-tabname   = wl_tabname.
        wl_field-fieldname = 'DEP_RESP_DESC'.
        wl_field-s         = 'X'.
        APPEND wl_field TO gt_field.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
        cucol                     = '10'
        curow                     = '5'
        fieldname                 = wl_fieldname
        tabname                   = wl_tabname
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = gt_field
        select_values             = gt_value
        valuetab                  = gt_valuetab
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

*--------------------------------------------------------------------------------------------------*
* GET ONF4                                                                                         *
*--------------------------------------------------------------------------------------------------*
    READ TABLE gt_saida_0140 INTO wl_saida_0140 INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'MOEDA'.
        READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'WAERS'.
        MOVE wl_value-char79 TO wl_saida_0140-waers.

        MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX es_row_no-row_id
          TRANSPORTING waers.

      WHEN 'DEP_RESP'.
        READ TABLE gt_value INTO wl_value WITH KEY fieldname = 'DEP_RESP'.
        MOVE wl_value-char79 TO wl_saida_0140-dep_resp.

        MODIFY gt_saida_0140 FROM wl_saida_0140 INDEX es_row_no-row_id
          TRANSPORTING dep_resp.
      WHEN OTHERS.
    ENDCASE.

    CALL METHOD obj_alv_0140->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDMETHOD.                    "ON_ONF4
ENDCLASS.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION
