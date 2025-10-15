
CLASS lcl_handle_events2 DEFINITION DEFERRED.
DATA: gr_events2 TYPE REF TO lcl_handle_events2.
CLASS lcl_handle_events2 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command2 FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_before_user_command2 FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_after_user_command2 FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_link_click2 FOR EVENT link_click OF cl_salv_events_table IMPORTING row column sender,
      toolbar2 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_interactive e_object sender,
      on_handle_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm
          sender,
      on_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells sender.

ENDCLASS.
CLASS lcl_handle_events2 IMPLEMENTATION.
  METHOD on_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i08.
  ENDMETHOD.
  METHOD on_before_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i09.
  ENDMETHOD.
  METHOD on_after_user_command2.
    PERFORM show_function_info2 USING e_salv_function TEXT-i10.
  ENDMETHOD.
  METHOD on_link_click2.

    CLEAR: w_saida2.
    READ TABLE t_saida2 INTO w_saida2 INDEX ROW.
  SET PARAMETER ID 'JEF' FIELD w_saida2-docnum_nf.
CALL TRANSACTION 'J1B2N' AND SKIP FIRST SCREEN.

ENDMETHOD.                    "on_link_click2
METHOD toolbar2.
DATA : mt_toolbar TYPE stb_button.

CLEAR mt_toolbar.
mt_toolbar-butn_type = '3'.   "separator
APPEND mt_toolbar TO e_object->mt_toolbar.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.
*
*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'DELETE_ROW'.   "fcode
*    mt_toolbar-icon = '@B_DELR@'.
*    mt_toolbar-quickinfo = 'Eliminar linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'GERAR_OV'.   "fcode
*    mt_toolbar-icon = icon_transport."'@B_GENR@'.
*    mt_toolbar-quickinfo = 'Gerar OV'.
*    mt_toolbar-text = 'Gerar OV'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.
*
*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'ESTORNAR'.   "fcode
*    mt_toolbar-icon = icon_storno.
*    mt_toolbar-quickinfo = 'Estornar OV'.
*    mt_toolbar-text = 'Estornar OV'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.
*
*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'FATURAR'.   "fcode
*    mt_toolbar-icon = icon_operation.
*    mt_toolbar-quickinfo = 'Gerar Fatura'.
*    mt_toolbar-text = 'Gerar Fatura'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
  "3 DESABILITA E 0 HABILITA
  IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
    <fs_tollbar>-butn_type = '3'.
  ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
    <fs_tollbar>-butn_type = '3'.
  ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
    <fs_tollbar>-butn_type = '3'.
  ENDIF.
  IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
    <fs_tollbar>-function = 'INSERT_ROW'.
  ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
    <fs_tollbar>-function = 'DELETE_ROW'.
  ENDIF.
ENDLOOP.
ENDMETHOD.                    "TOOLBAR

METHOD on_handle_data_changed2.

*    FIELD-SYMBOLS: <mp_mod_rows> TYPE table.
*
*    DATA: lt_good_cells TYPE lvc_t_modi,
*          ls_good_cell  TYPE lvc_s_modi.
*    DATA: qtd TYPE decfloat34.
*    DATA qtd_total TYPE decfloat34.
*    FREE: t_saida2_mem.",lt_good_cells.
*
*    CLEAR: w_saida2,qtd,qtd_total,lr_scell2,lr_scells2,lr_scolumns2,lr_srows2,lr_smode2.
*
*    lt_good_cells = er_data_changed->mt_good_cells.
*
*    ASSIGN er_data_changed->mp_mod_rows->* TO <mp_mod_rows>.
*    MOVE-CORRESPONDING <mp_mod_rows> TO t_saida2_mem.
*
*    LOOP AT lt_good_cells ASSIGNING FIELD-SYMBOL(<_qtd_tot>) WHERE fieldname = 'QTDFATUR' OR fieldname = 'QTDSOBRA'.
*      READ TABLE t_saida2_mem ASSIGNING FIELD-SYMBOL(<_tsaida2mem>) INDEX <_qtd_tot>-row_id.
*      MODIFY t_saida2 FROM <_tsaida2mem> INDEX <_qtd_tot>-row_id.
*      READ TABLE t_saida2 ASSIGNING FIELD-SYMBOL(<_tsaida2>) INDEX <_qtd_tot>-row_id.
*      IF <_tsaida2>-qtdpedid IS NOT INITIAL.
*        CLEAR:qtd_total.
*        qtd_total = ( ( <_tsaida2>-qtdpedid + ( <_tsaida2>-qtdfatur ) * -1 ) + <_tsaida2>-qtdsobra ).
*        DATA(msg_qtd_erro) = |Quantidade Faturada excede o Total!|.
*        IF qtd_total < 0.
*          MESSAGE msg_qtd_erro TYPE 'S' DISPLAY LIKE 'E'.
*          EXIT.
*          gr_table2->refresh( ).
*        ELSE.
*          IF  <_qtd_tot>-fieldname = 'QTDFATUR'.
*            CLEAR: qtd.
*            CONDENSE <_qtd_tot>-value NO-GAPS.
*            qtd = <_qtd_tot>-value.
*            <_tsaida2>-qtdfatur = qtd.
*          ENDIF.
*          IF  <_qtd_tot>-fieldname = 'QTDSOBRA'.
*            CLEAR: qtd.
*            CONDENSE <_qtd_tot>-value NO-GAPS.
*            qtd = <_qtd_tot>-value.
*            <_tsaida2>-qtdsobra = qtd.
*          ENDIF.
*
*          CLEAR:<_tsaida2>-qtdsaldo.
*          <_tsaida2>-qtdsaldo = ( ( <_tsaida2>-qtdpedid - <_tsaida2>-qtdfatur ) + <_tsaida2>-qtdsobra ).
*
*          IF <_tsaida2>-qtdsaldo < 0.
*            MESSAGE msg_qtd_erro TYPE 'S' DISPLAY LIKE 'E'.
*            EXIT.
*            gr_table2->refresh( ).
*          ENDIF.
*        ENDIF.
*
*      ELSE.
*        DATA(msg_qtd_pedido) = |Quantidade do pedido não pode ser nula!|.
*        MESSAGE msg_qtd_pedido TYPE 'S' DISPLAY LIKE 'E'.
*        FREE: t_saida2,t_saida2_mem.
*        EXIT.
*        gr_table2->refresh( ).
*      ENDIF.
*    ENDLOOP.
*
*    gr_table2->refresh( ).
ENDMETHOD.

METHOD on_data_changed_finished2.

*    IF t_saida2 IS NOT INITIAL.
**    lr_scell2,
**    lr_scells2,
**    lr_scolumns2,
**    lr_srows2,
**    lr_smode2.
*      PERFORM resultado_total_alv1.
*      PERFORM get_values_alv2.
*      CLEAR: w_saida2.
*      READ TABLE t_saida2 INTO w_saida2 INDEX lr_scell2-row.
*      READ TABLE t_zsdt0306 INTO DATA(W_zsdt0306) INDEX 1.
*
*
*      DATA: w_zlest0055 TYPE zlest0055.
*      CLEAR w_zlest0055.
*
*
*      DATA: aux_matkl TYPE zlest0055-matkl.
*      CLEAR: aux_matkl.
*
*      CONDENSE w_saida1-matkl NO-GAPS.
*      UNPACK w_saida1-matkl TO aux_matkl.
*
*      w_saida2-matnr = w_saida1-matnr.
*      w_saida2-matkl = w_saida1-matkl.
*      w_saida2-bukrs_ped = w_saida1-bukrs.
*      w_saida2-werks_ped = w_saida1-werks.
*
*
*      IF w_saida2-bukrs_fat IS NOT INITIAL
*      AND w_saida2-werks_fat IS NOT INITIAL
*      AND w_saida2-cl_codigo IS NOT INITIAL
*      AND w_saida2-auart IS NOT INITIAL.
*        SELECT SINGLE * FROM zlest0055
*          WHERE status = '1'
*          AND operacao = @w_saida1-operacao
*          AND vkorg = @w_saida2-bukrs_fat
*          AND waerk = @w_saida2-waerk
*          AND kunnr = @w_saida2-cl_codigo
*          AND matkl = @aux_matkl
*          AND auart = @w_saida2-auart
*          INTO @w_zlest0055.
*
*        IF w_zlest0055 IS NOT INITIAL.
*
*          SELECT SINGLE * FROM mara WHERE matnr = @w_saida2-matnr INTO @DATA(lr_mara) .
*          SELECT SINGLE * FROM zsdt0307 WHERE emp_pedido   = @w_saida1-bukrs AND emp_fat_serv = @w_saida2-bukrs_fat INTO @DATA(lr_zsdt0307).
*
*          w_saida2-menge = w_saida2-qtdfatur.
*          w_saida2-kunnr = w_zlest0055-kunnr.
*          w_saida2-gewei = lr_mara-gewei.
*          w_saida2-meins = lr_mara-meins.
*          w_saida2-vkaus = w_zlest0055-vkaus.
*          w_saida2-vtweg = w_zlest0055-vtweg.
*          w_saida2-spart = w_zlest0055-spart.
*          w_saida2-zterm = w_zlest0055-zterm.
*          w_saida2-netpr = w_zlest0055-netpr.
*          w_saida2-kurst = w_zlest0055-kurst.
*          w_saida2-centro_fat_serv = lr_zsdt0307-centro_fat_serv.
*          w_saida2-pedidos = w_saida2-pedidos && W_zsdt0306-ebeln && '/' && W_zsdt0306-ebelp && '-'.
*
*          DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
*
*          CREATE OBJECT obj_zcl_util_sd.
*
*          CASE  w_saida2-waerk.
*            WHEN 'BRL'.
*              l_vlr_brl = ( ( w_saida2-qtdfatur / 1000 ) *  w_saida2-netpr  ).
*              l_gdatu   =  w_saida2-dt_fatura.
*
*              obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
*              obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_saida2-kurst ).
*              obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_saida2-waerk ).
*              obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'USD' ).
*              obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).
*
*              IF l_ukurs IS NOT INITIAL.
*                w_saida2-tax_dolar  = ( l_ukurs * -1 ).
*                l_vlr_usd           = ( l_vlr_brl /  ( l_ukurs * -1 ) ).
*                w_saida2-vlr_brl    = l_vlr_brl.
*                w_saida2-vlr_usd    = l_vlr_usd.
*              ELSE.
*                MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Taxa do câmbio não cadastrada.'.
*                EXIT.
*              ENDIF.
*
*            WHEN 'USD'.
*              l_vlr_usd = ( ( w_saida2-qtdfatur / 1000 ) *  w_zlest0055-netpr  ).
*              l_gdatu   =  w_saida2-dt_fatura.
*
*              obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
*              obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_saida2-kurst ).
*              obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_saida2-waerk ).
*              obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'BRL' ).
*              obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).
*
*              IF l_ukurs IS NOT INITIAL.
*                l_vlr_brl           = l_vlr_usd * l_ukurs.
*                w_saida2-tax_dolar  = l_ukurs.
*                w_saida2-vlr_brl    = l_vlr_brl.
*                w_saida2-vlr_usd    = l_vlr_usd.
*              ELSE.
*                MESSAGE s024(sd) WITH 'Taxa do câmbio não cadastrada.' DISPLAY LIKE 'E'.
*                EXIT.
*              ENDIF.
*
*          ENDCASE.
*
*          MODIFY t_saida2    FROM w_saida2 INDEX lr_scell2-row.
*
*        ENDIF.
*
*
*      ENDIF.
*
*    ENDIF.
*
*    gr_table2->refresh( ).

ENDMETHOD.

ENDCLASS.
