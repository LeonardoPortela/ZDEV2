**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Interface Trace Cotton                                                    |*
**/===========================================================================\*

REPORT zsdr0093.


TABLES: zsdt0165, zsdt0166, sscrfields.

CONSTANTS tx_conv_lib_kl TYPE p LENGTH 5 DECIMALS 4 VALUE '2.2046'.
*
TYPES BEGIN OF ty_0166.
INCLUDE TYPE zsdt0166.

TYPES peso_lote_a TYPE dzmeng.
TYPES peso_lote_r TYPE dzmeng.
TYPES d_status TYPE char50.
TYPES d_cliente TYPE char50.
TYPES d_tamanho_fardo TYPE char50.
TYPES check TYPE char1.
TYPES butxt TYPE butxt.
TYPES acts_contr TYPE zacts.
TYPES END OF ty_0166.


TYPES BEGIN OF ty_trace.
TYPES: id_trace   TYPE numc15,
       lote       TYPE charg_d,
       kunnr      TYPE kunnr,
*       data_takeup TYPE dats,
       safra      TYPE gjahr,
       werks      TYPE werks_d,
       algodoeira TYPE zsdt0166-algodoeira,
       data       TYPE zsdt0166-data,
       hora       TYPE zsdt0166-hora.
TYPES END OF ty_trace.

TYPES BEGIN OF ty_0045.
INCLUDE TYPE zsdt0045.
TYPES: id_trace            TYPE numc15,
       qtd_fardos_vinc     TYPE int4,
       saldo_fardos        TYPE int4,
       qtd_peso_vinc       TYPE dzmeng,
       saldo_peso          TYPE dzmeng,
       terminal_desc       TYPE name1,
       armz_desc           TYPE name1,
       ponto_c_desc        TYPE name1,
       cod_despac_desc     TYPE name1,
       cod_transp_desc     TYPE name1,
       terminal_estuf_desc TYPE name1,
       controladora_desc   TYPE name1,
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
       desc_local_entrega  TYPE name1,
       desc_oper_log       TYPE name1.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
TYPES END OF ty_0045.

TYPES: BEGIN OF type_mchb,
         matnr  TYPE mchb-matnr,
         werks  TYPE mchb-werks,
         lgort  TYPE mchb-lgort,
         charg  TYPE mchb-charg,
         clabs  TYPE mchb-clabs,
         cspem  TYPE mchb-cspem,
         maktx  TYPE makt-maktx,
         lgortr TYPE mchb-lgort,
         chargr TYPE mchb-charg,
         matnc  TYPE mchb-matnr,
       END   OF type_mchb,

       BEGIN OF type_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END   OF type_makt,

       BEGIN OF type_msn,
         tp_msn   TYPE bapi_mtype,
         doc_mat  TYPE bapi2017_gm_head_ret-mat_doc,
         ano      TYPE bapi2017_gm_head_ret-doc_year,
         lote     TYPE charg_d,
         messagem TYPE bapi_msg,
       END   OF type_msn.


DATA: t_mchb        TYPE TABLE OF type_mchb,
      it_msn        TYPE TABLE OF type_msn,
      t_msn         TYPE TABLE OF type_msn,
      t_texto_geral TYPE tsftext,
      v_butxt       TYPE t001-butxt,
      ok_code       TYPE sy-ucomm.

DATA: _0166           TYPE TABLE OF ty_0166,
      w_0166          TYPE ty_0166,
      _0166_total     TYPE TABLE OF ty_0166,
      wa_ins          TYPE zsdt0166,
      _instrucao      TYPE TABLE OF ty_0045,
      w_instrucao_aux TYPE ty_0045,
      w_instrucao     TYPE ty_0045,
      _aux            TYPE TABLE OF zsdt0166,
      it_save         TYPE TABLE OF zsdt0166,
      it_0182         TYPE TABLE OF zsdt0182,
      w_aux           TYPE zsdt0166,
      w_ender_algo    TYPE string,
      w_ender_ie_pt   TYPE string,
      w_ender_ie_en   TYPE string,
      w_ender_provis  TYPE string,
      w_ender_carg_pt TYPE string,
      w_ender_carg_en TYPE string,
      l_endereco_pt   TYPE string,
      l_endereco_en   TYPE string,
      paragrafo       TYPE string,
      it_trace        TYPE TABLE OF ty_trace,
      _fcat           TYPE lvc_t_fcat,
      _container      TYPE REF TO cl_gui_custom_container,
      _function       TYPE ui_functions,
      _grid           TYPE REF TO cl_gui_alv_grid,
      _container1     TYPE REF TO cl_gui_custom_container,
      _grid1          TYPE REF TO cl_gui_alv_grid,
      _layout         TYPE lvc_s_layo,
      _save           TYPE c,
      _stable         TYPE lvc_s_stbl VALUE 'XX',
      c_alv_tm        TYPE REF TO cl_alv_grid_toolbar_manager,
      _toolbar        TYPE stb_button,
      _rows           TYPE lvc_t_row,
      _variant        TYPE disvariant,
      var_tipo        TYPE char200,
      t_fieldcat      TYPE   slis_t_fieldcat_alv,
      w_stable        TYPE lvc_s_stbl.

DATA: w_0143   TYPE zsdt0143.  "*-CS2023000189-05.04.2023-#108694-JT

DATA: _edit           TYPE c,
      wg_mensagem(30),
      tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      _versao         TYPE c VALUE abap_true,
      r_status        TYPE RANGE OF c,
      t_rows          TYPE lvc_t_row,
      w_rows          TYPE lvc_s_row.

DATA: _nro_sol_ov TYPE zsded013,
      vl_contrato TYPE bstkd.

TYPES: BEGIN OF ty_editor,
         line(130),
       END OF ty_editor.

DATA: gt_editor         TYPE TABLE OF ty_editor,
      obj_custom_txt    TYPE REF TO cl_gui_custom_container,
      obj_custom_editor TYPE REF TO cl_gui_textedit.

DATA: wl_header        TYPE thead,
      it_texto         TYPE TABLE OF tline,
      g_new_status     TYPE z_status_trace,
      i                TYPE i,
      wa_texto         TYPE tline,
      w_lfa1_werks1    TYPE lfa1,
      w_lfa1_werks2    TYPE lfa1,
      w_lfa1_werks3    TYPE lfa1,
      w_lfa1_werks4    TYPE lfa1,
      l_lote           TYPE string,
      l_aprove1        TYPE string,
      l_aprove2        TYPE string,
      l_aprove3        TYPE string,
      l_aprove4(10000) TYPE c,
      l_aprove_a       TYPE string,
      l_aprove_b       TYPE string,
      l_aprove_c       TYPE string,
      l_aprove_d       TYPE string,
      l_aprove_e       TYPE string,
      l_cnpj_formatado TYPE char18.

DATA: x_screen TYPE sy-dynnr VALUE '0200'.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_stat FOR zsdt0166-status   NO INTERVALS,
                  s_emp  FOR zsdt0166-empresa  NO INTERVALS, " OBLIGATORY,
                  s_safr FOR zsdt0166-safra    NO INTERVALS,
                  s_lote FOR zsdt0166-lote     NO INTERVALS,
                  s_matn FOR zsdt0166-matnr    NO INTERVALS,
                  s_tipo FOR zsdt0166-tipo     NO INTERVALS,
                  s_werk FOR zsdt0166-werks    NO INTERVALS,
                  s_cont FOR zsdt0166-contrato NO INTERVALS,
                  s_data FOR zsdt0166-data_takeup.
SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN: FUNCTION KEY 2.

CLASS zcl_trace DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_dados,
      set_dados,
      get_log,
      set_catalogo IMPORTING input TYPE char3,
      set_alv IMPORTING input TYPE char3,
      set_seq RETURNING VALUE(return) TYPE int2,
      set_layout,
      get_desc_cliente IMPORTING kunnr         TYPE kunnr
                       RETURNING VALUE(return) TYPE char50, "name1, *-CS2023000189-05.04.2023-#108694-JT
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
      get_desc_local_entrega
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_desc_oper_log
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
      get0143  IMPORTING contrato      TYPE bstkd
                         bukrs         TYPE bukrs
               RETURNING VALUE(return) TYPE zsdt0143,
      get_contrato  IMPORTING contrato      TYPE bstkd
                              bukrs         TYPE bukrs
                              safra         TYPE zsdt0143-safra
                    RETURNING VALUE(return) TYPE zsdt0143,
      smartforms IMPORTING input         TYPE tdsfname,
      geratransferencia IMPORTING input TYPE zsdt0166,
      set_id_smart IMPORTING ucomm         TYPE sy-ucomm
                             input         TYPE zsdt0166
                   RETURNING VALUE(return) TYPE char1,
      getnextid IMPORTING input         TYPE numc1
                RETURNING VALUE(return) TYPE numc10,
      reprocessar,
      get_lfa1 IMPORTING input         TYPE lifnr
               RETURNING VALUE(return) TYPE name1,
      enviar_zsdt0121 IMPORTING input TYPE lvc_t_row,
      show_msgre,
      f4_solov RETURNING VALUE(return) TYPE zsded013,
      popup_to_confirm IMPORTING input         TYPE sy-ucomm
                       RETURNING VALUE(return) TYPE char1,
      get_limite_peso
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_peso_max
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_instrucao
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE zsdt0045,

      get_texto RETURNING VALUE(return) TYPE string,

      dados_cadastrais,

      dados_cadastro_depositos, "*-CS2023000189-02.05.2023-#108747-JT-inicio

      busca_texto_geral IMPORTING i_ucomm TYPE sy-ucomm.

    METHODS:
      constructor IMPORTING io_alv_grid  TYPE REF TO cl_gui_alv_grid,
      on_toolbar  FOR EVENT toolbar               OF cl_gui_alv_grid IMPORTING e_object e_interactive sender,
      menu_button FOR EVENT menu_button OF cl_gui_alv_grid IMPORTING e_object e_ucomm,
      on_handle   FOR EVENT user_command          OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_dt_cnd   FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
      on_dt_cnd_f FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

    METHODS:
*      CONSTRUCTOR1 IMPORTING IO_ALV_GRID  TYPE REF TO CL_GUI_ALV_GRID,
*      ON_TOOLBAR1  FOR EVENT TOOLBAR               OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_INTERACTIVE SENDER,
*      MENU_BUTTON1 FOR EVENT MENU_BUTTON OF CL_GUI_ALV_GRID IMPORTING E_OBJECT E_UCOMM,
      on_handle1   FOR EVENT user_command          OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_dt_cnd1   FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
      on_dt_cnd_f1 FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.

DATA: o_event  TYPE REF TO zcl_trace.

CLASS zcl_trace IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_tm
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD on_toolbar.

    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@42@' function  = 'REFRESH' quickinfo = 'Atualizar'         text = '' butn_type = 0 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@39@' function  = 'PROVI'   quickinfo = 'Gerar Provisional' text = '' butn_type = 1 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@IF@' function  = 'WARE'    quickinfo = 'WareHouse'         text = '' butn_type = 1 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    " APPEND VALUE #( ICON = '@4A@' FUNCTION  = 'RESER'   QUICKINFO = 'Gerar Reserva'     TEXT = '' BUTN_TYPE = 0 ) TO E_OBJECT->MT_TOOLBAR.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@0Q@' function  = 'EDIT'    quickinfo = 'Editar'            text = '' butn_type = 0 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@DH@' function  = 'LOG'     quickinfo = 'Log'               text = '' butn_type = 0 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@9B@' function  = 'REPROC'  quickinfo = 'Reprocessar'       text = '' butn_type = 0 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@BB@' function  = 'IMP_121' quickinfo = 'Criar Instrução'   text = '' butn_type = 0 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = ''     function  = ''        quickinfo = ''                  text = '' butn_type = 3 ) TO e_object->mt_toolbar.



*    LOOP AT E_OBJECT->MT_TOOLBAR ASSIGNING FIELD-SYMBOL(<LS_TOOLBAR>).
*      CASE <LS_TOOLBAR>-FUNCTION.
*        WHEN '&LOCAL&INSERT_ROW'
*          OR '&LOCAL&DELETE_ROW'
*          OR '&LOCAL&APPEND'
*          OR '&LOCAL&COPY_ROW'
*          OR '&REFRESH'
*          OR '&LOCAL&CUT'
*          OR '&LOCAL&COPY'
*          OR '&LOCAL&PASTE'
*          OR '&LOCAL&UNDO'
*          OR '&DETAIL'
*          OR '&CHECK'.
*          DELETE E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
*      ENDCASE.
*    ENDLOOP.
*
*    C_ALV_TM->REORGANIZE( IO_ALV_TOOLBAR = E_OBJECT ).

  ENDMETHOD.

  METHOD menu_button.
    CASE e_ucomm.

      WHEN 'PROVI'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'PROVI1'
            text  = 'Provisional Base'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'PROVI2'
            text  = 'Provisional LDC'.

      WHEN 'WARE'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE1'
            text  = 'Modelo Base'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE2'
            text  = 'Modelo Cargill'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE3'
            text  = 'Modelo Olam'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE4'
            text  = 'Modelo Paul'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE5'
            text  = 'Modelo para Descrever'.

*-CS2021000532-#84613-12.08.2022-JT-inicio
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'WARE6'
            text  = 'Modelo LDC'.
*-CS2021000532-#84613-12.08.2022-JT-fim

    ENDCASE.
  ENDMETHOD.

  METHOD on_handle.

    TYPES BEGIN OF ty_normt.
    TYPES tipo TYPE normt.
    TYPES END OF ty_normt.

    DATA: _tipo  TYPE TABLE OF ty_normt,
          status TYPE c.
*          VAR_TIPO  TYPE ZSDT0166-TIPO.

    CALL METHOD _grid->get_selected_rows
      IMPORTING
        et_index_rows = t_rows. "_rows.  "*-CS2022000332-#79430-02.08.2022-JT-inicio

    _rows[] = t_rows[].

    FREE: _aux, _tipo.


    "BREAK-POINT.

    LOOP AT _rows INTO DATA(wa).
      w_aux = _0166[ wa-index ].
      wa_ins = w_aux.

      IF w_aux-status NE 'A'.
        status = abap_true.
      ENDIF.

      w_aux-nr_warehouse = |{ w_aux-nr_warehouse ALPHA = OUT }|.

      APPEND VALUE #( tipo = w_aux-tipo ) TO _tipo.

      SHIFT w_aux-qtd_fardos LEFT DELETING LEADING '0'.
      APPEND w_aux TO _aux.
    ENDLOOP.

    CLEAR: w_aux-tipo, var_tipo.

    SORT _tipo.
    DELETE ADJACENT DUPLICATES FROM _tipo  COMPARING ALL FIELDS.

    LOOP AT _tipo INTO DATA(w_tipo).

*      IF W_AUX-TIPO IS INITIAL.
      IF var_tipo IS INITIAL.
*        W_AUX-TIPO = W_TIPO-TIPO.
        var_tipo = w_tipo-tipo.
      ELSE.
        var_tipo = |{ var_tipo } - { w_tipo-tipo }|.
*        W_AUX-TIPO = |{ W_AUX-TIPO } - { W_TIPO-TIPO }|.
*        VAR_TIPO = W_AUX-TIPO.
      ENDIF.

    ENDLOOP.

    DATA(it_aux) = _aux[].

    CASE e_ucomm.
      WHEN 'IMP_121'.
        IF status IS NOT INITIAL.
          MESSAGE |Status diferente de Aprovado!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        zcl_trace=>enviar_zsdt0121( _rows ).

      WHEN 'REPROC'.
        zcl_trace=>reprocessar( ).
      WHEN 'LOG'.
        zcl_trace=>get_log( ).
      WHEN 'REFRESH'.
        zcl_trace=>get_dados( ).
      WHEN 'EDIT'.
*        _EDIT = ABAP_TRUE.
        CALL METHOD _grid->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      WHEN 'RESER'.

        IF status IS NOT INITIAL.
          MESSAGE |Status diferente de Aprovado!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        SORT it_aux BY status.
        DELETE ADJACENT DUPLICATES FROM it_aux COMPARING status.

        IF it_aux[ 1 ]-status EQ 'A'.

          LOOP AT _aux INTO w_aux.
            geratransferencia( w_aux ).
          ENDLOOP.
        ELSE.
          MESSAGE |Selecione apenas lote aprovado| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      WHEN 'PROVI1' OR 'PROVI2' OR 'WARE1' OR 'WARE2' OR 'WARE3' OR 'WARE4' OR 'WARE5' OR 'WARE6'.

        IF status IS NOT INITIAL.
          MESSAGE |Status diferente de Aprovado!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CHECK _rows IS NOT INITIAL.

*       SORT it_aux BY kunnr contrato werks data_takeup.
*       DELETE ADJACENT DUPLICATES FROM it_aux COMPARING kunnr contrato werks data_takeup.
        SORT it_aux BY empresa kunnr contrato data_takeup.
        DELETE ADJACENT DUPLICATES FROM it_aux COMPARING empresa kunnr contrato data_takeup.

        IF lines( it_aux ) NE 1.
*         MESSAGE |Lotes Selecionados devem ser do mesmo Cliente, Contrato, Algodoeira e data de Take-up| TYPE 'S' DISPLAY LIKE 'E'.
          MESSAGE |Lotes Selecionados devem ser da mesma Empresa, Cliente, Contrato e data de Take-up| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        LOOP AT _aux INTO DATA(_aux2).
          IF _aux2-valor_total IS INITIAL.
            DELETE _aux INDEX sy-tabix.
          ENDIF.
        ENDLOOP.

        READ TABLE _aux INTO _aux2 INDEX 1.
        IF sy-subrc <> 0.
          MESSAGE |Documento de Contrato sem Preço!| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CHECK set_id_smart( ucomm = e_ucomm input = it_aux[ 1 ] ) IS INITIAL.
        w_aux = _aux[ 1 ].
*        W_AUX-TIPO = VAR_TIPO.
        busca_texto_geral( i_ucomm = e_ucomm ).
        smartforms( input = SWITCH #(  e_ucomm  WHEN 'PROVI1' THEN 'ZSDF0005'
                                                WHEN 'PROVI2' THEN 'ZSDF0015'
                                                WHEN 'WARE1'  THEN 'ZSDF0008'
                                                WHEN 'WARE2'  THEN 'ZSDF0009'
                                                WHEN 'WARE3'  THEN 'ZSDF0010'
                                                WHEN 'WARE4'  THEN 'ZSDF0011'
                                                WHEN 'WARE5'  THEN 'ZSDF0012'
                                                WHEN 'WARE6'  THEN 'ZSDF0016' "*-CS2021000532-#84613-12.08.2022-JT
                    ) ).

    ENDCASE.

    zcl_trace=>set_alv( '100' ).

  ENDMETHOD.

  METHOD on_handle1.

  ENDMETHOD.

  METHOD on_dt_cnd.

*    DATA: VALUE_ TYPE KURSF.
*
*    CHECK ER_DATA_CHANGED->MT_GOOD_CELLS[] IS NOT INITIAL.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(_GOOD) WHERE FIELDNAME EQ 'AGIO_PONTOS'
*                                                               OR FIELDNAME EQ 'DESAGIO_PONTOS'.
*
*      DATA(WA_0166) = _0166[ _GOOD-ROW_ID ].
*
*      VALUE_ = _GOOD-VALUE.
*
*      CASE _GOOD-FIELDNAME.
*        WHEN 'AGIO_PONTOS'.
*          WA_0166-VLR_AGIO         = ( VALUE_ * TX_CONV_LIB_KL ) * WA_0166-PESO_LOTE.
*        WHEN 'DESAGIO_PONTOS'.
*          WA_0166-PRECO_KG_DESAGIO = VALUE_ * TX_CONV_LIB_KL.
*      ENDCASE.
*
*    ENDLOOP.
*
*    WA_0166-VLR_DESAGIO = WA_0166-PRECO_KG_DESAGIO * WA_0166-PESO_LOTE.
*    WA_0166-PRECO_FINAL = WA_0166-PRECO_CTR + WA_0166-AGIO_PONTOS - WA_0166-DESAGIO_PONTOS.
*    WA_0166-VALOR_TOTAL = WA_0166-PRECO_FINAL * TX_CONV_LIB_KL * WA_0166-PESO_LOTE.
*    WA_0166-VALOR_ANTEC = WA_0166-VALOR_TOTAL * ( ZCL_TRACE=>GET0143( CONV #( WA_0166-CONTRATO ) )-PCTGEM_ANT / 100 ).
*    WA_0166-USNAM       = SY-UNAME.
*    WA_0166-DATA_ATUAL  = SY-DATUM.
*    WA_0166-HORA_ATUAL  = SY-UZEIT.
*
*    MODIFY _0166 FROM WA_0166 INDEX _GOOD-ROW_ID.
*
*    ZCL_TRACE=>SET_ALV( '100' ).

  ENDMETHOD.

  METHOD on_dt_cnd_f.

    DATA: value_ TYPE kursf.

    CHECK et_good_cells[] IS NOT INITIAL.

    LOOP AT et_good_cells INTO DATA(_good) WHERE fieldname EQ 'AGIO_PONTOS'
                                              OR fieldname EQ 'DESAGIO_PONTOS'.

      DATA(wa_0166) = _0166[ _good-row_id ].

      value_ = _good-value.

      CASE _good-fieldname.
        WHEN 'AGIO_PONTOS'.
          wa_0166-vlr_agio         = ( value_ * tx_conv_lib_kl ) * wa_0166-peso_lote.
        WHEN 'DESAGIO_PONTOS'.
          wa_0166-preco_kg_desagio = value_ * tx_conv_lib_kl.
      ENDCASE.

    ENDLOOP.

    wa_0166-vlr_desagio = wa_0166-preco_kg_desagio * wa_0166-peso_lote.
    wa_0166-preco_final = wa_0166-preco_ctr + wa_0166-agio_pontos - wa_0166-desagio_pontos.

*&----------------------------------------------------sd - ajuste zsdt0141 bug solto #121723 aoenning

*    IF wa_0166-preco_final > 0.
*      wa_0166-acts_contr = abap_true.
*    ENDIF.

    IF me->get_contrato( contrato = CONV #( wa_0166-contrato ) bukrs = wa_0166-empresa safra = wa_0166-safra )-acts = abap_true AND wa_0166-preco_final IS NOT INITIAL.
      wa_0166-acts_contr = abap_true.
    ELSE.
      wa_0166-acts_contr = abap_false.
    ENDIF.
*&----------------------------------------------------SD - Ajuste ZSDT0141 BUG SOLTO #121723 AOENNING



    wa_0166-valor_total = wa_0166-preco_final * tx_conv_lib_kl * wa_0166-peso_lote.
    wa_0166-valor_antec = wa_0166-valor_total * ( zcl_trace=>get0143( contrato = CONV #( wa_0166-contrato )
                                                                      bukrs    = wa_0166-empresa )-pctgem_ant / 100 ).
    wa_0166-usnam       = sy-uname.
    wa_0166-data_atual  = sy-datum.
    wa_0166-hora_atual  = sy-uzeit.

    MODIFY _0166 FROM wa_0166 INDEX _good-row_id.

    zcl_trace=>set_alv( '100' ).

  ENDMETHOD.

  METHOD on_dt_cnd1.

*    DATA: _CONTRATO TYPE TABLE OF TEXT50,
*          _STATUS   TYPE TABLE OF C.
*    DATA: VALUE_ TYPE NUMC10.

*    _CONTRATO = VALUE #( FOR LS IN _INSTRUCAO ( LS-CONTRATO ) ).
*    _STATUS = VALUE #( FOR LS IN _INSTRUCAO ( LS-STATUS ) ).

    DATA limite_peso TYPE zsdt0045-btgew.

    CHECK er_data_changed->mt_good_cells[] IS NOT INITIAL.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(_good) WHERE fieldname EQ 'SALDO_FARDOS' OR fieldname EQ 'SALDO_PESO'.
      DATA(wa_) = _instrucao[ _good-row_id ].
*      VALUE_ = _GOOD-VALUE.
*
*      IF LINES( _CONTRATO ) NE 1.
*        MESSAGE 'Somente Linhas do mesmo contrato podem ser Selecionada!' TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*
*      IF LINES( _STATUS ) EQ 1.
*        IF _STATUS[ 1 ] NE 'A'.
*          MESSAGE 'Seleção Contem Status Diferente de Aprovado!' TYPE 'S' DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*      ELSE.
*        MESSAGE 'Seleção Contem mais de um Status Diferente!' TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.

      CASE _good-fieldname.
        WHEN 'SALDO_FARDOS'.

          DATA(_limite) = wa_-quantidade - wa_-qtd_fardos_vinc.

          IF  _good-value <= _limite.
            wa_-saldo_fardos = _good-value.
            wa_-saldo_peso = ( wa_-btgew / wa_-quantidade )  * wa_-saldo_fardos.
          ELSE.
            MESSAGE |Quantidade de Fardos não pode ser superior ao Saldo!| TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

        WHEN 'SALDO_PESO'.

          limite_peso = wa_-btgew - wa_-qtd_peso_vinc.

          IF  _good-value <= limite_peso.
*            WA_-SALDO_FARDOS = _GOOD-VALUE.
            wa_-saldo_peso = ( wa_-btgew / wa_-quantidade )  * wa_-saldo_fardos.
          ELSE.
            MESSAGE |Peso não pode ser superior ao Saldo!| TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

      ENDCASE.

      MODIFY _instrucao FROM wa_ INDEX _good-row_id.
      show_msgre( ).
      zcl_trace=>set_alv( '200' ).

    ENDLOOP.

  ENDMETHOD.

  METHOD on_dt_cnd_f1.

  ENDMETHOD.

  METHOD get_dados.

    IF s_stat-low IS NOT INITIAL.
      r_status = VALUE #( (
                            option = 'EQ'
                            sign = 'I'
                            low = s_stat-low(1)
                        ) ).
    ENDIF.

    SELECT * FROM zsdt0166
      INTO CORRESPONDING FIELDS OF TABLE _0166
      WHERE lote  IN s_lote AND
            matnr IN s_matn AND
            tipo  IN s_tipo AND
            werks IN s_werk AND
            safra IN s_safr AND
            contrato IN s_cont AND
            empresa IN s_emp AND
            data_takeup IN s_data.

    LOOP AT _0166 ASSIGNING FIELD-SYMBOL(<fs_0166_tmp>) WHERE data_atual IS INITIAL.
      <fs_0166_tmp>-data_atual = <fs_0166_tmp>-data.
      <fs_0166_tmp>-hora_atual = <fs_0166_tmp>-hora.
    ENDLOOP.

    SORT _0166 BY id data_atual DESCENDING hora_atual DESCENDING.
    DELETE ADJACENT DUPLICATES FROM _0166 COMPARING id.

    IF _0166[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0165 INTO TABLE @DATA(lit_zsdt0165)
        FOR ALL ENTRIES IN @_0166
       WHERE id EQ @_0166-id.

      SORT  lit_zsdt0165 BY id.
      LOOP AT _0166 ASSIGNING FIELD-SYMBOL(<fs_0166>).
        READ TABLE lit_zsdt0165 INTO DATA(lwa_zsdt0165) WITH KEY id = <fs_0166>-id BINARY SEARCH.
        CHECK sy-subrc EQ 0.
        <fs_0166>-data = lwa_zsdt0165-data.
        <fs_0166>-hora = lwa_zsdt0165-hora.
      ENDLOOP.
    ENDIF.

    DATA(lt_0166) = _0166.
    SORT lt_0166 BY empresa.
    DELETE ADJACENT DUPLICATES FROM lt_0166 COMPARING empresa.
    IF ( lt_0166 IS NOT INITIAL ).
      SELECT bukrs, butxt
        INTO TABLE @DATA(lt_t001)
        FROM t001
        FOR ALL ENTRIES IN @lt_0166
        WHERE bukrs EQ @lt_0166-empresa.

      SORT lt_t001 BY bukrs.
    ENDIF.

    LOOP AT _0166 ASSIGNING FIELD-SYMBOL(<f_0066>).

      <f_0066>-d_status = SWITCH #( <f_0066>-status
                            WHEN 'A' THEN 'Aprovado'
                            WHEN 'R' THEN 'Reprovado'
                            WHEN 'S' THEN 'Stand by'
                            WHEN 'N' THEN 'Não Avaliado'
                            ELSE '' ).

      <f_0066>-peso_lote_a = SWITCH #( <f_0066>-status
                               WHEN 'A' THEN <f_0066>-peso_lote
                               WHEN 'S' THEN <f_0066>-peso_lote
                               ELSE 0 ).

      <f_0066>-peso_lote_r = SWITCH #( <f_0066>-status
                               WHEN 'R' THEN <f_0066>-peso_lote
                               ELSE 0 ).

      <f_0066>-d_cliente = get_desc_cliente( <f_0066>-kunnr ).
      <f_0066>-d_tamanho_fardo = SWITCH #( <f_0066>-tamanho_fardo
                                   WHEN 'P' THEN 'Pequeno'
                                   WHEN 'G' THEN 'Grande' ).
      TRY .
          DATA(_trace) = it_trace[
                                   lote        = <f_0066>-lote
                                   kunnr       = <f_0066>-kunnr
*                                   data_takeup = <f_0066>-data_takeup
                                   safra       = <f_0066>-safra
                                   werks       = <f_0066>-werks
                                   algodoeira  = <f_0066>-algodoeira
                                  ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR _trace.
      ENDTRY.

      IF _trace IS INITIAL.
        APPEND VALUE #(
                        id_trace    = <f_0066>-id
                        lote        = <f_0066>-lote
                        kunnr       = <f_0066>-kunnr
*                        data_takeup = <f_0066>-data_takeup
                        safra       = <f_0066>-safra
                        werks       = <f_0066>-werks
                        algodoeira  = <f_0066>-algodoeira
                        data        = <f_0066>-data
                        hora        = <f_0066>-hora
                      ) TO it_trace.
      ELSE.

        DATA(_data_hora_reg_atual)    = <f_0066>-data && <f_0066>-hora.
        DATA(_data_hora_reg_anterior) = _trace-data && _trace-hora.

        IF _data_hora_reg_anterior < _data_hora_reg_atual.
          DELETE it_trace WHERE id_trace = _trace-id_trace.
          APPEND VALUE #(
                          id_trace    = <f_0066>-id
                          lote        = <f_0066>-lote
                          kunnr       = <f_0066>-kunnr
*                          data_takeup = <f_0066>-data_takeup
                          safra       = <f_0066>-safra
                          werks       = <f_0066>-werks
                          algodoeira  = <f_0066>-algodoeira
                        ) TO it_trace.
        ENDIF.

      ENDIF.

      READ TABLE lt_t001 REFERENCE INTO DATA(lo_wa_t001) WITH KEY bukrs = <f_0066>-empresa BINARY SEARCH.
      IF ( sy-subrc IS INITIAL ).
        <f_0066>-butxt = lo_wa_t001->butxt.
      ENDIF.
    ENDLOOP.

    LOOP AT _0166 ASSIGNING <f_0066>.
      IF NOT line_exists( it_trace[ id_trace = <f_0066>-id ] ).
        <f_0066>-check = abap_true.
      ENDIF.
      IF <f_0066>-status EQ 'N' OR ( NOT <f_0066>-status IN r_status ).
        <f_0066>-check = abap_true.
      ENDIF.

*-CS2023000189-05.04.2023-#108694-JT-inicio
      IF zcl_trace=>get_contrato( contrato = CONV #( <f_0066>-contrato ) bukrs = <f_0066>-empresa safra = <f_0066>-safra )-acts = abap_true AND
         <f_0066>-preco_final IS NOT INITIAL.
        <f_0066>-acts_contr = abap_true.
      ELSE.
        <f_0066>-acts_contr = abap_false.
      ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

    ENDLOOP.

    DELETE _0166 WHERE check IS NOT INITIAL.

    _0166_total[] = _0166[].

    SORT _0166 BY lote werks id.

  ENDMETHOD.

  METHOD set_dados.

    DATA: l_acts_true  TYPE char1,
          l_acts_false TYPE char1,
          l_erro       TYPE char1.

*-CS2022000332-#79430-02.08.2022-JT-inicio
*    LOOP AT _0166_total   INTO DATA(w_0166_total).
*      READ TABLE _0166 INTO DATA(w_0166) WITH KEY id = w_0166_total-id.
*      IF sy-subrc <> 0 AND w_0166_total-carga_dados = abap_true.
*        APPEND w_0166_total TO _0166.
*      ENDIF.
*    ENDLOOP.
*-CS2022000332-#79430-02.08.2022-JT-fim

*-CS2023000189-05.04.2023-#108694-JT-inicio
    LOOP AT _0166  INTO DATA(w_0166).
      w_0166-acts     = w_0166-acts_contr.
      MODIFY _0166 FROM w_0166 INDEX sy-tabix.
    ENDLOOP.
*-CS2023000189-05.04.2023-#108694-JT-fim


    it_save = VALUE #( FOR ls IN _0166 ( CORRESPONDING #( ls ) ) ).

    CHECK it_save IS NOT INITIAL.

*-CS2023000189-05.04.2023-#108694-JT-inicio
*-verifica se ha mais de umcontrato com ACTS diferentes entre si
    CLEAR l_erro.

    LOOP AT it_save INTO w_aux.

      CLEAR: l_acts_true, l_acts_false.

      SELECT empresa, contrato, safra, acts
        FROM zsdt0143
        INTO TABLE @DATA(t_143)
       WHERE contrato     = @w_aux-contrato
         AND safra        = @w_aux-safra
         AND empresa      = @w_aux-empresa
         AND cancelado   <> @abap_true.

      IF sy-subrc = 0.
        LOOP AT t_143 INTO DATA(w_143).
          IF w_143-acts = abap_true.
            l_acts_true = abap_true.
          ELSE.
            l_acts_false = abap_true.
          ENDIF.
        ENDLOOP.

        IF l_acts_true = abap_true AND l_acts_false = abap_true.
          l_erro = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_erro = abap_true.
      MESSAGE s024(sd) WITH 'Não Salvo. Há ACTS incompatíveis entre contratos!'
                            'Verifique! Contrato: '
                            w_143-contrato DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

    MODIFY zsdt0166 FROM TABLE it_save.

*    _EDIT = ABAP_FALSE.
    CALL METHOD _grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

  ENDMETHOD.

  METHOD get_log.

    TYPES: BEGIN OF ty_estrutura.
             INCLUDE TYPE slis_fieldcat_main.
             INCLUDE TYPE slis_fieldcat_alv_spec.
    TYPES: END OF ty_estrutura.

    DATA: _lay_    TYPE slis_layout_alv,
          _fcat_   TYPE TABLE OF ty_estrutura,
          data_log TYPE zsdt0178-data_atual.

    _lay_-colwidth_optimize = abap_true.
    _lay_-zebra             = abap_true.

    _fcat_ = VALUE #(
                      ( tabname   = 'IT_LOG'  fieldname = 'ID'          seltext_l = 'Id'       )
                      ( tabname   = 'IT_LOG'  fieldname = 'ID_TRACE'    seltext_l = 'Id Trace' )
                      ( tabname   = 'IT_LOG'  fieldname = 'FIELDNAME'   seltext_l = 'Campo'    )
                      ( tabname   = 'IT_LOG'  fieldname = 'MESSAGEM'    seltext_l = 'Mensagem' )
                      ( tabname   = 'IT_LOG'  fieldname = 'USNAM'       seltext_l = 'Usuário'  )
                      ( tabname   = 'IT_LOG'  fieldname = 'DATA_ATUAL'  seltext_l = 'Data'     )
                      ( tabname   = 'IT_LOG'  fieldname = 'HORA_ATUAL'  seltext_l = 'Hora'     )
                   ).

    data_log = sy-datum - 30.

    SELECT *
      FROM zsdt0178
      INTO TABLE @DATA(it_log)
    WHERE status EQ @abap_false
      AND data_atual GE @data_log
      ORDER BY data_atual DESCENDING.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        is_layout   = _lay_
        it_fieldcat = _fcat_
      TABLES
        t_outtab    = it_log.

  ENDMETHOD.

  METHOD set_catalogo.
    FREE _fcat.

    CASE input.
      WHEN '100'.

        CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
          EXPORTING
            i_program_name         = sy-repid
            i_structure_name       = 'ZSDT0166'
          CHANGING
            ct_fieldcat            = t_fieldcat
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.

        _fcat = VALUE #(
        ( col_pos = set_seq( ) fieldname = 'ID'               scrtext_l = 'Sequencial'      )
        ( col_pos = set_seq( ) fieldname = 'D_STATUS'         scrtext_l = 'Status'          )
        ( col_pos = set_seq( ) fieldname = 'LOTE'             scrtext_l = 'Lote'            )
        ( col_pos = set_seq( ) fieldname = 'SAFRA'            scrtext_l = 'Safra'           )
        ( col_pos = set_seq( ) fieldname = 'KUNNR'            scrtext_l = 'Cód. Cliente'    no_zero = abap_true )
        ( col_pos = set_seq( ) fieldname = 'D_CLIENTE'        scrtext_l = 'Cliente'         )
        ( col_pos = set_seq( ) fieldname = 'ACTS'             scrtext_l = 'ACTS TakeUp'     outputlen = 13 checkbox = abap_true ) "*-CS2023000189-05.04.2023-#108694-JT
        ( col_pos = set_seq( ) fieldname = 'ACTS_CONTR'       scrtext_l = 'ACTS Contrato'   outputlen = 13 checkbox = abap_true ) "*-CS2023000189-05.04.2023-#108694-JT
        ( col_pos = set_seq( ) fieldname = 'MATNR'            scrtext_l = 'Cód. Material'   no_zero = abap_true )
        ( col_pos = set_seq( ) fieldname = 'TIPO'             scrtext_l = 'Tipo'            )
        ( col_pos = set_seq( ) fieldname = 'QTD_FARDOS'       scrtext_l = 'Qtd. Fardos'     no_zero = abap_true )
        ( col_pos = set_seq( ) fieldname = 'PESO_LOTE_A'      scrtext_l = 'Peso Aprovado'   )
        ( col_pos = set_seq( ) fieldname = 'PESO_LOTE_R'      scrtext_l = 'Peso Recusado'   )
        ( col_pos = set_seq( ) fieldname = 'MOTIVO'           scrtext_l = 'Motivo'          )
        ( col_pos = set_seq( ) fieldname = 'WERKS'            scrtext_l = 'Centro'          )
        ( col_pos = set_seq( ) fieldname = 'ALGODOEIRA'       scrtext_l = 'Algodoeira'      )
        ( col_pos = set_seq( ) fieldname = 'CONTRATO'         scrtext_l = 'Contrato'        )
        ( col_pos = set_seq( ) fieldname = 'D_TAMANHO_FARDO'  scrtext_l = 'Tamanho Fardos'  )
        ( col_pos = set_seq( ) fieldname = 'PRECO_CTR'        scrtext_l = 'Preço CTR'       )
        ( col_pos = set_seq( ) fieldname = 'DATA_TAKEUP'      scrtext_l = 'Data take-up'    )
        ( col_pos = set_seq( ) fieldname = 'AGIO_PONTOS'      scrtext_l = 'AGIO Pontos'     edit = abap_true ) "COND #( WHEN _EDIT IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE ) )
        ( col_pos = set_seq( ) fieldname = 'DESAGIO_PONTOS'   scrtext_l = 'DESAGIO Pontos'  edit = abap_true ) "COND #( WHEN _EDIT IS INITIAL THEN ABAP_FALSE ELSE ABAP_TRUE ) )
        ( col_pos = set_seq( ) fieldname = 'VLR_AGIO'         scrtext_l = 'Valor U$ Agio'   )
        ( col_pos = set_seq( ) fieldname = 'PRECO_KG_DESAGIO' scrtext_l = 'Preço em KG Desagio' )
        ( col_pos = set_seq( ) fieldname = 'VLR_DESAGIO'      scrtext_l = 'Valor U$ Desagio')
        ( col_pos = set_seq( ) fieldname = 'PRECO_FINAL'      scrtext_l = 'Preço Final'     )
        ( col_pos = set_seq( ) fieldname = 'VALOR_TOTAL'      scrtext_l = 'Valor U$'        )
        ( col_pos = set_seq( ) fieldname = 'VALOR_ANTEC'      scrtext_l = 'Antecipação'     )
        ( col_pos = set_seq( ) fieldname = 'NR_PROVISIONAL'   scrtext_l = 'Nº Provisional'  )
        ( col_pos = set_seq( ) fieldname = 'NR_WAREHOUSE'     scrtext_l = 'Nº Warehouse'    )
        ( col_pos = set_seq( ) fieldname = 'MODEL_WH'         scrtext_l = 'Nº Modelo'       )
        ( col_pos = set_seq( ) fieldname = 'EMPRESA'          scrtext_l = 'Empresa'         )
        ( col_pos = set_seq( ) fieldname = 'BUTXT'            scrtext_l = 'Nome Empresa'    )
        ( col_pos = set_seq( ) fieldname = 'CARGA_DADOS'      scrtext_l = 'Importado'       checkbox = abap_true )
        ).

        LOOP AT _fcat INTO DATA(_wfcat).
          DATA(l_tabix) = sy-tabix.
          READ TABLE t_fieldcat INTO DATA(w_fcat) WITH KEY fieldname = _wfcat-fieldname.

          CASE sy-subrc.
            WHEN 0.
              _wfcat-datatype  = w_fcat-datatype.
              _wfcat-inttype   = w_fcat-inttype.
              _wfcat-intlen    = w_fcat-intlen.
              _wfcat-lowercase = w_fcat-lowercase.
              MODIFY _fcat FROM _wfcat INDEX l_tabix.

            WHEN OTHERS.
              CASE _wfcat-fieldname.
                WHEN 'D_STATUS' OR 'D_TAMANHO_FARDO'.
                  _wfcat-lowercase = abap_true.
                  MODIFY _fcat FROM _wfcat INDEX l_tabix.
                WHEN 'BUTXT'.
                  _wfcat-datatype  = 'CHAR'.
                  _wfcat-inttype   = 'C'.
                  _wfcat-intlen    = 35.
                  _wfcat-lowercase = abap_true.
                  MODIFY _fcat FROM _wfcat INDEX l_tabix.
              ENDCASE.
          ENDCASE.
        ENDLOOP.

      WHEN '200'.

        _fcat = VALUE #(
                        ( col_pos = set_seq( ) fieldname = 'ID'               scrtext_l = 'Sequencial' )
                        ( col_pos = set_seq( ) fieldname = 'OBJEK'            scrtext_l = 'NroSolOV' )
                        ( col_pos = set_seq( ) fieldname = 'BUKRS'            scrtext_l = 'Empresa' )
                        ( col_pos = set_seq( ) fieldname = 'CHARG'            scrtext_l = 'Lote(Deposito)' )
                        ( col_pos = set_seq( ) fieldname = 'WERKS'            scrtext_l = 'Centro' )
                        ( col_pos = set_seq( ) fieldname = 'INSTRUCAO'        scrtext_l = 'Instrução' )
                        ( col_pos = set_seq( ) fieldname = 'DATA_INSTR'       scrtext_l = 'Dt.Instrução'      no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'CONTRATO'         scrtext_l = 'Contrato' )
                        ( col_pos = set_seq( ) fieldname = 'DATA_RETIRADA'    scrtext_l = 'Dt.Retirada'       no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'DEADLINE_DRAFT'   scrtext_l = 'Dt.DD.Draft'       no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'DEADLINE_DOCUMEN' scrtext_l = 'Dt.DD.Carga'       no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'DATA_PORTO'       scrtext_l = 'Dt.FinalPorto'     no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'DATA_IN_PORTO'    scrtext_l = 'Dt.Inicial Porto'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'DATA_ETA'         scrtext_l = 'Dt. do ETA'        no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'SAFRA'            scrtext_l = 'Safra'             no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'QUANTIDADE'       scrtext_l = 'Quantidade' no_zero = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'QTD_FARDOS_VINC'  scrtext_l = 'Qtd Fardos Vinculados' )
                        ( col_pos = set_seq( ) fieldname = 'SALDO_FARDOS'     scrtext_l = 'Qtd Fardos à Vincular' edit = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'VOLEH'            scrtext_l = 'UNQtd' )
                        ( col_pos = set_seq( ) fieldname = 'MATNR'            scrtext_l = 'Material'  no_out = _versao no_zero = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'DMBTR'            scrtext_l = 'MontanteMI' )
                        ( col_pos = set_seq( ) fieldname = 'PMEIN'            scrtext_l = 'UMpreço' )
                        ( col_pos = set_seq( ) fieldname = 'BTGEW'            scrtext_l = 'Peso total')
                        ( col_pos = set_seq( ) fieldname = 'QTD_PESO_VINC'    scrtext_l = 'Qtd Peso Vinculado' )
                        ( col_pos = set_seq( ) fieldname = 'SALDO_PESO'       scrtext_l = 'Qtd Peso a Vincular'  edit = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'GEWEI'            scrtext_l = 'Unidade de peso' )
                        ( col_pos = set_seq( ) fieldname = 'PORTO_EMBARQUE'   scrtext_l = 'PortoEmbarque'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'TERMINAL'         scrtext_l = 'Terminal'  no_out = _versao no_zero = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'PONTO_C'          scrtext_l = 'Ponto de Coleta' no_zero = abap_true edit = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'BOOKING'          scrtext_l = 'Booking'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'MAPA'             scrtext_l = 'Mapa'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'FUMIGACAO'        scrtext_l = 'Fumigação'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'HRS_FGACAO'       scrtext_l = 'Horas Fumigação'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'ARMADOR'          scrtext_l = 'Armador'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'FREE_TIME'        scrtext_l = 'FreeTime'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'QTD_CTNERS'       scrtext_l = 'QtdContainers'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'COD_DESPACH'      scrtext_l = 'Despachante'  no_out = _versao no_zero = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'COD_TRANSP'       scrtext_l = 'Transporta'  no_out = _versao no_zero = abap_true )
                        ( col_pos = set_seq( ) fieldname = 'VLR_FRETE'        scrtext_l = 'ValorFrete'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'LIMITE_PESO'      scrtext_l = 'LimitePeso'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'PESO_MAX'         scrtext_l = 'PesoMaximo'  no_out = _versao )
                        ( col_pos = set_seq( ) fieldname = 'OBSERVACAO'       scrtext_l = 'Observação'  no_out = _versao )
*** CSCS2020001361 - Inicio Camila Brand
                        ( col_pos = set_seq( ) fieldname = 'ARMAZENAGEM'    scrtext_l = 'Armazém Terceiro'   )
                        ( col_pos = set_seq( ) fieldname = 'LOTE_ARMZ'      scrtext_l = 'Lote Armazenagem'   )
                        ( col_pos = set_seq( ) fieldname = 'COD_ARMZ'       scrtext_l = 'Armazém'   )
*** CSCS2020001361 - Fim  - Camila Brand
        ).


    ENDCASE.

  ENDMETHOD.

  METHOD set_alv.

    w_stable-row  = abap_true.
    w_stable-col  = abap_true.

    set_layout( ).
    set_catalogo( input ).

    CASE input.
      WHEN '100'.

        IF _container IS INITIAL.

          CREATE OBJECT _container
            EXPORTING
              container_name = 'CC_'.

          CREATE OBJECT _grid
            EXPORTING
              i_parent = _container.

          CREATE OBJECT o_event
            EXPORTING
              io_alv_grid = _grid.

          SET HANDLER: o_event->on_toolbar  FOR _grid,
                       o_event->menu_button FOR _grid,
                       o_event->on_handle   FOR _grid,
                       o_event->on_dt_cnd   FOR _grid,
                       o_event->on_dt_cnd_f FOR _grid.

          CALL METHOD _grid->set_table_for_first_display
            EXPORTING
              is_variant      = _variant
              is_layout       = _layout
              i_save          = _save
              i_default       = abap_true
            CHANGING
              it_fieldcatalog = _fcat
              it_outtab       = _0166.

          CALL METHOD _grid->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.

          CALL METHOD _grid->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD _grid->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_enter.

        ELSE.

*          CALL METHOD _GRID->SET_FRONTEND_FIELDCATALOG
*            EXPORTING
*              IT_FIELDCATALOG = _FCAT.

          CALL METHOD _grid->refresh_table_display
            EXPORTING
              is_stable = _stable.

*-CS2022000332-#79430-02.08.2022-JT-inicio
          IF lines( t_rows ) > 0.
            CALL METHOD _grid->set_selected_rows
              EXPORTING
                it_index_rows = t_rows.
          ENDIF.
*-CS2022000332-#79430-02.08.2022-JT-fim
        ENDIF.

      WHEN '200'.

*        SET_CATALOGO( INPUT ).

        IF _container1 IS INITIAL.

          CREATE OBJECT _container1
            EXPORTING
              container_name = 'CC_INS'.

          CREATE OBJECT _grid1
            EXPORTING
              i_parent = _container1.

          CREATE OBJECT o_event
            EXPORTING
              io_alv_grid = _grid1.

          SET HANDLER: "O_EVENT->ON_TOOLBAR1  FOR _GRID1,
                       "O_EVENT->MENU_BUTTON1 FOR _GRID1,
                       "O_EVENT->ON_HANDLE1   FOR _GRID1,
                       o_event->on_dt_cnd1   FOR _grid1,
                       o_event->on_dt_cnd_f1 FOR _grid1.

          _function = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

          CALL METHOD _grid1->set_table_for_first_display
            EXPORTING
              it_toolbar_excluding = _function
              is_variant           = _variant
              is_layout            = _layout
              i_save               = _save
              i_default            = abap_true
*             I_STRUCTURE_NAME     = 'ZSDT0045'
            CHANGING
              it_fieldcatalog      = _fcat
              it_outtab            = _instrucao.

*          CALL METHOD _GRID1->SET_READY_FOR_INPUT
*            EXPORTING
*              I_READY_FOR_INPUT = 1.

          CALL METHOD _grid1->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD _grid1->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_enter.

        ELSE.

          CALL METHOD _grid1->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = _fcat.

          CALL METHOD _grid1->refresh_table_display
            EXPORTING
              is_stable = _stable.
        ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD set_seq.
    DATA(qtd) = lines( _fcat ).
    ADD qtd TO return.
    ADD 1 TO return.
  ENDMETHOD.

  METHOD set_layout.

    _save = abap_true.
    _variant-report = sy-repid.

    _layout = VALUE #(
                       zebra      = abap_true
*                       NO_ROWMARK = ABAP_TRUE
                       no_toolbar = abap_false
                       cwidth_opt = abap_true
                       sel_mode   = 'C'
                     ).
  ENDMETHOD.

  METHOD get_desc_cliente.

*-CS2023000189-05.04.2023-#108694-JT-inicio
*   SELECT SINGLE name1 FROM kna1 INTO return WHERE kunnr EQ kunnr.
    SELECT SINGLE adrnr FROM kna1 INTO @DATA(l_adrnr) WHERE kunnr EQ @kunnr.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM adrc
      INTO @DATA(w_adrc)
     WHERE addrnumber EQ @l_adrnr
       AND date_to    >= @sy-datum.

    CHECK sy-subrc = 0.

    return = w_adrc-name1 && w_adrc-name2.
*-CS2023000189-05.04.2023-#108694-JT-fim

  ENDMETHOD.

  METHOD get0143.
    CLEAR return.

    SELECT SINGLE *
      FROM zsdt0143
      INTO return
      WHERE contrato  EQ contrato
        AND empresa   EQ bukrs
        AND cancelado <> abap_true.
  ENDMETHOD.

  METHOD get_contrato.
    CLEAR return.

    SELECT SINGLE *
      FROM zsdt0143
      INTO return
      WHERE contrato     = contrato
        AND safra        = safra
        AND empresa      = bukrs
*       AND visao        = 'P'
*       AND intercompany = 'X'
        AND cancelado   <> abap_true.
  ENDMETHOD.

  METHOD smartforms.

    DATA: vl_name      TYPE rs38l_fnam,
          ls_options   TYPE ssfcompop,
          control      TYPE ssfctrlop,
          wa_143       TYPE zsdt0143,
          t_166        TYPE TABLE OF ty_0166,
          t_166_app    TYPE TABLE OF ty_0166,
          t_mcod3      TYPE TABLE OF lfa1,
          t_stcd1      TYPE TABLE OF lfa1,
          l_formulario TYPE char1,
          l_lifnr      TYPE lifnr,
          l_tabix      TYPE sy-tabix,
          l_input      TYPE tdsfname.

    SELECT *
      FROM zsdt0143
      INTO wa_143
        UP TO 1 ROWS
     WHERE contrato     = w_aux-contrato
       AND safra        = w_aux-safra
       AND empresa      = w_aux-empresa
       AND visao        = 'P'
*      AND intercompany = 'X'
       AND cancelado   <> abap_true.
    ENDSELECT.
    IF sy-subrc <> 0.
      SELECT *
        FROM zsdt0143
        INTO wa_143
          UP TO 1 ROWS
       WHERE contrato     = w_aux-contrato
         AND safra        = w_aux-safra
         AND empresa      = w_aux-empresa
         AND visao        = 'E'
*        AND intercompany = 'X'
         AND cancelado   <> abap_true.
      ENDSELECT.
    ENDIF.

*------------------------------------
*-- enderecos algodoeira
*------------------------------------
    FREE: t_166.
    LOOP AT _rows INTO DATA(w_row).
      w_aux = _0166[ w_row-index ].
      APPEND w_aux  TO t_166.
    ENDLOOP.

    t_166_app[] = t_166[].

    SORT t_166 BY werks.
    DELETE ADJACENT DUPLICATES FROM t_166
                          COMPARING werks.


    DATA(l_lines) = lines( t_166 ).

    FREE: w_ender_algo, w_ender_provis, t_mcod3, t_stcd1,
          w_ender_ie_pt, w_ender_ie_en.

    LOOP AT t_166 INTO w_aux.
      l_tabix = sy-tabix.
      l_lifnr = |{ w_aux-werks ALPHA = IN }|.

      SELECT SINGLE *
        INTO @DATA(w_lfa1)
        FROM lfa1
       WHERE lifnr = @l_lifnr.

      CHECK sy-subrc = 0.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_lfa1-stcd1
        IMPORTING
          output = l_cnpj_formatado.

      CASE w_lfa1-lifnr.
        WHEN '0000001507'.
          l_endereco_pt = 'Tucunaré, Rod. MT 235 KM 133, no município de Sapezal-MT'.
          l_endereco_en = 'Tucunaré at Rod. MT 235 KM 133, at city in Sapezal-MT'.
        WHEN '0000001520'.
          l_endereco_pt = 'Água Quente Estrada SZ-01 KM 34 – margem esquerda, no município de Sapezal-MT'.
          l_endereco_en = 'Água Quente Road SZ-01 KM 34 – right side , at city in Sapezal-MT'.
        WHEN '0000001521'.
          l_endereco_pt = 'Itamarati Rod. BR 364 entroncamento com MT 358, Munícipio Campo Novo do Parecis-MT'.
          l_endereco_en = 'Itamarati at Rod. BR 364 junction with MT 358, City at in Campo Novo do Parecis-MT'.
        WHEN OTHERS.
          l_endereco_pt = ''.
          l_endereco_en = ''.
      ENDCASE.

      IF l_endereco_pt IS NOT INITIAL.
        CONCATENATE w_ender_carg_pt l_endereco_pt ','
               INTO w_ender_carg_pt SEPARATED BY space.
        CONCATENATE w_ender_carg_en l_endereco_en ','
               INTO w_ender_carg_en SEPARATED BY space.
      ENDIF.

      IF l_tabix = 1.
        CONCATENATE w_ender_ie_pt  'Inscrito no CNPJ:' l_cnpj_formatado 'e I.E. sob número'
                    w_lfa1-stcd3   ','
               INTO w_ender_ie_pt  SEPARATED BY space.
        CONCATENATE w_ender_ie_en  'Subscribed on the CNPJ:' l_cnpj_formatado 'and I.E. number'
                    w_lfa1-stcd3   ','
               INTO w_ender_ie_en  SEPARATED BY space.
      ELSE.
        CONCATENATE w_ender_ie_pt  'CNPJ:' l_cnpj_formatado 'e I.E. sob número'
                    w_lfa1-stcd3   ','
               INTO w_ender_ie_pt  SEPARATED BY space.
        CONCATENATE w_ender_ie_en  'CNPJ:' l_cnpj_formatado 'and I.E. number'
                    w_lfa1-stcd3   ','
               INTO w_ender_ie_en  SEPARATED BY space.
      ENDIF.

      IF l_tabix = l_lines.
        CONCATENATE w_ender_algo   w_lfa1-name4 '-' w_lfa1-stras '-' 'CEP: ' w_lfa1-pstlz '-'
                                   w_lfa1-mcod3 '-' w_lfa1-regio '.'
               INTO w_ender_algo   SEPARATED BY space.
      ELSE.
        CONCATENATE w_ender_algo   w_lfa1-name4 '-' w_lfa1-stras '-' 'CEP:'  w_lfa1-pstlz '-'
                                   w_lfa1-mcod3 '-' w_lfa1-regio ','
               INTO w_ender_algo   SEPARATED BY space.
      ENDIF.

      APPEND w_lfa1  TO t_mcod3.
      APPEND w_lfa1  TO t_stcd1.
    ENDLOOP.

    SORT t_mcod3 BY mcod3.
    DELETE ADJACENT DUPLICATES FROM t_mcod3
                          COMPARING mcod3.

    l_lines = lines( t_mcod3 ).

    LOOP AT t_mcod3 INTO DATA(w_mcod3).
      l_tabix = sy-tabix.
      IF l_tabix = l_lines.
        CONCATENATE w_ender_provis w_mcod3-mcod3
               INTO w_ender_provis SEPARATED BY space.
      ELSE.
        CONCATENATE w_ender_provis w_mcod3-mcod3 ','
               INTO w_ender_provis SEPARATED BY space.
      ENDIF.
    ENDLOOP.

*------------------------------------------
* enderecos vendedor
* jaime
*------------------------------------------
    SORT t_stcd1 BY stcd1.
    DELETE ADJACENT DUPLICATES FROM t_stcd1
                          COMPARING stcd1.

    l_lines = lines( t_stcd1 ).

    FREE: w_lfa1_werks1, w_lfa1_werks2,
          w_lfa1_werks3, w_lfa1_werks4,
          l_tabix.

    LOOP AT t_stcd1 INTO DATA(w_stcd1).
      SELECT SINGLE *
        INTO w_lfa1
        FROM lfa1
       WHERE lifnr = w_stcd1-lifnr.

      CHECK sy-subrc = 0.

      SELECT SINGLE *
               INTO @DATA(w_t005t)
               FROM t005t
              WHERE land1 = @w_lfa1-land1
                AND spras = @sy-langu.

      l_tabix = l_tabix + 1.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = w_lfa1-stcd1
        IMPORTING
          output = l_cnpj_formatado.

      CASE l_tabix.
        WHEN 1.
          w_lfa1_werks1-name3 = l_cnpj_formatado.
          w_lfa1_werks1-stcd3 = w_lfa1-stcd3.
          w_lfa1_werks1-stras = w_lfa1-stras.
          w_lfa1_werks1-ort01 = w_lfa1-ort01.
          w_lfa1_werks1-regio = w_lfa1-regio.
          w_lfa1_werks1-name4 = w_t005t-landx.

        WHEN 2.
          w_lfa1_werks2-name3 = l_cnpj_formatado.
          w_lfa1_werks2-stcd3 = w_lfa1-stcd3.
          w_lfa1_werks2-stras = w_lfa1-stras.
          w_lfa1_werks2-ort01 = w_lfa1-ort01.
          w_lfa1_werks2-regio = w_lfa1-regio.
          w_lfa1_werks2-name4 = w_t005t-landx.

        WHEN 3.
          w_lfa1_werks3-name3 = l_cnpj_formatado.
          w_lfa1_werks3-stcd3 = w_lfa1-stcd3.
          w_lfa1_werks3-stras = w_lfa1-stras.
          w_lfa1_werks3-ort01 = w_lfa1-ort01.
          w_lfa1_werks3-regio = w_lfa1-regio.
          w_lfa1_werks3-name4 = w_t005t-landx.

        WHEN 4.
          w_lfa1_werks4-name3 = l_cnpj_formatado.
          w_lfa1_werks4-stcd3 = w_lfa1-stcd3.
          w_lfa1_werks4-stras = w_lfa1-stras.
          w_lfa1_werks4-ort01 = w_lfa1-ort01.
          w_lfa1_werks4-regio = w_lfa1-regio.
          w_lfa1_werks4-name4 = w_t005t-landx.
      ENDCASE.
    ENDLOOP.

*-----------------------------------------
* montar texto LOTS APROVE - modelo OLAM
*-----------------------------------------
    FREE: l_aprove1, l_aprove2, l_aprove3, l_aprove4,
          l_aprove_a, l_aprove_b, l_aprove_c, l_aprove_d,  l_aprove_e.

    l_aprove1 = 'Lots approved were'.
    l_aprove2 = 'producer'.
    l_aprove3 = 'region'.

    LOOP AT t_166 INTO w_aux.
      l_tabix = sy-tabix.

      FREE: l_lote.

      l_lifnr = |{ w_aux-werks ALPHA = IN }|.

      SELECT SINGLE *
        INTO w_lfa1
        FROM lfa1
       WHERE lifnr = l_lifnr.

      DATA(l_ind) = 0.
      LOOP AT t_166_app INTO DATA(w_0166) WHERE werks = w_aux-werks.
        l_ind = l_ind + 1.
        IF l_ind = 1.
          l_lote = w_0166-lote.
        ELSE.
*         l_lote = l_lote && ',' && w_0166-lote.
          CONCATENATE l_lote ',' w_0166-lote INTO l_lote SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF l_tabix = 1.
        CONCATENATE              l_aprove1 l_lote ',' l_aprove2 w_lfa1-name4 l_aprove3
                    w_lfa1-ort01 w_lfa1-regio '.'
               INTO l_aprove4 SEPARATED BY space.
      ELSE.
        CONCATENATE l_aprove4    l_aprove1 l_lote ',' l_aprove2 w_lfa1-name4 l_aprove3
                    w_lfa1-ort01 w_lfa1-regio '.'
               INTO l_aprove4 SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    l_aprove_a = l_aprove4(255).
    l_aprove_b = l_aprove4+255(255).
    l_aprove_c = l_aprove4+510(255).
    l_aprove_d = l_aprove4+765(255).
    l_aprove_e = l_aprove4+1020(255).

    l_input = input.

    IF input+7 EQ '2'.
      paragrafo = get_texto( ).

      IF paragrafo EQ 'BACK'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
      ENDIF.

    ENDIF.

    IF     l_input = 'ZSDF0005'.
      l_input      = 'ZSDF0015'.
      l_formulario = '1'.
    ELSEIF l_input = 'ZSDF0015'.
      l_formulario = '2'.
    ENDIF.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = l_input
      IMPORTING
        fm_name            = vl_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

*  Impresora
    ls_options-tddest   = 'LOCL'.     "Disposit. saída
    ls_options-tdimmed  = abap_true.  "Saída Imediata
    ls_options-tdnewid  = abap_true.  "Nova Ordem SPOOL
    ls_options-tdcovtitle = |{ SWITCH #( input+6(02) WHEN '08' THEN 'WareHouse_Model_1_'
                                                     WHEN '09' THEN 'WareHouse_Model_2_'
                                                     WHEN '00' THEN 'WareHouse_Model_3_'
                                                     WHEN '01' THEN 'WareHouse_Model_4_'
                                                     WHEN '02' THEN 'WareHouse_Model_5_'
                                                     WHEN '05' THEN 'Provision_Model_1_'
                                                     WHEN '15' THEN 'Provision_Model_2_'
                                       ) }{ sy-uname }_{ sy-datum }_{ sy-uzeit }|. "Titulo
    "PHQL

    CALL FUNCTION vl_name
      EXPORTING
        user_settings           = ' '
        output_options          = ls_options
        header                  = w_aux
        paragrafo               = paragrafo
        i_tipo                  = var_tipo
        ta_texto_geral_dinamico = t_texto_geral
        v_butxt                 = v_butxt
        i_zsdt0143              = wa_143
        i_formulario            = l_formulario
        i_ender_algo            = w_ender_algo
        i_ender_provis          = w_ender_provis
        i_ender_ie_pt           = w_ender_ie_pt
        i_ender_ie_en           = w_ender_ie_en
        i_ender_carg_pt         = w_ender_carg_pt
        i_ender_carg_en         = w_ender_carg_en
        i_aprove1               = l_aprove_a
        i_aprove2               = l_aprove_b
        i_aprove3               = l_aprove_c
        i_aprove4               = l_aprove_d
        i_aprove5               = l_aprove_e
        w_lfa1_werks1           = w_lfa1_werks1
        w_lfa1_werks2           = w_lfa1_werks2
        w_lfa1_werks3           = w_lfa1_werks3
        w_lfa1_werks4           = w_lfa1_werks4
      TABLES
        itens                   = _aux
      EXCEPTIONS
        formatting_error        = 1
        internal_error          = 2
        send_error              = 3
        user_canceled           = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD geratransferencia.

    SELECT matnr werks lgort
           charg clabs cspem
      FROM mchb
      INTO TABLE t_mchb
    WHERE  matnr EQ input-matnr
      AND  werks EQ input-werks
      AND  lgort EQ input-lote
      AND  chjin EQ input-safra.

    DATA: sl_header   TYPE bapi2017_gm_head_01,
          vl_code     TYPE bapi2017_gm_code,
          vl_material TYPE bapi2017_gm_head_ret-mat_doc,
          vl_year     TYPE bapi2017_gm_head_ret-doc_year,
          tl_item     TYPE TABLE OF bapi2017_gm_item_create,
          tl_return   TYPE TABLE OF bapiret2,
          sl_zmmt0008 TYPE zmmt0008.

    REFRESH t_msn.
    CLEAR sl_header.

    SORT t_mchb BY matnr ASCENDING
                   werks ASCENDING
                   lgort ASCENDING
                   charg ASCENDING.

    LOOP AT t_mchb INTO DATA(sl_trans).

      CLEAR: vl_material, vl_year, sl_zmmt0008.
      REFRESH: tl_item, tl_return.

      vl_code = '06'.
      sl_header = VALUE #(
                            pstng_date = sy-datum
                            doc_date   = input-data_takeup
                            header_txt  = input-contrato
                         ).

*---> 19/06/2023 - Migração S4 - DG
      DATA: lv_material_long TYPE matnr40,
            lv_material(18)  TYPE c.
      DATA(v_len) = strlen( sl_trans-matnr ).

      IF v_len > 18.
        lv_material_long = CONV #( sl_trans-matnr ).
      ELSE.
        lv_material       = CONV #( sl_trans-matnr ). "#EC CI_FLDEXT_OK[2215424]
      ENDIF.
*<--- 19/06/2023 - Migração S4 - DG

      tl_item = VALUE #( (
                            move_type  = '344'

*---> 19/06/2023 - Migração S4 - DG
*           material   = sl_trans-matnr
          material      = lv_material
          material_long = lv_material_long
*<--- 19/06/2023 - Migração S4 - DG
                            plant      = sl_trans-werks
                            stge_loc   = sl_trans-lgort
                            batch      = sl_trans-charg
                            entry_qnt  = sl_trans-clabs
                            move_plant = sl_trans-werks
                            move_stloc = sl_trans-lgortr
                            move_mat   = sl_trans-matnc
                            customer   = input-kunnr
                            move_batch = sl_trans-charg
                       ) ).


      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = sl_header
          goodsmvt_code    = vl_code
        IMPORTING
          materialdocument = vl_material
          matdocumentyear  = vl_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      IF NOT vl_material IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        REFRESH tl_return.
        tl_return = VALUE #( (
                             type = 'S'
                             message = |{ TEXT-013 } { vl_material } { TEXT-014 } { vl_year } { TEXT-015 }|
                           ) ).

        it_msn = VALUE #( FOR ls IN  tl_return
                           (
                             tp_msn    = ls-type
                             messagem  = ls-message
                             doc_mat   = vl_material
                             ano       = vl_year
                             lote      = sl_trans-charg
                            )
                        ).
        APPEND LINES OF it_msn TO t_msn.

        IF line_exists( tl_item[ move_type = 'ZA1' ] ).
          sl_zmmt0008 = VALUE #(
                                  werks = sl_trans-werks
                                  lgort = sl_trans-lgort
                                  charg = sl_trans-charg
                                  menge = sl_trans-clabs
                               ).

          MODIFY zmmt0008 FROM sl_zmmt0008.
        ENDIF.
        DELETE t_mchb  WHERE matnr EQ sl_trans-matnr
                         AND werks EQ sl_trans-werks
                         AND lgort EQ sl_trans-lgort
                         AND charg EQ sl_trans-charg.

      ELSE.
*     Retorna Erro
        it_msn = VALUE #( FOR ls IN  tl_return
                           (
                             tp_msn    = ls-type
                             messagem  = ls-message
                             doc_mat   = vl_material
                             ano       = vl_year
                             lote      = sl_trans-charg
                            )
                        ).
        APPEND LINES OF it_msn TO t_msn.
      ENDIF.

      CLEAR: sl_trans.

    ENDLOOP.

    IF NOT t_msn[] IS INITIAL.
      CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
        TABLES
          table    = t_msn
        EXCEPTIONS
          fb_error = 1
          OTHERS   = 2.
    ENDIF.

  ENDMETHOD.

  METHOD set_id_smart.

    DATA w_0166 TYPE zsdt0166.
    DATA l_0166 TYPE ty_0166.
    DATA: p_resp.

    DATA(it_aux) = _aux.
    SORT it_aux.

    CASE ucomm.
      WHEN 'PROVI1' OR 'PROVI2'.

        DELETE ADJACENT DUPLICATES FROM it_aux COMPARING nr_provisional.

        IF lines( it_aux ) EQ 1.
          MOVE-CORRESPONDING it_aux[ 1 ] TO w_0166.
        ELSE.
          IF zcl_trace=>popup_to_confirm( ucomm ) EQ 1.
            MOVE-CORRESPONDING it_aux[ 1 ] TO w_0166.
            w_0166-nr_provisional = ''.
          ELSE.
            return = 1.
            EXIT.
          ENDIF.

        ENDIF.

        w_0166-nr_provisional = COND #( WHEN w_0166-nr_provisional IS INITIAL THEN getnextid( 1 ) ELSE w_0166-nr_provisional ).

      WHEN 'WARE1' OR 'WARE2' OR 'WARE3' OR 'WARE4' OR 'WARE5' OR 'WARE6'.

        DELETE ADJACENT DUPLICATES FROM it_aux COMPARING nr_warehouse.

        IF lines( it_aux ) EQ 1.
          MOVE-CORRESPONDING it_aux[ 1 ] TO w_0166.
        ELSE.
          IF zcl_trace=>popup_to_confirm( ucomm ) EQ 1.
            MOVE-CORRESPONDING it_aux[ 1 ] TO w_0166.
            w_0166-model_wh = ''.
          ELSE.
            return = 1.
            EXIT.
          ENDIF.
        ENDIF.

        w_0166-nr_warehouse = COND #( WHEN w_0166-nr_warehouse IS INITIAL THEN getnextid( 2 ) ELSE w_0166-nr_warehouse ).
        w_0166-nr_warehouse = |{ w_0166-nr_warehouse ALPHA = OUT }|.
        w_0166-model_wh = ucomm+4(1).

    ENDCASE.

    MOVE-CORRESPONDING w_0166 TO l_0166.

    LOOP AT _aux INTO DATA(wa_).

      MODIFY _aux FROM w_0166 TRANSPORTING nr_provisional
                                           nr_warehouse
                                           model_wh
           WHERE id EQ wa_-id.

      MODIFY _0166 FROM l_0166 TRANSPORTING nr_provisional
                                            nr_warehouse
                                            model_wh
           WHERE id EQ wa_-id.

      UPDATE zsdt0166 SET nr_provisional = w_0166-nr_provisional
                          nr_warehouse   = w_0166-nr_warehouse
                          model_wh       = w_0166-model_wh
           WHERE id EQ wa_-id.
    ENDLOOP.


  ENDMETHOD.

  METHOD getnextid.
    CASE input.
      WHEN 1.
        SELECT MAX( nr_provisional ) FROM zsdt0166 INTO return.
        ADD 1 TO return.
      WHEN 2.
        SELECT MAX( nr_warehouse ) FROM zsdt0166 INTO return.
        ADD 1 TO return.
    ENDCASE.
  ENDMETHOD.

  METHOD get_lfa1.
    SELECT SINGLE name1 FROM lfa1 INTO return WHERE lifnr EQ input.
  ENDMETHOD.

*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
  METHOD get_desc_local_entrega.
    DATA: lv_kunnr TYPE kna1-kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = lv_kunnr.

    SELECT SINGLE name1 FROM kna1 INTO value WHERE kunnr EQ lv_kunnr.
  ENDMETHOD.

  METHOD get_desc_oper_log.
    DATA: lv_lifnr TYPE lfa1-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = lv_lifnr.

    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ lv_lifnr.
  ENDMETHOD.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130

  METHOD reprocessar.

    DATA: it_0165           TYPE zsdt0165_t,
          it_0176           TYPE TABLE OF zsdt0176,
          t_zsdt_depara_cen TYPE TABLE OF zsdt_depara_cen.

    DATA: r_algod           TYPE RANGE OF char50,
          r_status          TYPE RANGE OF zsdt0165-status,
          r_status_aux      TYPE RANGE OF zsdt0165-status,
          w_zsdt_depara_cen TYPE zsdt_depara_cen.

    DATA: w_0166  TYPE zsdt0166.


    IF s_werk-low IS NOT INITIAL.

      "Projeto Reestruturação Algodao 2024
      IF s_safr-low IS NOT INITIAL.
        IF s_stat-low IS NOT INITIAL.
          IF s_stat-low EQ 'A' OR s_stat-low EQ 'R' OR s_stat-low EQ 'S'.

            r_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'APROVADO' ) ).
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'REPROVADO' ) ).
            APPEND LINES OF r_status_aux TO r_status.
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'STANDBY' ) ).
            APPEND LINES OF r_status_aux TO r_status.
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'NÃO AVALIANDO' ) ).
            APPEND LINES OF r_status_aux TO r_status.
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'NÃO AVALIADO' ) ).
            APPEND LINES OF r_status_aux TO r_status.
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'NAOAVALIADO' ) ).
            APPEND LINES OF r_status_aux TO r_status.
            r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'RECUSADO' ) ).
            APPEND LINES OF r_status_aux TO r_status.


            SELECT * FROM zsdt0165
             INTO TABLE it_0165
             WHERE rg_atualizado          EQ abap_false
              AND  codigo_filial          IN s_werk
              AND  safra                  IN s_safr
              AND  status                 IN r_status
              AND contrato                IN s_cont.

          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE |Informar no filtro inicial os campos Satus/Safra/Fazenda!| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.



*      SELECT * FROM zsdt0176
*        INTO TABLE it_0176
*        WHERE werks IN s_werk.
*
*      IF it_0176 IS NOT INITIAL.
*
*        IF s_safr-low IS NOT INITIAL.
*          IF s_stat-low IS NOT INITIAL.
*            IF s_stat-low EQ 'A' OR s_stat-low EQ 'R' OR s_stat-low EQ 'S'.
*
*              r_algod =  VALUE #( FOR ls IN it_0176 ( sign = 'I' option = 'EQ' low = ls-empresa ) ).
*
*              r_status = VALUE #( ( sign = 'I' option = 'EQ' low = 'APROVADO' ) ).
*              r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'REPROVADO' ) ).
*              APPEND LINES OF r_status_aux TO r_status.
*              r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'STANDBY' ) ).
*              APPEND LINES OF r_status_aux TO r_status.
*              r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'NÃO AVALIANDO' ) ).
*              APPEND LINES OF r_status_aux TO r_status.
*              r_status_aux = VALUE #( ( sign = 'I' option = 'EQ' low = 'NÃO AVALIADO' ) ).
*              APPEND LINES OF r_status_aux TO r_status.
*
*              SELECT * FROM zsdt0165
*               INTO TABLE it_0165
*               WHERE rg_atualizado EQ abap_false
*                AND  algodoeira    IN r_algod
*                AND  safra         IN s_safr
*                AND  status        IN r_status
*                AND contrato       IN s_cont.
*
*            ENDIF.
*          ENDIF.
*        ELSE.
*          MESSAGE |Informar no filtro inicial os campos Satus/Safra/Fazenda!| TYPE 'S' DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*      ELSE.
*        MESSAGE |Informar no filtro inicial os campos Satus/Safra/Fazenda!| TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
      ""Projeto Reestruturação Algodao 2024
    ENDIF.

*    SELECT *
*      FROM zsdt0165
*      INTO TABLE it_0165
*      WHERE rg_atualizado EQ abap_false.

*    CHECK it_0165 IS NOT INITIAL.

    IF it_0165 IS NOT INITIAL.

      CALL FUNCTION 'ZSD_INTERFACE_TRACECOTTON'
        EXPORTING
          tracecotton = it_0165.

      zcl_trace=>get_dados( ).

    ELSE.
      MESSAGE |Não foram encontrados registros para serem reprocessados, com base nos dados do filtro!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD enviar_zsdt0121.

    DATA: sol_ov TYPE zsded013.
    DATA: _contrato    TYPE TABLE OF text50,
          _status      TYPE TABLE OF c,
          _dmbtr       TYPE c,
          l_acts_true  TYPE char1,
          l_acts_false TYPE char1,
          l_erro       TYPE char1.

    FREE: _instrucao, _contrato, _status, w_instrucao, l_erro.

    LOOP AT input INTO DATA(wa).

      DATA(_local) = _0166[ wa-index ].

      APPEND _local-contrato TO _contrato.
      APPEND _local-status TO _status.

      MOVE-CORRESPONDING _local TO w_instrucao.
      MOVE _local-empresa       TO w_instrucao-bukrs.  "*-CS2021000532-#84613-12.08.2022-JT-inicio

*-CS2023000189-05.04.2023-#108694-JT-inicio
*-verifica se ha mais de umcontrato com ACTS diferentes entre si
      CLEAR: l_acts_true, l_acts_false.

      SELECT empresa, contrato, safra, acts
        FROM zsdt0143
        INTO TABLE @DATA(t_143)
       WHERE contrato     = @_local-contrato
         AND safra        = @_local-safra
         AND empresa      = @_local-empresa
         AND cancelado   <> @abap_true.

      IF sy-subrc = 0.
        LOOP AT t_143 INTO DATA(w_143).
          IF w_143-acts = abap_true.
            l_acts_true = abap_true.
          ELSE.
            l_acts_false = abap_true.
          ENDIF.
        ENDLOOP.

        IF l_acts_true = abap_true AND l_acts_false = abap_true.
          l_erro = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

      sol_ov = |{ w_instrucao-objek ALPHA = IN }|.

      w_instrucao-objek = sol_ov.
      w_instrucao-objecttable = 'ZSDT0051'.

      w_instrucao-ponto_c = _local-werks.

      w_instrucao-data_criacao = sy-datum.
      w_instrucao-usuario = sy-uname.

      w_instrucao-id_trace = _local-id.
      w_instrucao-charg = _local-lote.

      w_instrucao-quantidade = _local-qtd_fardos.
      w_instrucao-voleh = 'FD'.

      w_instrucao-dmbtr = _local-preco_final.
      w_instrucao-pmein = 'LB'.

      w_instrucao-btgew = _local-peso_lote.
      w_instrucao-gewei = 'KG'.

      SELECT SUM( qtd_fardos )
        FROM zsdt0182
        INTO w_instrucao-qtd_fardos_vinc
      WHERE id_trace EQ w_instrucao-id_trace.

      w_instrucao-saldo_fardos = w_instrucao-quantidade - w_instrucao-qtd_fardos_vinc.

      SELECT SUM( peso )
        FROM zsdt0182
        INTO w_instrucao-qtd_peso_vinc
      WHERE id_trace EQ w_instrucao-id_trace.

      w_instrucao-saldo_peso = w_instrucao-btgew - w_instrucao-qtd_peso_vinc.

      IF w_instrucao-dmbtr IS INITIAL.
        _dmbtr = abap_true.
      ENDIF.

      APPEND w_instrucao TO _instrucao.
      CLEAR w_instrucao.
    ENDLOOP.

    w_instrucao-data_instr = sy-datum. " Rubenilson Pereira - 13.02.25 - US164130
    w_instrucao-INCOTERM = 'CIF'.


    SORT _contrato.
    SORT _status.
    DELETE ADJACENT DUPLICATES FROM _contrato COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM _status COMPARING ALL FIELDS.

*    SHOW_MSGRE( ).

    IF lines( _contrato ) NE 1.
      MESSAGE 'Somente Linhas do mesmo contrato podem ser Selecionada!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF lines( _status ) EQ 1.
      IF _status[ 1 ] NE 'A'.
        MESSAGE 'Seleção Contem Status Diferente de Aprovado!' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE 'Seleção Contem mais de um Status Diferente!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF _dmbtr IS NOT INITIAL.
      MESSAGE 'Seleção contem Lote sem preço Final!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*-CS2023000189-05.04.2023-#108694-JT-inicio
    IF l_erro = abap_true.
      READ TABLE _contrato INTO DATA(_wcontrato) INDEX 1.
      MESSAGE s024(sd) WITH 'Há ACTS incompatíveis entre contratos: ' _wcontrato DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

    CHECK tg_msg_ret[] IS INITIAL.

    x_screen = '300'.
    CALL SCREEN 0100.

  ENDMETHOD.

  METHOD show_msgre.

    DATA: _contrato    TYPE TABLE OF text50,
          _status      TYPE TABLE OF c,
          _preco       TYPE c,
          tabix        TYPE sy-tabix,
          _limite      TYPE p DECIMALS 2,
          l_acts_true  TYPE char1,
          l_acts_false TYPE char1,
          l_erro       TYPE char1.

    FREE tg_msg_ret.

    IF w_instrucao-objek IS NOT INITIAL.
      _nro_sol_ov = |{ w_instrucao-objek ALPHA = IN }|.

      IF vl_contrato IS INITIAL.
        vl_contrato = wa_ins-contrato.
      ENDIF.

      SELECT COUNT(*) FROM zsdt0051 WHERE nro_sol_ov EQ _nro_sol_ov AND bstkd EQ vl_contrato.
      IF sy-subrc IS NOT INITIAL.
        APPEND VALUE #(
                        field = 'W_INSTRUCAO-NRO_SOL_OV'
                        msg = |Contrato da Solicitação informada difere do Contrato dos Lotes!|
                       ) TO tg_msg_ret.
      ENDIF.
    ENDIF.

*    _CONTRATO = VALUE #( FOR LS IN _INSTRUCAO ( LS-CONTRATO ) ).
*    _STATUS = VALUE #( FOR LS IN _INSTRUCAO ( LS-STATUS ) ).

*    SORT _CONTRATO.
*    SORT _STATUS.
*    DELETE ADJACENT DUPLICATES FROM _CONTRATO COMPARING ALL FIELDS.
*    DELETE ADJACENT DUPLICATES FROM _STATUS COMPARING ALL FIELDS.
*
*    IF LINES( _CONTRATO ) NE 1.
*      APPEND VALUE #(
*                      FIELD = 'W_INSTRUCAO-PRECO_FINAL'
*                      MSG = |Somente Linhas do mesmo contrato podem ser Selecionada!|
*                     ) TO TG_MSG_RET.
*    ENDIF.
*
*    IF LINES( _STATUS ) EQ 1.
*      IF _STATUS[ 1 ] NE 'A'.
*        APPEND VALUE #(
*                        FIELD = 'W_INSTRUCAO-PRECO_FINAL'
*                        MSG = |Seleção Contem Status Diferente de Aprovado!|
*                       ) TO TG_MSG_RET.
*      ENDIF.
*    ELSE.
*      APPEND VALUE #(
*                      FIELD = 'W_INSTRUCAO-PRECO_FINAL'
*                      MSG = |Seleção Contem mais de um Status Diferente!|
*                     ) TO TG_MSG_RET.
*    ENDIF.

    IF x_screen EQ '300'.

      LOOP AT _instrucao INTO DATA(_local).
        tabix = sy-tabix.

*      IF _LOCAL-DMBTR IS INITIAL.
*        APPEND VALUE #(
*                        FIELD = 'W_INSTRUCAO-PRECO_FINAL'
*                        MSG = |Seleção contem Lote sem preço Final! Linha{ TABIX }|
*                       ) TO TG_MSG_RET.
*      ENDIF.

        _limite = _local-quantidade - _local-qtd_fardos_vinc.

        IF  _local-saldo_fardos <= _limite.
        ELSE.
          APPEND VALUE #(
                          field = 'W_INSTRUCAO-PRECO_FINAL'
                          msg = |Quantidade de Fardos não pode ser superior ao Saldo! Linha { tabix }|
                         ) TO tg_msg_ret.
        ENDIF.

        _limite = _local-btgew - _local-qtd_peso_vinc.

        IF _local-saldo_peso <= _limite.
        ELSE.
          APPEND VALUE #(
                          field = 'W_INSTRUCAO-PRECO_FINAL'
                          msg = |Peso não pode ser superior ao Saldo! Linha { tabix }|
                         ) TO tg_msg_ret.
        ENDIF.

        IF _local-saldo_fardos IS INITIAL.
          APPEND VALUE #(
                          field = 'SALDO_FARDOS'
                          msg = |Quantidade à Vincular não pode ser Zero! Linha { tabix }|
                         ) TO tg_msg_ret.
        ENDIF.

*** CS2020001361 - Inicio - Camila Brand
        IF _local-armazenagem = 'X' AND (   _local-lote_armz IS INITIAL AND _local-cod_armz IS INITIAL ).
          APPEND VALUE #(
                          field = 'LOTE_ARMZ'
                          msg = |Lote Armazenagem não informado! Linha { tabix }|
                         ) TO tg_msg_ret.

          APPEND VALUE #(
                field = 'COD_ARMZ'
                msg = |Código Armazém não informado! Linha { tabix }|
               ) TO tg_msg_ret.

        ENDIF.
*** CS2020001361 - Fim - Camila Brand

*-US82037-28.06.2022-JT-inicio
        SELECT vkorg
          INTO @DATA(l_vkorg)
          FROM zsdt0051
            UP TO 1 ROWS
         WHERE nro_sol_ov EQ @_nro_sol_ov
           AND bstkd      EQ @vl_contrato.
        ENDSELECT.
        IF sy-subrc <> 0.
          CLEAR l_vkorg.
        ENDIF.
        IF _local-bukrs <> l_vkorg.
          APPEND VALUE #(
                          field = 'W_INSTRUCAO-NRO_SOL_OV'
                          msg = |Empresa da Solicitação diferente Organização de Vendas do Contrato!|
                         ) TO tg_msg_ret.
        ENDIF.
*-US82037-28.06.2022-JT-inicio

      ENDLOOP.

    ENDIF.

*-CS2023000189-05.04.2023-#108694-JT-inicio
*-verifica se ha mais de umcontrato com ACTS diferentes entre si
*    CLEAR l_erro.
*
*    LOOP AT _0166 INTO w_aux.
*
*      CLEAR: l_acts_true, l_acts_false.
*
*      SELECT empresa, contrato, safra, acts
*        FROM zsdt0143
*        INTO TABLE @DATA(t_143)
*       WHERE contrato     = @w_aux-contrato
*         AND safra        = @w_aux-safra
*         AND empresa      = @w_aux-empresa
*         AND cancelado   <> @abap_true.
*
*      IF sy-subrc = 0.
*        LOOP AT t_143 INTO DATA(w_143).
*          IF w_143-acts = abap_true.
*            l_acts_true = abap_true.
*          ELSE.
*            l_acts_false = abap_true.
*          ENDIF.
*        ENDLOOP.
*
*        IF l_acts_true = abap_true AND l_acts_false = abap_true.
*          l_erro = abap_true.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF l_erro = abap_true.
*      APPEND VALUE #( field = 'W_INSTRUCAO-NRO_SOL_OV'
*                      msg = |Há ACTS incompatíveis entre contratos!|
*                    ) TO tg_msg_ret.
*    ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_show      = COND #( WHEN sy-ucomm EQ 'SHOW_MSGRE' THEN abap_true ELSE abap_false )
        i_repid     = sy-repid
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.


  ENDMETHOD.

  METHOD f4_solov.

    DATA: it_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc.

    TYPES: BEGIN OF ty_solov,
             nro_sol_ov TYPE zsdt0051-nro_sol_ov,
             vkbur      TYPE zsdt0051-vkbur,
             auart      TYPE zsdt0051-auart,
             inco1      TYPE zsdt0051-inco1,
             bstkd      TYPE zsdt0051-bstkd,
           END OF ty_solov.

    DATA: it_solov TYPE  TABLE OF ty_solov.

    SELECT nro_sol_ov vkbur auart inco1 bstkd
      FROM zsdt0051
      INTO TABLE it_solov
      WHERE param_espec EQ 'A'
         OR param_espec EQ 'X'
*      AND tp_venda IN ('14', '15' )
      ORDER BY nro_sol_ov DESCENDING.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NRO_SOL_OV'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = it_solov
        return_tab      = it_return
        dynpfld_mapping = tl_dselc.

    TRY .
        return = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR return.
    ENDTRY.

  ENDMETHOD.

  METHOD popup_to_confirm.

    DATA(msg) = COND #( WHEN input EQ 'PROVI' THEN 'Provisional' ELSE 'Warehouse' ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = |Já existe { msg } gerada para algum(s) lote(s), Caso confirme o número será Substituído!|
        text_button_1         = 'Confirmar'
        text_button_2         = 'Cancelar'
        display_cancel_button = ' '
      IMPORTING
        answer                = return.
  ENDMETHOD.

  METHOD get_limite_peso.
    FREE value.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE limite_peso
      FROM zsdt0045
      INTO value
      WHERE instrucao EQ input.
  ENDMETHOD.

  METHOD get_peso_max.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE peso_max
      FROM zsdt0045
      INTO @DATA(vl_value)
      WHERE instrucao EQ @input.
    value = vl_value.
  ENDMETHOD.

  METHOD get_instrucao.

    DATA: r_instr  TYPE RANGE OF zsdt0045-instrucao,
          rr_instr LIKE LINE OF r_instr.

    CHECK input IS NOT INITIAL.

    rr_instr-sign   = 'I'.
    rr_instr-option = 'CP'.
    rr_instr-low    = input.
    APPEND rr_instr TO r_instr.
    CLEAR rr_instr.

    SELECT SINGLE * FROM zsdt0045 INTO  @DATA(vl_value)
      WHERE instrucao IN @r_instr.

    value = vl_value.

  ENDMETHOD.

  METHOD get_texto.

    CLEAR wl_header.

    IF NOT obj_custom_txt IS INITIAL AND NOT obj_custom_editor IS INITIAL.
      CALL METHOD obj_custom_txt->free( ).
      CALL METHOD obj_custom_editor->free( ).
    ENDIF.

    wl_header-tdname = w_aux-nr_warehouse && w_aux-model_wh.
    wl_header-tdobject = 'ZWAREHOUSE'.
    wl_header-tdid     = 'ZWH'.
    wl_header-tdspras  = sy-langu.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = wl_header-tdid
        language                = sy-langu
        name                    = wl_header-tdname
        object                  = wl_header-tdobject
      TABLES
        lines                   = it_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    gt_editor = VALUE #( FOR ls IN it_texto ( line = ls-tdline ) ).

    CREATE OBJECT: obj_custom_txt    EXPORTING container_name = 'CC_TEXTO',
                   obj_custom_editor EXPORTING
                                                wordwrap_mode     = 1
                                                wordwrap_position = 76
                                                max_number_chars  = 20000
                                                parent         = obj_custom_txt.

    CALL METHOD obj_custom_editor->delete_text.

    CALL METHOD obj_custom_editor->set_text_as_r3table
      EXPORTING
        table = gt_editor.

    CALL SCREEN 400.

    return = sy-ucomm.
    CHECK return NE 'BACK'.

    CLEAR return.
    FREE: gt_editor, it_texto.

    CALL METHOD obj_custom_editor->get_text_as_stream
      IMPORTING
        text = gt_editor.

    LOOP AT gt_editor INTO DATA(gl_editor).

      IF return IS INITIAL.
        return = gl_editor-line.
      ELSE.
        return = |{ return }{ gl_editor-line }|.
      ENDIF.

    ENDLOOP.

    DATA(len1) = strlen( return ).
    i = 0.

    DO.

      IF i = len1.
        EXIT.
      ENDIF.

      DATA(len) = strlen( return+i ).
      IF len >= 130.
        APPEND VALUE #( tdline = return+i(130) ) TO it_texto.
        ADD 130 TO i.
      ELSE.
        APPEND VALUE #( tdline = return+i(len) ) TO it_texto.
        ADD len TO i.
      ENDIF.

    ENDDO.

    IF it_texto IS NOT INITIAL.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header          = wl_header
          savemode_direct = abap_true
        TABLES
          lines           = it_texto
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.

      CLEAR it_texto.

      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          object          = wl_header-tdobject
          name            = wl_header-tdname
          id              = wl_header-tdid
          language        = sy-langu
          savemode_direct = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD dados_cadastrais.

    CALL TRANSACTION 'ZSDT0196'.

  ENDMETHOD.

  METHOD dados_cadastro_depositos.

    CALL TRANSACTION 'ZSDT0206'.

  ENDMETHOD.

  METHOD busca_texto_geral.

    FREE: t_texto_geral, v_butxt.

    IF ( w_aux-empresa IS NOT INITIAL ).
      SELECT SINGLE butxt
        INTO @v_butxt
        FROM t001
        WHERE bukrs EQ @w_aux-empresa.

      IF ( i_ucomm EQ 'PROVI1' OR
           i_ucomm EQ 'PROVI2').
        SELECT *
          INTO TABLE @DATA(lt_zsdt0292)
          FROM zsdt0292
          WHERE empresa EQ @w_aux-empresa
            AND status  EQ 'A'.

        IF ( lt_zsdt0292 IS NOT INITIAL )..
          SORT lt_zsdt0292 BY empresa data DESCENDING hora DESCENDING.
          DATA(lv_name) = lt_zsdt0292[ 1 ]-texto_geral.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'ST'
              language                = sy-langu
              name                    = lv_name
              object                  = 'TEXT'
            TABLES
              lines                   = t_texto_geral
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

        ENDIF.
      ENDIF.
    ELSE.
      v_butxt = 'AGROPECUÁRIA MAGGI LTDA'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

  sscrfields-functxt_01 = 'Dados Cadastrais'(002).
  sscrfields-functxt_02 = 'De-Para Depósitos'(003).

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'S_EMP-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN. "PAI
  IF sy-ucomm <> 'FC01' AND sy-ucomm <> 'FC02'.
    IF s_emp[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Preencher os campos obrigatórios.' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.
      zcl_trace=>dados_cadastrais( ).

    WHEN 'FC02'.
      zcl_trace=>dados_cadastro_depositos( ).

  ENDCASE.

START-OF-SELECTION.
*  if ( s_emp IS INITIAL ).
*    MESSAGE 'Informar campo empresa' TYPE 'S' DISPLAY LIKE 'E'.
*    leave LIST-PROCESSING.
*  endif.
  zcl_trace=>get_dados( ).
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
*  SET PF-STATUS 'PFSTATUS'.
*  SET TITLEBAR 'TITLE'.
  zcl_trace=>set_alv( '100' ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
      zcl_trace=>set_dados( ).
      FREE tg_msg_ret.
    WHEN 'BACK' OR 'EXIT'.
      FREE tg_msg_ret.
    WHEN 'CANCEL'.
      zcl_trace=>get_dados( ).
*      _EDIT = ABAP_FALSE.
      CALL METHOD _grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
*  SET PF-STATUS 'PF0200'.
*  SET TITLEBAR 'TI0200'.

  zcl_trace=>show_msgre( ).

  zcl_trace=>set_alv( '200' ).

  IF w_instrucao-limite_peso NE 'S'.
    LOOP AT SCREEN.
      IF screen-name EQ 'W_INSTRUCAO-PESO_MAX'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: it_rsparams TYPE TABLE OF rsparams.

  CASE sy-ucomm.
    WHEN 'SHOW_MSGRE'.
      zcl_trace=>show_msgre( ).
    WHEN 'BACK'.
      x_screen = '200'.
      zcl_trace=>show_msgre( ).
    WHEN 'BTN_COM'.
      _versao = abap_false.
    WHEN 'BTN_SIM'.
      _versao = abap_true.
    WHEN 'BTN_ENVIAR'.

      CHECK tg_msg_ret[] IS INITIAL.
      CHECK w_instrucao-objek IS NOT INITIAL.

*** Inicio - Rubenilson Pereira - 13.02.25 - US164130
      IF w_instrucao-data_container < w_instrucao-data_instr OR
         w_instrucao-data_eta < w_instrucao-data_instr OR
         w_instrucao-data_in_porto < w_instrucao-data_instr OR
         w_instrucao-deadline_documen < w_instrucao-data_instr OR
         w_instrucao-deadline_draft < w_instrucao-data_instr OR
         w_instrucao-data_porto < w_instrucao-data_instr OR
         w_instrucao-data_retirada < w_instrucao-data_instr.
        MESSAGE 'Todas as datas devem ser iguais ou maiores que a data de instrução' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
*** Fim - Rubenilson Pereira - 13.02.25 - US164130

      _nro_sol_ov = |{ w_instrucao-objek ALPHA = IN }|.

      it_rsparams =
                     VALUE #(
                             ( selname = 'S_SOLOV' kind = 'S' sign = 'I' option = 'EQ' low = _nro_sol_ov    )
                             ( selname = 'P_INST'  kind = 'P' sign = ''  option = ''   low = abap_true    )
                            ).

      EXPORT: _instrucao TO MEMORY ID 'ZSENDINS'.

      SUBMIT zsdr0072 WITH SELECTION-TABLE it_rsparams AND RETURN.

      LOOP AT _instrucao ASSIGNING FIELD-SYMBOL(<f_ins>).

        SELECT SUM( qtd_fardos )
          FROM zsdt0182
        INTO <f_ins>-qtd_fardos_vinc
        WHERE id_trace EQ <f_ins>-id_trace.

        <f_ins>-saldo_fardos = <f_ins>-quantidade - <f_ins>-qtd_fardos_vinc.

        SELECT SUM( peso )
          FROM zsdt0182
        INTO <f_ins>-qtd_peso_vinc
        WHERE id_trace EQ <f_ins>-id_trace.

        <f_ins>-saldo_peso = <f_ins>-btgew - <f_ins>-qtd_peso_vinc.

      ENDLOOP.

*      _EDIT = ABAP_FALSE.
      CALL METHOD _grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

    WHEN 'BTNP'.
      IF w_instrucao-limite_peso NE 'S'.
        w_instrucao-peso_max = 0.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRL_REG_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE crl_reg_modify INPUT.

  DATA: vl_werks    TYPE werks_d.

  vl_contrato = wa_ins-contrato.
  vl_werks = wa_ins-werks.

*  IF W_INSTRUCAO-OBJEK IS NOT INITIAL.
*    _NRO_SOL_OV = |{ W_INSTRUCAO-OBJEK ALPHA = IN }|.
*    SELECT COUNT(*) FROM ZSDT0051 WHERE NRO_SOL_OV EQ _NRO_SOL_OV AND BSTKD EQ VL_CONTRATO.
*    IF SY-SUBRC IS NOT INITIAL.
*      MESSAGE |Contrato, da Solicitação informada, difere do Contrato dos Lotes!| TYPE 'S' DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDIF.

  IF zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'S' OR
     zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'N'.
    w_instrucao-limite_peso = zcl_trace=>get_limite_peso( w_instrucao-instrucao ).
  ENDIF.

  IF zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'S' OR
     zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'N'.
    w_instrucao-peso_max = zcl_trace=>get_peso_max( w_instrucao-instrucao ).
  ENDIF.

  w_instrucao-terminal_desc  = zcl_trace=>get_lfa1( |{ w_instrucao-terminal ALPHA = IN }| ).
  w_instrucao-armz_desc = zcl_trace=>get_lfa1( |{ w_instrucao-cod_armz ALPHA = IN }| ).
  w_instrucao-ponto_c_desc   = zcl_trace=>get_lfa1( |{ w_instrucao-ponto_c ALPHA = IN }| ).
  w_instrucao-cod_despac_desc = zcl_trace=>get_lfa1( |{ w_instrucao-cod_despach ALPHA = IN }| ).
  w_instrucao-cod_transp_desc = zcl_trace=>get_lfa1( |{ w_instrucao-cod_transp ALPHA = IN }| ).

  w_instrucao-terminal_estuf_desc = zcl_trace=>get_lfa1( |{ w_instrucao-terminal_estuf ALPHA = IN }| ).
  w_instrucao-controladora_desc = zcl_trace=>get_lfa1( |{ w_instrucao-controladora ALPHA = IN }| ).

  IF vl_werks IS NOT INITIAL.
    SELECT SINGLE vkorg FROM t001w INTO w_instrucao-bukrs WHERE werks EQ vl_werks.
  ENDIF.

*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
  w_instrucao-desc_local_entrega = zcl_trace=>get_desc_local_entrega( w_instrucao-local_entrega ).
  w_instrucao-desc_oper_log = zcl_trace=>get_desc_oper_log( w_instrucao-oper_log ).
*** Fim - Rubenilson Pereira - 12.02.25 - US164130

  LOOP AT _instrucao ASSIGNING FIELD-SYMBOL(<f_instrucao>).
    <f_instrucao>-objek            = w_instrucao-objek.
    <f_instrucao>-bukrs            = w_instrucao-bukrs.
    <f_instrucao>-instrucao        = w_instrucao-instrucao.
    <f_instrucao>-data_instr       = w_instrucao-data_instr.
    <f_instrucao>-data_in_porto    = w_instrucao-data_in_porto.
    <f_instrucao>-deadline_draft   = w_instrucao-deadline_draft.
    <f_instrucao>-mapa             = w_instrucao-mapa.
    <f_instrucao>-fumigacao        = w_instrucao-fumigacao.
    <f_instrucao>-data_retirada    = w_instrucao-data_retirada.
    <f_instrucao>-data_porto       = w_instrucao-data_porto.
    <f_instrucao>-deadline_documen = w_instrucao-deadline_documen.
    <f_instrucao>-hrs_fgacao       = w_instrucao-hrs_fgacao.
    <f_instrucao>-armador          = w_instrucao-armador.
    <f_instrucao>-porto_embarque   = w_instrucao-porto_embarque.
    <f_instrucao>-free_time        = w_instrucao-free_time.
    <f_instrucao>-qtd_ctners       = w_instrucao-qtd_ctners.
    <f_instrucao>-terminal         = w_instrucao-terminal.
    <f_instrucao>-vlr_frete        = w_instrucao-vlr_frete.
*    <F_INSTRUCAO>-PONTO_C          = W_INSTRUCAO-PONTO_C.
    <f_instrucao>-booking          = w_instrucao-booking.
    <f_instrucao>-cod_despach      = w_instrucao-cod_despach.
    <f_instrucao>-observacao       = w_instrucao-observacao.
    <f_instrucao>-cod_transp       = w_instrucao-cod_transp.
    <f_instrucao>-data_eta         = w_instrucao-data_eta.
    <f_instrucao>-navio            = w_instrucao-navio.
    <f_instrucao>-limite_peso      = COND #( WHEN w_instrucao-limite_peso EQ 'S' THEN w_instrucao-limite_peso ELSE 'N').
    <f_instrucao>-peso_max         = COND #( WHEN w_instrucao-limite_peso EQ 'S' THEN w_instrucao-peso_max ELSE 0 ).
    <f_instrucao>-terminal_estuf   = w_instrucao-terminal_estuf.
    <f_instrucao>-controladora     = w_instrucao-controladora.
    <f_instrucao>-pais_des         = w_instrucao-pais_des.
    <f_instrucao>-data_container   = w_instrucao-data_container.
*** CS2020001361 - Inicio - Camila Brand
    <f_instrucao>-armazenagem      = w_instrucao-armazenagem.
    <f_instrucao>-lote_armz	       = w_instrucao-lote_armz.
    <f_instrucao>-cod_armz         = w_instrucao-cod_armz.
*** CS2020001361 - Fim - Camila Brand

*** Inicio - Rubenilson Pereira - 13.02.25 - US164130
    <f_instrucao>-incoterm           = w_instrucao-incoterm.
    <f_instrucao>-local_entrega      = w_instrucao-local_entrega.
    <f_instrucao>-desc_local_entrega = w_instrucao-desc_local_entrega.
    <f_instrucao>-oper_log           = w_instrucao-oper_log.
    <f_instrucao>-desc_oper_log      = w_instrucao-desc_oper_log.
*** Fim - Rubenilson Pereira - 13.02.25 - US164130

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PFSTATUS'.
  SET TITLEBAR 'TITLE'.

  zcl_trace=>show_msgre( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SHOW_MSGRE'.
      zcl_trace=>show_msgre( ).
    WHEN 'SAVE'.
      zcl_trace=>set_dados( ).
      FREE tg_msg_ret.
      zcl_trace=>get_dados( ).
    WHEN 'REPLICAR'.
      PERFORM f_replicar_dados.
    WHEN 'DELETAR'.
      PERFORM f_deletar_dados.
*-CS2022000332-#79430-02.08.2022-JT-inicio
    WHEN 'IMPORTAR'.
      PERFORM f_importar_dados.
    WHEN 'ALTSTATUS'.
      PERFORM f_alterar_status.
*-CS2022000332-#79430-02.08.2022-JT-fim
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_doc INPUT.
  w_instrucao-objek = zcl_trace=>f4_solov( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'PF0400'.
  SET TITLEBAR 'TI0400'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'AVANCAR'.
      LEAVE TO SCREEN 0.
    WHEN 'REPLICAR'.
      PERFORM f_replicar_dados.
    WHEN 'DELETAR'.
      PERFORM f_deletar_dados.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INSTRUCAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE instrucao INPUT.
  CLEAR w_instrucao_aux.

  w_instrucao_aux = zcl_trace=>get_instrucao( w_instrucao-instrucao ).
  IF w_instrucao_aux-instrucao IS NOT INITIAL.
    MOVE-CORRESPONDING w_instrucao_aux TO w_instrucao.
    CLEAR w_instrucao-objek.
  ENDIF.

  IF zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'S' OR
     zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'N'.
    w_instrucao-limite_peso = zcl_trace=>get_limite_peso( w_instrucao-instrucao ).
  ENDIF.

  IF zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'S' OR
     zcl_trace=>get_limite_peso( w_instrucao-instrucao ) = 'N'.
    w_instrucao-peso_max = zcl_trace=>get_peso_max( w_instrucao-instrucao ).
  ENDIF.

  LOOP AT _instrucao ASSIGNING <f_instrucao>.
    <f_instrucao>-objek            = w_instrucao-objek.
    <f_instrucao>-instrucao        = w_instrucao-instrucao.
    <f_instrucao>-limite_peso      = w_instrucao-limite_peso.
    <f_instrucao>-peso_max         = w_instrucao-peso_max.
  ENDLOOP.

*  w_instrucao-limite_peso = zcl_trace=>get_limite_peso( w_instrucao-instrucao ).
*  w_instrucao-limite_peso = COND #( WHEN zcl_trace=>get_limite_peso( w_instrucao-instrucao ) EQ 'S' THEN zcl_trace=>get_limite_peso( w_instrucao-instrucao ) ELSE 'N' ).
*  w_instrucao-peso_max = COND #( WHEN w_instrucao-limite_peso EQ 'S' THEN CONV #( zcl_trace=>get_peso_max( w_instrucao-instrucao ) ) ELSE '0' ).

W_INSTRUCAO-INCOTERM = 'CIF'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PESO_MAX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE peso_max INPUT.
  IF w_instrucao-limite_peso NE 'S'.
    w_instrucao-peso_max = 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_REPLICAR_DADOS
*&---------------------------------------------------------------------*
FORM f_replicar_dados .

  DATA: t_zsdt0143  TYPE TABLE OF zsdt0143.

  DATA: v_mensagem  TYPE string,
        v_mensagem2 TYPE string,
        v_mensagem3 TYPE string,
        l_seq       TYPE numc15,
        l_kunnr     TYPE kunnr.

  DATA: w_zsdt0143 TYPE zsdt0143,
        w_0116_aux TYPE zsdt0166.

  CALL METHOD _grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows. "DATA(lt_sel_rows). *-CS2022000332-#79430-02.08.2022-JT-inicio

  DATA(lt_sel_rows) = t_rows[].

  LOOP AT lt_sel_rows INTO DATA(w_sel_rows).

    READ TABLE _0166 INTO DATA(w_0166) INDEX w_sel_rows-index.

    IF sy-subrc IS INITIAL.
      IF  w_0166-status = 'A' AND
          w_0166-id_origem IS NOT INITIAL.

        CONCATENATE 'Take-up já foi replicado para a empresa'
              w_0166-empresa
              INTO v_mensagem SEPARATED BY space.

        CONCATENATE 'ID ja foi replicado:'
              w_0166-id
              INTO v_mensagem2 SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel        = 'Replicação de dados'
            textline1    = v_mensagem
            textline2    = v_mensagem2
            start_column = 25
            start_row    = 6.
        EXIT.
      ELSEIF w_0166-status = 'A' AND
             w_0166-id_origem IS INITIAL.

        SELECT *
          FROM zsdt0143
          INTO @DATA(w_0143)
            UP TO 1 ROWS
         WHERE contrato     = @w_0166-contrato
           AND safra        = @w_0166-safra
           AND empresa      = @w_0166-empresa
           AND visao        = 'P'
           AND intercompany = 'X'
           AND cancelado   <> @abap_true
          ORDER BY id_contrato ASCENDING.
        ENDSELECT.

        IF sy-subrc IS INITIAL.
          SELECT *
            FROM zsdt0143
            INTO TABLE @DATA(t_0143)
           WHERE id_contrato_referencia = @w_0143-id_contrato
             AND cancelado              = @abap_off.

          LOOP AT t_0143 INTO w_0143.

*---------------------
*---------- proximo numero
*---------------------
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZSEQ0165'
              IMPORTING
                number                  = l_seq
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.

*---------------------
*---------- material
*---------------------
            SELECT *
              INTO @DATA(w_cabn)
              FROM cabn
                UP TO 1 ROWS
              WHERE atnam = 'COD_ALGODAO_SIMILAR'.
            ENDSELECT.
            IF sy-subrc <> 0.
              CLEAR w_cabn.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = w_0166-matnr
              IMPORTING
                output = w_0166-matnr.


            SELECT *
              INTO @DATA(w_ausp)
              FROM ausp
                UP TO 1 ROWS
              WHERE atinn = @w_cabn-atinn
                AND klart = '001'
                AND atwrt = @w_0166-matnr.
            ENDSELECT.
            IF sy-subrc <> 0.
              CLEAR w_ausp.
            ENDIF.

*---------------------
* busca normt
*---------------------
            SELECT SINGLE normt
              INTO @DATA(l_normt)
              FROM mara
             WHERE matnr = @w_ausp-objek.



*---------------------
*---------- popula tabela
*---------------------
            w_0116_aux-mandt          = sy-mandt.
            w_0116_aux-id             = l_seq.
            w_0116_aux-data           = sy-datum.
            w_0116_aux-hora           = sy-uzeit.
            w_0116_aux-status         = w_0166-status.
            w_0116_aux-lote           = w_0166-lote.
            w_0116_aux-kunnr          = w_0143-cliente.
            w_0116_aux-matnr          = w_ausp-objek.
            w_0116_aux-tipo           = l_normt.
            w_0116_aux-qtd_fardos     = w_0166-qtd_fardos.
            w_0116_aux-peso_lote      = w_0166-peso_lote.
            w_0116_aux-motivo         = w_0166-motivo.
            w_0116_aux-werks          = w_0166-kunnr+6(4).
            w_0116_aux-algodoeira     = w_0166-werks && '-' && w_0166-algodoeira.
            w_0116_aux-contrato       = w_0143-contrato.
            w_0116_aux-tamanho_fardo  = w_0166-tamanho_fardo.
            w_0116_aux-preco_ctr      = w_0143-preco.
            w_0116_aux-data_takeup    = w_0166-data_takeup.
            w_0116_aux-safra          = w_0166-safra.
            w_0116_aux-id_origem      = w_0166-id.
            w_0116_aux-empresa        = w_0143-empresa.
            w_0166-d_cliente          = zcl_trace=>get_desc_cliente( w_0143-cliente ).
*           w_0116_aux-acts           = w_0166-acts.  "*-CS2023000189-05.04.2023-#108694-JT

            MOVE-CORRESPONDING w_0116_aux  TO w_0166.
            APPEND w_0166                  TO _0166.
          ENDLOOP.
        ELSE.
          CONCATENATE 'Este take-up do lote'  w_0166-lote
                 INTO v_mensagem  SEPARATED BY space.
          CONCATENATE 'não pode ser replicado, pois seu ' 'contrato não é Intercompany.'
                 INTO v_mensagem2 SEPARATED BY space.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              titel        = 'Replicação de dados'
              textline1    = v_mensagem
              textline2    = v_mensagem2
              start_column = 25
              start_row    = 6.
        ENDIF.
      ELSE.
        CONCATENATE 'Este take-up do lote'  w_0166-lote
               INTO v_mensagem  SEPARATED BY space.
        CONCATENATE 'não pode ser replicado, pois seu ' 'contrato não é Intercompany.'
               INTO v_mensagem2 SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel        = 'Replicação de dados'
            textline1    = v_mensagem
            textline2    = v_mensagem2
            start_column = 25
            start_row    = 6.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_importar_dados.

  DATA: l_importou TYPE char1.

*------------------------------
* importar arquivo
*------------------------------
  CALL FUNCTION 'ZSD_IMPORTAR_DADOS_TAKEUP'
    EXPORTING
      i_bukrs    = s_emp-low
    IMPORTING
      e_importou = l_importou.

  IF l_importou = abap_true.
    zcl_trace=>get_dados( ).
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alterar_status.

  DATA: l_erro       TYPE c,
        l_resp       TYPE c,
        l_tabix      TYPE sy-tabix,
        l_seq        TYPE numc15,
        w_0166_grava TYPE zsdt0166.

  CLEAR: g_new_status, l_erro.

  CALL METHOD _grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  READ TABLE t_rows INTO DATA(w_sel_rows) INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Marcar pelo menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT t_rows INTO w_sel_rows.
    READ TABLE _0166 INTO DATA(w_0166) INDEX w_sel_rows-index.
    CHECK sy-subrc = 0.
    IF w_0166-carga_dados = abap_off.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH 'Devem ser escolhidas linhas'
                          ' que foram Importadas!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------------
* informar status
*-----------------------------
  CALL SCREEN 500 STARTING AT 62 1
                    ENDING AT 91 2.

  CHECK ok_code = 'OK'.

*-----------------------------
* confirmar
*-----------------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação de Alteração de Status'
      text_question         = 'Confirma Alteração de Status?'
      text_button_1         = 'Confirma'
      text_button_2         = 'Cancela'
      icon_button_1         = 'ICON_SYSTEM_OKAY'
      icon_button_2         = 'ICON_SYSTEM_CANCEL'
      display_cancel_button = ' '
    IMPORTING
      answer                = l_resp
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_resp = '1'.

  LOOP AT t_rows INTO w_sel_rows.
    READ TABLE _0166 INTO w_0166 INDEX w_sel_rows-index.
    l_tabix = sy-tabix.

    CHECK sy-subrc = 0.
    CHECK w_0166-carga_dados = abap_true.

    w_0166-status              = g_new_status.
    w_0166-d_status            = SWITCH #( w_0166-status
                                   WHEN 'A' THEN 'Aprovado'
                                   WHEN 'R' THEN 'Reprovado'
                                   WHEN 'S' THEN 'Stand by'
                                   WHEN 'N' THEN 'Não Avaliado'
                                   ELSE '' ).

    MODIFY _0166 FROM w_0166 INDEX l_tabix.

*---------------------
*-- proximo numero
*---------------------
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0165'
      IMPORTING
        number                  = l_seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    MOVE-CORRESPONDING w_0166 TO w_0166_grava.
    MOVE l_seq                TO w_0166_grava-id.
    MOVE sy-datum             TO w_0166_grava-data.
    MOVE sy-uzeit             TO w_0166_grava-hora.
    MOVE sy-datum             TO w_0166_grava-data_atual.
    MOVE sy-uzeit             TO w_0166_grava-hora_atual.
    MOVE sy-uname             TO w_0166_grava-usnam.

    MODIFY zsdt0166         FROM w_0166_grava.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  zcl_trace=>get_dados( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DELETAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_deletar_dados .

  DATA: lt_zsdt0045 TYPE TABLE OF zsdt0045.

  CALL METHOD _grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows. "DATA(lt_sel_rows). *-CS2022000332-#79430-02.08.2022-JT-inicio

  DATA(lt_sel_rows) = t_rows[].

  LOOP AT lt_sel_rows INTO DATA(w_sel_rows).

    READ TABLE _0166 INTO DATA(w_0166) INDEX w_sel_rows-index.
    CHECK sy-subrc = 0.

    IF w_0166-id_origem  IS INITIAL OR
       w_0166-carga_dados = abap_true. "*-CS2022000332-#79430-02.08.2022-JT-inicio
      MESSAGE s024(sd) WITH 'Exclusão não Permitida!' DISPLAY LIKE 'E'.
    ELSE.
      SELECT *
        FROM zsdt0045
        INTO TABLE lt_zsdt0045
        WHERE contrato = w_0166-contrato
          AND bukrs    = w_0166-empresa.
      IF sy-subrc IS INITIAL.
        MESSAGE s024(sd) WITH 'Para este take-up já temos uma INSTRUÇÃO criada' DISPLAY LIKE 'E'.
*       WRITE 'Para este take-up já temos uma INSTRUÇÃO criada'.
      ELSE.
        DELETE _0166 INDEX w_sel_rows-index.
        DELETE FROM zsdt0166 WHERE id   = w_0166-id   AND
                                   data = w_0166-data AND
                                   hora = w_0166-hora.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE ok_code.
    WHEN 'OK'.
      IF g_new_status <> 'N' AND
         g_new_status <> 'R' AND
         g_new_status <> 'S'.
        MESSAGE s024(sd) WITH 'Informar um Status Valido!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CLEAR ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  CLEAR ok_code.
  SET PF-STATUS 'ZSDR0093B'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F_HELP_STATUS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_help_status INPUT.

  DATA: BEGIN OF s_domi,
          domvalue_l TYPE domvalue_l,
          ddtext     TYPE val_text,
        END OF s_domi,
        t_domi LIKE TABLE OF s_domi.
  DATA: t_dd07v TYPE STANDARD TABLE OF dd07v,
        s_dd07v TYPE dd07v.

  REFRESH: t_domi.

* Get the domain values
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'Z_STATUS_TRACE'   " Give your domain here
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

* Prepare the data.
  LOOP AT t_dd07v INTO s_dd07v.
    CHECK s_dd07v-domvalue_l <> 'A'.
    MOVE-CORRESPONDING s_dd07v TO s_domi.
    APPEND s_domi TO t_domi.
  ENDLOOP.

* F4
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      pvalkey          = ' '
      retfield         = 'DOMVALUE_L'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'WA_NOVO-VISAO'
      callback_program = sy-repid
      value_org        = 'S'
    TABLES
      value_tab        = t_domi
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.
