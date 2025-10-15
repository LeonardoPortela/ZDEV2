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
**| Cadastro de Formação de Lote                                              |*
**/===========================================================================\*

REPORT zsdr0074.

***********************************************************************************************
* TABELAS
***********************************************************************************************
TABLES: zsdt0051.

***********************************************************************************************
* PARAMETROS DE SELEÇÃO
***********************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_solov FOR zsdt0051-nro_sol_ov NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

***********************************************************************************************
* ESTRUTURAS
***********************************************************************************************
TYPES: BEGIN OF ty_0066.
TYPES: icon(4)       TYPE c.
TYPES: status_trace(4) TYPE c.
       INCLUDE STRUCTURE zsdt0066.
TYPES: color(4)        TYPE c,
       werks_desc      TYPE name1,
       matnr_desc      TYPE maktx,
       terminal_desc   TYPE name1,
       ponto_c_desc    TYPE name1,
       lentrega_desc   TYPE name1,
       kunnr_desc      TYPE name1,
       btgew           TYPE gsgew.

TYPES END OF ty_0066.

TYPES: BEGIN OF ty_0051.
         INCLUDE TYPE zsdt0051.
TYPES:   %nro_sol_ov TYPE zsdt0045-objek,
         %bstkd      TYPE zsdt0045-contrato.
TYPES END OF  ty_0051.

TYPES: t_0066 TYPE TABLE OF zsdt0066.
******************************************************************************************************
******** DECLARAÇÕES GLOBAIS
******************************************************************************************************
DATA: it_0066         TYPE TABLE OF ty_0066 WITH HEADER LINE,
      ws_0066         TYPE ty_0066,
      wa_0066         TYPE zsdt0066,
      wa_edit1        TYPE zsdt0066,
      it_0051         TYPE TABLE OF ty_0051 WITH HEADER LINE,
      it_new          TYPE TABLE OF ty_0066 WITH HEADER LINE,
      it_save         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      wa_save         TYPE zsdt0066,
      it_edit         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_edit1        TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_dele         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_0066_old     TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      l_nro_sol_ov    TYPE objnum,               "*-CS2023000189-19.04.2023-#108709-JT
      l_tabix         TYPE sy-tabix,            "*-CS2023000189-19.04.2023-#108709-JT
      l_referencia    TYPE numc10   VALUE 1000, "*-CS2023000189-19.04.2023-#108709-JT
      l_bstkd         TYPE text50             , "*-CS2023000189-19.04.2023-#108709-JT
      l_back          TYPE char1,               "*-CS2023000189-19.04.2023-#108709-JT
      t_zsdt0328      TYPE TABLE OF zsdt0328,   "*-CS2023000189-19.04.2023-#108709-JT
      w_zsdt0328      TYPE zsdt0328,            "*-CS2023000189-19.04.2023-#108709-JT
      t_zsdt0327      TYPE TABLE OF zsdt0327,   "*-CS2023000189-19.04.2023-#108709-JT
      w_zsdt0327      TYPE zsdt0327,            "*-CS2023000189-19.04.2023-#108709-JT
      t_ret           TYPE TABLE OF zsdt0328,   "*-CS2023000189-19.04.2023-#108709-JT
      wa_0066_old     TYPE zsdt0066,
      it_alt_qtd      TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      tg_save_log     TYPE TABLE OF zsdt0083,
      it_log_old      TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      obj_alv         TYPE REF TO cl_gui_alv_grid,
      obj_cont        TYPE REF TO cl_gui_custom_container,
      wa_variant      TYPE disvariant,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      str             TYPE REF TO data,
      l_value_edit    TYPE sy-tabix,
      it_fcat         TYPE lvc_t_fcat,
      it_fcat_log     TYPE lvc_t_fcat,
      tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_mensagem(30),
      lv_acao         TYPE sy-ucomm,
      vlgort          TYPE t001l-lgort.

DATA: cont       TYPE n LENGTH 2,
      str_l(255) TYPE c,
      campo      TYPE char20,
      campo2     TYPE char20,
      tam        TYPE i.

FIELD-SYMBOLS: <fs_campo>  TYPE any,
               <fs_campo1> TYPE any,
               <fs_campo2> TYPE any.

CLASS zcl_events DEFINITION.
  PUBLIC SECTION.

    METHODS: on_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS zcl_formlote DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_string,
             stn TYPE char100,
           END   OF ty_string.

    DATA: t_string TYPE TABLE OF ty_string.

    METHODS:
      contructor,
      get_dados
        RETURNING VALUE(return) TYPE char1,

      get_dados_trace                       "*-CS2023000189-19.04.2023-#108709-JT
        RETURNING VALUE(return) TYPE char1,

      get_header
        IMPORTING nro_sol_ov   TYPE zsded013 OPTIONAL
                  bstkd        TYPE bstkd OPTIONAL
        RETURNING VALUE(value) TYPE zsdt0051,

      get_posnr
        IMPORTING i_value        TYPE zsded013 OPTIONAL
                  i_referencia   TYPE numc10   OPTIONAL
        RETURNING VALUE(r_value) TYPE posnr_va,

      monta_log,
      set_log
        IMPORTING input TYPE t_0066 OPTIONAL,
      input_log
        IMPORTING value1 TYPE lvc_fname OPTIONAL
                  value2 TYPE char20  OPTIONAL
                  value3 TYPE sy-tabix OPTIONAL,
      f4_bstkd
        RETURNING VALUE(r_value) TYPE bstkd,
      f4_solov
        RETURNING VALUE(r_value) TYPE zsded013,
      f4_instrucao
        EXPORTING e_referencia   TYPE numc10 "*-CS2023000189-19.04.2023-#108709-JT
        RETURNING VALUE(r_value) TYPE zseq_inst,

      get_0045
        IMPORTING i_seq        TYPE any OPTIONAL
                  i_ins        TYPE any OPTIONAL
                  i_referencia TYPE numc10 OPTIONAL,  "*-CS2023000189-19.04.2023-#108709-JT

      get_zseq
        IMPORTING i_value        TYPE nrobj OPTIONAL
        RETURNING VALUE(r_value) TYPE zseq_inst,

      convert_meng
        IMPORTING i_matnr TYPE matnr OPTIONAL
                  i_zieme TYPE meins OPTIONAL " ZIEME
                  i_pmein TYPE meins OPTIONAL " PMEIN
                  i_zmeng TYPE bstmg OPTIONAL, " ZMENG

      calc_lote
        IMPORTING i_dmbtr         TYPE dmbtr OPTIONAL "DMBTR
        RETURNING VALUE(r_vlrtot) TYPE dmbtr,

      calc_usd
        IMPORTING i_libra_to TYPE zsded040 OPTIONAL "LIBRA_TO
                  i_zieme    TYPE meins OPTIONAL, " ZIEME
      set_desc,
      get_desc_cen
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_desc_mat
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_desc_lfa
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_desc_kna
        IMPORTING input        TYPE any OPTIONAL
        RETURNING VALUE(value) TYPE string,
      get_local_entrega
        IMPORTING input        TYPE kunnr
        RETURNING VALUE(value) TYPE kunnr,

      get_instrucao_f4,


      set_layout,
      show_msgre,
      set_erros,
      save,
      act_new_lote,
      act_edit_lote,
      act_dele_lote,
      act_reenvia_trace, "*-CS2023000189-19.04.2023-#108709-JT
      get_index
        IMPORTING input          TYPE sy-ucomm OPTIONAL
        RETURNING VALUE(r_value) TYPE sy-tabix,
      confirm
        IMPORTING input          TYPE any      OPTIONAL
        RETURNING VALUE(r_value) TYPE char1,
      remove_zero
        IMPORTING input          TYPE any      OPTIONAL
        RETURNING VALUE(r_value) TYPE numc10,
      modify_alv,
      liberar_lote,
      check_no_out
        IMPORTING input          TYPE lvc_fname OPTIONAL
        RETURNING VALUE(r_value) TYPE char1,
      no_out,
      split
        IMPORTING input TYPE any OPTIONAL,
      scrtext
        IMPORTING input          TYPE lvc_fname OPTIONAL
        RETURNING VALUE(r_value) TYPE scrtext_l,
      en_de_queue
        IMPORTING input        TYPE any
                  dir          TYPE any
        RETURNING VALUE(subrc) TYPE sy-subrc,
      freetable,
      check_botao,
      alterar_qtd,
      set_lock_unlock,

*-CS2023000189-19.04.2023-#108709-JT-inicio
      check_zmeng
        RETURNING VALUE(r_erro) TYPE char1,
*-CS2023000189-19.04.2023-#108709-JT-fim

      atualiza_qtd
        IMPORTING input         TYPE STANDARD TABLE OPTIONAL
        RETURNING VALUE(return) TYPE sy-subrc.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_fields,
             field TYPE lvc_fname,
           END OF ty_fields.

    DATA:
      it_f4_ins   TYPE TABLE OF zsdt0045,
      it_return   TYPE TABLE OF ddshretval,
      tl_dselc    TYPE TABLE OF dselc,
      at_usd(5)   TYPE p DECIMALS 4 VALUE'22.046',
      at_zmeng    TYPE bstmg,
      at_libra_to TYPE zsded040,
      at_lib_to_f TYPE zsded040,
      at_qtd      TYPE dzmeng.

    DATA: at_hist      TYPE zsded032,
          at_limit_qtd TYPE dzmeng,
          at_inst_qtd  TYPE gsgew,
          it_index     TYPE lvc_t_row.

    DATA: tg_0066 TYPE TABLE OF ty_0066,
          fields  TYPE TABLE OF ty_fields.

ENDCLASS.
*******
DATA(obj_fmlot) = NEW zcl_formlote( ).
DATA(obj_events) = NEW zcl_events( ).

**********************************************************************************************
*SELECTION-SCREEN VALUE-REQUEST
**********************************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_solov-low.
  s_solov-low = NEW zcl_formlote( )->f4_solov( ).
**********************************************************************************************

CLASS zcl_events IMPLEMENTATION.

  METHOD on_click.

    CASE e_column_id.
      WHEN 'VBELN'.
        CHECK it_0066[ e_row_id ]-vbeln IS NOT INITIAL.  "*-CS2023000189-19.04.2023-#108709-JT
        DATA(vbeln) = it_0066[ e_row_id ]-vbeln.         "*-CS2023000189-19.04.2023-#108709-JT

        SET PARAMETER ID 'AUN' FIELD vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

*-CS2023000189-19.04.2023-#108709-JT-inicio
      WHEN 'NRO_SOL_OV' OR 'INSTRUCAO'.
        DATA(_nro_sol_ov) = it_0066[ e_row_id ]-nro_sol_ov.
        DATA(_posnr)      = it_0066[ e_row_id ]-posnr.
        l_nro_sol_ov      = _nro_sol_ov.
        l_referencia      = _posnr.
        l_bstkd           = it_0051-bstkd.

        CALL FUNCTION 'ZSD_SELECAO_FORMACAO_LOTE'
          EXPORTING
            i_nro_sol_ov    = l_nro_sol_ov
            i_posnr         = _posnr
            i_contrato      = l_bstkd
            i_referencia    = l_referencia
            i_somente_exibe = abap_true
            i_editar        = abap_true
          TABLES
            t_retorno       = t_zsdt0328.  "t_ret.

      WHEN 'STATUS_TRACE'.
        _nro_sol_ov  = it_0066[ e_row_id ]-nro_sol_ov.
        _posnr       = it_0066[ e_row_id ]-posnr.

        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_nro_sol_ov   = _nro_sol_ov
            i_posnr        = _posnr
            i_tipo_integra = 'OV'.
*-CS2023000189-19.04.2023-#108709-JT-fim


    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_formlote IMPLEMENTATION.

  METHOD contructor.
    no_out( ).
    freetable( ).
  ENDMETHOD.

  METHOD get_dados.

    contructor( ).

    MOVE-CORRESPONDING obj_fmlot->get_header( nro_sol_ov = s_solov-low
                                             ) TO it_0051.

    IF it_0051-param_espec NE 'A' AND
       it_0051-param_espec NE 'Z'.
      MESSAGE 'Solic. de Venda informada não é de Algodão!' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_true.
      EXIT.
    ENDIF.

    SELECT *
      FROM zsdt0066
      INTO CORRESPONDING FIELDS OF TABLE tg_0066
      WHERE nro_sol_ov EQ it_0051-nro_sol_ov
      AND status NE 'D'.

    it_new-classificacao = 'C'.

*-CS2023000189-26.04.2023-#108710-JT-inicio
    IF tg_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
         FOR ALL ENTRIES IN tg_0066
       WHERE nro_sol_ov  = tg_0066-nro_sol_ov
         AND posnr       = tg_0066-posnr.
    ENDIF.

    SORT t_zsdt0327 BY nro_sol_ov posnr seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                     COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim

    LOOP AT tg_0066 ASSIGNING FIELD-SYMBOL(<f0066>).

*-CS2023000189-26.04.2023-#108710-JT-inicio
      READ TABLE t_zsdt0327 INTO w_zsdt0327 WITH KEY nro_sol_ov  = <f0066>-nro_sol_ov
                                                     posnr       = <f0066>-posnr.
      IF sy-subrc <> 0.
        <f0066>-status_trace = icon_dummy.
      ELSEIF w_zsdt0327-tipo_msg = 'E'.
        <f0066>-status_trace = icon_alert. "ICON_FAILURE
      ELSEIF w_zsdt0327-tipo_msg = 'S'.
        <f0066>-status_trace = icon_checked.
      ELSE.
        <f0066>-status_trace = icon_dummy.
      ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim

      <f0066>-icon = SWITCH #( <f0066>-status
                               WHEN 'A' OR '' THEN '@Q3@'
                               WHEN 'L'       THEN '@5Y@'
                               WHEN 'E'       THEN '@16@'
                             ).

      <f0066>-werks_desc    = obj_fmlot->get_desc_cen( <f0066>-werks ).
      <f0066>-matnr_desc    = obj_fmlot->get_desc_mat( <f0066>-matnr ).
      <f0066>-terminal_desc = obj_fmlot->get_desc_lfa( <f0066>-terminal ).
      <f0066>-ponto_c_desc  = obj_fmlot->get_desc_lfa( <f0066>-ponto_c ).
      <f0066>-lentrega_desc = obj_fmlot->get_desc_kna( <f0066>-lentrega ).
      <f0066>-kunnr_desc    = obj_fmlot->get_desc_kna( <f0066>-kunnr ).

    ENDLOOP.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF tg_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0328
        INTO TABLE t_zsdt0328
         FOR ALL ENTRIES IN tg_0066
       WHERE nro_sol_ov = tg_0066-nro_sol_ov
         AND posnr      = tg_0066-posnr
         AND cancelado  = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT

      LOOP AT t_zsdt0328   INTO w_zsdt0328.
        w_zsdt0328-referencia = w_zsdt0328-posnr.
        MODIFY t_zsdt0328  FROM w_zsdt0328 TRANSPORTING referencia.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    APPEND LINES OF tg_0066[] TO it_0066[].

    obj_fmlot->set_desc( ).
    obj_fmlot->monta_log( ).

  ENDMETHOD.
*
*-CS2023000189-26.04.2023-#108710-JT-inicio
  METHOD get_dados_trace.

    IF it_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_zsdt0327
         FOR ALL ENTRIES IN it_0066
       WHERE nro_sol_ov  = it_0066-nro_sol_ov
         AND posnr       = it_0066-posnr.
    ENDIF.

    SORT t_zsdt0327 BY nro_sol_ov posnr seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_zsdt0327
                     COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim

    LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<f0066>).

      READ TABLE t_zsdt0327 INTO w_zsdt0327 WITH KEY nro_sol_ov  = <f0066>-nro_sol_ov
                                                     posnr       = <f0066>-posnr.
      IF sy-subrc <> 0.
        <f0066>-status_trace = icon_dummy.
      ELSEIF w_zsdt0327-tipo_msg = 'E'.
        <f0066>-status_trace = icon_alert. "ICON_FAILURE
      ELSEIF w_zsdt0327-tipo_msg = 'S'.
        <f0066>-status_trace = icon_checked.
      ELSE.
        <f0066>-status_trace = icon_dummy.
      ENDIF.

    ENDLOOP.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.
*-CS2023000189-26.04.2023-#108710-JT-fim

  METHOD get_header.
    SELECT SINGLE * FROM zsdt0051 INTO value WHERE nro_sol_ov EQ nro_sol_ov.
  ENDMETHOD.
*******
  METHOD get_posnr.

    SELECT MAX( posnr )
      FROM zsdt0066
      INTO r_value
      WHERE nro_sol_ov EQ s_solov-low.

    DATA(seq) = REDUCE i( INIT x = 0 FOR ls_save IN it_save WHERE ( posnr IS NOT INITIAL ) NEXT x = x + 1 ).

    seq = seq * 10.
    ADD seq TO r_value.
    ADD 10 TO r_value.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    LOOP AT t_zsdt0328   INTO w_zsdt0328 WHERE referencia = i_referencia.
      w_zsdt0328-nro_sol_ov = s_solov-low.
      w_zsdt0328-posnr      = r_value.
      MODIFY t_zsdt0328  FROM w_zsdt0328 INDEX sy-tabix.
    ENDLOOP.
*-CS2023000189-19.04.2023-#108709-JT-fim

  ENDMETHOD.
*******
  METHOD monta_log.
    it_0066_old[] = CORRESPONDING #( it_0066[] ).
  ENDMETHOD.

  METHOD set_log.

    DATA: tabix TYPE sy-tabix.

    FREE it_fcat_log.

    ASSIGN 'ZSDT0066' TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA str TYPE (<fs_str>).

    it_fcat_log = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

    LOOP AT input INTO wa_0066.

      tabix = sy-tabix.
      CLEAR wa_0066_old.

      TRY .
          wa_0066_old = it_0066_old[ tabix ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT it_fcat_log INTO DATA(wa_fcat).
        obj_fmlot->input_log( EXPORTING
                        value1 = |WA_0066-{ wa_fcat-fieldname }|
                        value2 = 'form.lote'
                        value3 = tabix
                     ).
      ENDLOOP.
    ENDLOOP.

    FREE it_fcat_log.

  ENDMETHOD.

  METHOD input_log.

    DATA: wl_field(30),
          wl_field_old(40),
          wl_field_aux(40),
          wl_field_aux2(40).

    FIELD-SYMBOLS: <fs_field>     TYPE any,
                   <fs_field_old> TYPE any.

    UNASSIGN <fs_field>.
    UNASSIGN <fs_field_old>.

    wl_field = value1.
    SPLIT wl_field AT '-' INTO wl_field_aux
                               wl_field_aux2.

    wl_field_old = |{ wl_field_aux }_OLD-{ wl_field_aux2 }| .

    ASSIGN (wl_field) TO <fs_field>.
    ASSIGN (wl_field_old) TO <fs_field_old>.
    IF <fs_field> IS ASSIGNED.
      IF <fs_field> NE <fs_field_old>.
        IF  obj_fmlot->at_hist IS INITIAL.
          obj_fmlot->get_zseq( EXPORTING i_value = 'ZHISTORIC').
        ENDIF.
        SPLIT value1 AT '-' INTO wl_field
                                 wl_field_aux.

        APPEND VALUE #( nro_sol_ov   = s_solov-low
                        linha        = value3
                        id_historico = obj_fmlot->at_hist
                        area         = value2
                        campo        = wl_field_aux
                        new_value    = <fs_field>
                        old_value    = <fs_field_old>
                        usnam        = sy-uname
                        data_atual   = sy-datum
                        hora_atual   = sy-uzeit
                     ) TO tg_save_log.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD f4_bstkd.

    TYPES: BEGIN OF ty_bstkd,
             contrato TYPE zsdt0143-contrato,
             safra    TYPE zsdt0143-safra,
             empresa  TYPE zsdt0143-empresa,
           END OF ty_bstkd.

    DATA: it_bstkd TYPE TABLE OF ty_bstkd.

    SELECT contrato safra empresa
      FROM zsdt0143
      INTO TABLE it_bstkd
    WHERE cancelado EQ abap_false.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CONTRATO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = it_bstkd
        return_tab      = it_return
        dynpfld_mapping = tl_dselc.

    TRY .
        r_value = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD f4_solov.

    TYPES: BEGIN OF ty_solov,
             nro_sol_ov TYPE zsdt0051-nro_sol_ov,
             vkbur      TYPE zsdt0051-vkbur,
             auart      TYPE zsdt0051-auart,
             inco1      TYPE zsdt0051-inco1,
             matnr      TYPE zsdt0051-matnr,
           END OF ty_solov.

    DATA: it_solov TYPE  TABLE OF ty_solov.

    SELECT nro_sol_ov vkbur auart inco1 matnr
      FROM zsdt0051
      INTO TABLE it_solov
      WHERE param_espec EQ 'A'.

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
        r_value = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD f4_instrucao.

    DATA: l_posnr  TYPE posnr_va,
          l_editar TYPE char1.

    CLEAR e_referencia.

    MOVE-CORRESPONDING obj_fmlot->get_header( nro_sol_ov = s_solov-low
                                             ) TO it_0051.

    it_0051-%nro_sol_ov = it_0051-nro_sol_ov.
    it_0051-%bstkd      = it_0051-bstkd.

    TYPES: BEGIN OF ty_f4,
             zseq_inst TYPE zseq_inst,
             instrucao TYPE zsded030,
             matnr     TYPE matnr,
             werks     TYPE werks_ext,
             ponto_c   TYPE lifnr,
             terminal  TYPE lifnr,
             charg     TYPE charg_d,
           END OF ty_f4.

    DATA: it_f4_ins TYPE TABLE OF ty_f4.

    IF it_new-posnr IS INITIAL AND it_new-referencia IS INITIAL.
      l_referencia = l_referencia + 1.
    ELSEIF it_new-posnr IS NOT INITIAL.
      l_referencia = it_new-posnr.
    ELSEIF it_new-referencia IS NOT INITIAL.
      l_referencia = it_new-referencia.
    ENDIF.

    l_posnr        = l_referencia.
    l_editar       = COND #( WHEN lv_acao = 'BNT_EDIT' OR lv_acao = 'BNT_QTD' THEN abap_true
                                                                              ELSE abap_false ).

*-CS2023000189-19.04.2023-#108709-JT-inicio
    CALL FUNCTION 'ZSD_SELECAO_FORMACAO_LOTE'
      EXPORTING
        i_nro_sol_ov = it_0051-%nro_sol_ov
        i_contrato   = it_0051-%bstkd
        i_referencia = l_referencia
        i_posnr      = l_posnr
        i_editar     = l_editar
      IMPORTING
        e_back       = l_back
      TABLES
        t_retorno    = t_zsdt0328.

    TRY .
        r_value      = t_zsdt0328[ 1 ]-zseq_inst.
        e_referencia = l_referencia.
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

    IF l_back = abap_true.
      CLEAR: r_value.
      EXIT.
    ENDIF.

    IF lv_acao = 'BNT_EDIT' OR lv_acao = 'BNT_QTD'.
      IF t_zsdt0328[] IS NOT INITIAL.
        READ TABLE it_0066 INTO DATA(w_0066) INDEX l_value_edit.
        CLEAR: it_new-volum, it_new-zmeng,
               w_0066-volum, w_0066-zmeng.

        LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE referencia = l_referencia.
          it_new-volum       = w_0066-volum       + w_zsdt0328-quantidade.
          it_new-zmeng       = w_0066-zmeng       + w_zsdt0328-btgew.
          w_0066-volum       = w_0066-volum       + w_zsdt0328-quantidade.
          w_0066-zmeng       = w_0066-zmeng       + w_zsdt0328-btgew.
        ENDLOOP.
      ENDIF.
      MODIFY it_0066 FROM w_0066  INDEX l_value_edit.
    ENDIF.

*-CS2023000189-04.09.2023-#122555-JT-inicio
    IF lv_acao = 'BNT_EDIT'. "*-CS2023000189-04.09.2023-#122555-JT
      CLEAR: r_value.
    ENDIF.

    IF lv_acao = 'BNT_QTD'.
      CLEAR: r_value.
      PERFORM f_check_zmeng.
    ENDIF.
*-CS2023000189-04.09.2023-#122555-JT-fim

*    SELECT *
*      FROM zsdt0045
*      INTO CORRESPONDING FIELDS OF TABLE it_f4_ins
*      WHERE objek EQ it_0051-%nro_sol_ov
*     AND contrato EQ it_0051-%bstkd.
*
*    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*      EXPORTING
*        retfield        = 'ZSEQ_INST'
*        dynpprog        = sy-repid
*        dynpnr          = sy-dynnr
*        value_org       = 'S'
*      TABLES
*        value_tab       = it_f4_ins
*        return_tab      = it_return
*        dynpfld_mapping = tl_dselc.
*
*    TRY .
*        r_value = it_return[ 1 ]-fieldval.
*      CATCH cx_sy_itab_line_not_found.
*        CLEAR r_value.
*    ENDTRY.
*-CS2023000189-19.04.2023-#108709-JT-fim

  ENDMETHOD.

  METHOD get_0045.

    DATA: l_quant TYPE i.

    CLEAR l_quant.

    IF i_seq IS NOT INITIAL.
      SELECT SINGLE * FROM zsdt0045
            INTO @DATA(wa_0045)
            WHERE zseq_inst EQ @i_seq.
    ENDIF.

    IF  i_ins IS NOT INITIAL.
      SELECT COUNT(*) FROM zsdt0045
        INTO @DATA(qtd)
         WHERE instrucao EQ @i_ins.
      IF qtd EQ 1.

        SELECT SINGLE * FROM zsdt0045
          INTO wa_0045
         WHERE instrucao EQ i_ins.

      ENDIF.
    ENDIF.

    CHECK wa_0045 IS NOT INITIAL.

    it_new            = CORRESPONDING #( wa_0045 ).
    it_new-referencia = i_referencia.  "*-CS2023000189-19.04.2023-#108709-JT

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF t_zsdt0328[] IS NOT INITIAL.
      CLEAR: wa_0045-quantidade, wa_0045-btgew.
      LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE referencia = it_new-referencia.
        l_quant            = l_quant            + 1.
        wa_0045-quantidade = wa_0045-quantidade + w_zsdt0328-quantidade.
        wa_0045-btgew      = wa_0045-btgew      + w_zsdt0328-btgew.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    it_new-instrucao   = wa_0045-instrucao.
    it_new-charg       = wa_0045-safra.
    it_new-volum       = wa_0045-quantidade.
    it_new-nro_sol_ov  = wa_0045-objek.
    it_new-kunnr       = it_new-werks.
    it_new-charg_ori   = COND #( WHEN l_quant  > 1 THEN '*'  "*-CS2023000189-19.04.2023-#108709-JT-inicio
                                                   ELSE wa_0045-charg ).
    it_new-ponto_c     = wa_0045-ponto_c.
    it_new-zmeng       = wa_0045-btgew.
    it_new-lentrega    = obj_fmlot->get_local_entrega( wa_0045-terminal_estuf ).
    it_new-terminal    = wa_0045-terminal_estuf.
    it_new-zieme       = wa_0045-gewei.

    CLEAR: it_new-dmbtr,  it_new-pmein.

  ENDMETHOD.

  METHOD convert_meng.

    CHECK: i_matnr IS NOT INITIAL,
           i_zieme IS NOT INITIAL,
           i_pmein IS NOT INITIAL,
           i_zmeng IS NOT INITIAL.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = i_matnr
        i_in_me              = i_zieme
        i_out_me             = i_pmein
        i_menge              = i_zmeng
      IMPORTING
        e_menge              = at_zmeng
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

  ENDMETHOD.

  METHOD get_zseq.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = i_value
      IMPORTING
        number      = r_value.

    at_hist = r_value.

  ENDMETHOD.


  METHOD calc_lote.
    obj_fmlot->convert_meng( i_matnr = it_new-matnr
                             i_zieme = it_new-zieme
                             i_pmein = it_new-pmein
                             i_zmeng = it_new-zmeng ).
    TRY.
        r_vlrtot =  obj_fmlot->at_zmeng * i_dmbtr.
      CATCH  cx_sy_arithmetic_overflow.
    ENDTRY.

  ENDMETHOD.

  METHOD calc_usd.

    at_libra_to = i_libra_to.

    MULTIPLY at_libra_to BY at_usd.

    CASE i_zieme.
      WHEN 'TO'.
        at_lib_to_f = at_libra_to.
        MULTIPLY at_lib_to_f BY obj_fmlot->at_zmeng.
      WHEN OTHERS.
        at_lib_to_f = ( at_libra_to * obj_fmlot->at_zmeng ) / 1000.
    ENDCASE.

  ENDMETHOD.

  METHOD get_desc_cen.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE name1 FROM t001w INTO value WHERE werks EQ input.
  ENDMETHOD.

  METHOD get_desc_mat.
    CHECK input IS NOT INITIAL.

    DATA: lv_matnr18 TYPE matnr18.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = input
          IMPORTING
            output = lv_matnr18.

    SELECT SINGLE maktx FROM makt INTO value WHERE matnr EQ lv_matnr18 AND spras EQ sy-langu.
  ENDMETHOD.

  METHOD get_desc_lfa.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ input.
  ENDMETHOD.

  METHOD get_desc_kna.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE name1 FROM kna1 INTO value WHERE kunnr EQ input.
  ENDMETHOD.

  METHOD get_local_entrega.

    CHECK input IS NOT INITIAL.

    SELECT SINGLE kn~kunnr
      FROM lfa1 AS lf
      INNER JOIN kna1 AS kn  ON lf~stcd1 = kn~stcd1
    "  INTO TABLE @DATA(TB)
      INTO value
    WHERE lf~lifnr = input.

  ENDMETHOD.

  METHOD get_instrucao_f4.

    DATA: l_ref  TYPE numc10.

    obj_fmlot->get_0045( i_seq        = obj_fmlot->f4_instrucao( IMPORTING e_referencia = l_ref )
                         i_referencia = l_referencia ).

  ENDMETHOD.

  METHOD set_desc.

    it_new-werks_desc    = obj_fmlot->get_desc_cen( it_new-werks ).
    it_new-matnr_desc    = obj_fmlot->get_desc_mat( it_new-matnr ).
    it_new-terminal_desc = obj_fmlot->get_desc_lfa( it_new-terminal ).
    it_new-ponto_c_desc  = obj_fmlot->get_desc_lfa( it_new-ponto_c ).
    it_new-lentrega_desc = obj_fmlot->get_desc_kna( it_new-lentrega ).
    it_new-kunnr_desc    = obj_fmlot->get_desc_kna( |{ it_new-kunnr ALPHA = IN }| ).

    it_new-vlrtot = obj_fmlot->calc_lote( it_new-dmbtr ).

    obj_fmlot->calc_usd( EXPORTING i_libra_to = it_new-libra_to
                                   i_zieme    = it_new-zieme ).

    it_new-usd_to = obj_fmlot->at_libra_to.
    it_new-vlr_tot_frm_usd = obj_fmlot->at_lib_to_f.

  ENDMETHOD.

  METHOD set_layout.

    CLEAR: wa_layout, wa_variant, wa_stable.

    wa_layout = VALUE #(
                        zebra      = abap_true
                        no_rowins  = abap_true
                        stylefname = 'ESTILO'
                        info_fname = 'COLOR'
                        sel_mode   = 'A'
                        ).

    wa_variant = VALUE #(
                         report = sy-repid
                         ).

    wa_stable = VALUE #(
                        row = abap_true
                        col = abap_true
                        ).

  ENDMETHOD.

  METHOD show_msgre.

    obj_fmlot->set_erros( ).

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_show      = abap_true
        i_repid     = sy-repid
        i_popup     = 1
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.

  ENDMETHOD.

  METHOD set_erros.

    FREE: tg_msg_ret.
    CASE lv_acao.
      WHEN 'BNT_ADD'.
        CHECK sy-ucomm NE 'SAVE'.

        LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE no_out IS INITIAL.

          campo = |IT_NEW-{ <fcat>-fieldname }|.
          ASSIGN (campo) TO <fs_campo>.

          CASE  <fcat>-fieldname.
            WHEN
                'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5'.
            WHEN OTHERS.
              IF <fs_campo> IS INITIAL.

                APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |Campo { <fcat>-scrtext_l } obrigatório!|
                               ) TO tg_msg_ret.
*** BUG 58231 - CSB - Inicio
              ELSE.
                IF <fcat>-fieldname EQ 'LGORT'.

                  campo = |IT_NEW-{ <fcat>-fieldname }|.
                  ASSIGN (campo) TO <fs_campo>.

                  campo2 = |IT_NEW-WERKS|.
                  ASSIGN (campo2) TO <fs_campo2>.

                  CLEAR: vlgort.
                  SELECT SINGLE lgort
                        INTO vlgort
                        FROM t001l
                        WHERE werks EQ <fs_campo2>
                          AND lgort EQ <fs_campo> .

                  IF vlgort IS INITIAL.
                    APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |Campo { <fcat>-scrtext_l } não existe na tabela T001L .|
                               ) TO tg_msg_ret.
                  ENDIF.

                ENDIF.
              ENDIF.
*** BUG 58231 - CSB - Fim
          ENDCASE.

        ENDLOOP.

        IF tg_msg_ret[] IS INITIAL .
          DATA vol_fardos_66 TYPE zsded029.
          DATA vol_fardos_retirar TYPE volum.

          LOOP AT it_0066 INTO DATA(wa_it_0066) WHERE instrucao = it_new-instrucao AND werks = it_new-werks AND posnr IS INITIAL.
            ADD wa_it_0066-volum TO vol_fardos_66.
          ENDLOOP.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO @DATA(vol_fardos_66_auxi)
           WHERE instrucao = @it_new-instrucao
           AND   werks   = @it_new-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

          ADD it_new-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE @DATA(it_fardos_45)
           WHERE instrucao = @it_new-instrucao
           AND    werks    = @it_new-werks.

          DATA vol_fardos_45 TYPE n LENGTH 10.

          LOOP AT it_fardos_45 INTO DATA(w_0045).
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO tg_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

      WHEN 'BNT_EDIT'.

        LOOP AT it_0066 INTO DATA(ln_0066) WHERE vbeln IS INITIAL.
          DATA(tabix) = sy-tabix.
          LOOP AT it_fcat ASSIGNING <fcat> WHERE no_out IS INITIAL.

            CASE <fcat>-fieldname.
              WHEN 'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'WERKS_DESC' OR
                'MATNR_DESC' OR
                'TERMINAL_DESC' OR
                'PONTO_C_DESC' OR
                'LENTREGA_DESC' OR
                'KUNNR_DESC' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5' OR
                'CHARG_ORI'.
                CONTINUE.
            ENDCASE.

            campo = |LN_0066-{ <fcat>-fieldname }|.
            ASSIGN (campo) TO <fs_campo>.

            IF <fs_campo> IS INITIAL.
              APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |Campo { <fcat>-scrtext_l } obrigatório! Linha { tabix }.|
                             ) TO tg_msg_ret.
*** BUG 58231 - CSB - Inicio
            ELSE.
              IF <fcat>-fieldname EQ 'LGORT'.

                campo = |LN_0066-{ <fcat>-fieldname }|.
                ASSIGN (campo) TO <fs_campo>.

                campo2 = |LN_0066-WERKS|.
                ASSIGN (campo2) TO <fs_campo2>.

                CLEAR: vlgort.
                SELECT SINGLE lgort
                      INTO vlgort
                      FROM t001l
                      WHERE werks EQ <fs_campo2>
                        AND lgort EQ <fs_campo> .

                IF vlgort IS INITIAL.
                  APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |Campo { <fcat>-scrtext_l } não existe na tabela T001L { tabix }.|
                             ) TO tg_msg_ret.
                ENDIF.

              ENDIF.
            ENDIF.
*** BUG 58231 - CSB - Fim
          ENDLOOP.
        ENDLOOP.

        IF tg_msg_ret[] IS INITIAL .

*//////   LOCALIZA O REGISTRO QUE ESTÁ SENDO EDITADO
          it_edit1 =
                VALUE #( FOR ls_0066 IN it_0066 WHERE ( color EQ 'C311' )
              ( CORRESPONDING #( ls_0066 ) )
            ).
*/////
          READ TABLE it_edit1 INTO wa_edit1 INDEX 1.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO vol_fardos_66_auxi
           WHERE instrucao = wa_edit1-instrucao
           AND   werks   = wa_edit1-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

*//////  Totaliza o Valor Original dessa instrução
          LOOP AT it_edit1 INTO DATA(w_edit1).

            SELECT SUM( volum )
              FROM zsdt0066
              INTO @DATA(vl_fd_ret)
             WHERE nro_sol_ov = @w_edit1-nro_sol_ov
             AND   posnr   = @w_edit1-posnr.

            ADD vl_fd_ret TO vol_fardos_retirar.
          ENDLOOP.
*/////

          vol_fardos_66 = vol_fardos_66 - vol_fardos_retirar.



          ADD it_new-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE it_fardos_45
           WHERE instrucao = it_new-instrucao
           AND    werks    = it_new-werks.

          LOOP AT it_fardos_45 INTO w_0045.
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO tg_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO tg_msg_ret.
          ENDIF.
        ENDIF.



    ENDCASE.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_repid     = sy-repid
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.

    CHECK tg_msg_ret[] IS NOT INITIAL AND sy-ucomm NE 'SHOW_MSGRE'.

    MESSAGE 'Verifique o Log de Erros!' TYPE 'S' DISPLAY LIKE 'E'.

  ENDMETHOD.

  METHOD act_new_lote.

    me->set_erros( ).

    CHECK tg_msg_ret[] IS INITIAL.

    APPEND VALUE #(
                    mandt               = sy-mandt
                    nro_sol_ov          = it_new-nro_sol_ov
                    instrucao           = it_new-instrucao
                    matnr               = it_new-matnr
                    matnr_desc          = it_new-matnr_desc
                    werks               = it_new-werks
                    werks_desc          = it_new-werks_desc
                    lgort               = it_new-lgort
                    ponto_c             = it_new-ponto_c
                    ponto_c_desc        = it_new-ponto_c_desc
                    charg               = it_new-charg
                    zmeng               = it_new-zmeng
                    zieme               = it_new-zieme
                    volum               = it_new-volum
                    voleh               = it_new-voleh
                    dmbtr               = it_new-dmbtr
                    pmein               = it_new-pmein
                    vlrtot              = it_new-vlrtot
                    waerk               = it_new-waerk
                    terminal            = it_new-terminal
                    terminal_desc       = it_new-terminal_desc
                    inco1               = it_new-inco1
                    inco2               = it_new-inco2
                    lentrega            = it_new-lentrega
                    lentrega_desc       = it_new-lentrega_desc
                    kunnr               = it_new-kunnr
                    kunnr_desc          = it_new-kunnr_desc
                    status              = 'A'
                    status_form         = it_new-status_form
                    libra_to            = it_new-libra_to
                    usd_to              = it_new-usd_to
                    vlr_tot_frm_usd     = it_new-vlr_tot_frm_usd
                    classificacao       = it_new-classificacao
                    auart               = it_new-auart
                    dco                 = it_new-dco
                    aviso               = it_new-aviso
                    usnam               = sy-uname
                    data_atual          = sy-datum
                    hora_atual          = sy-uzeit
                    color               = 'C511'
                    charg_ori           = it_new-charg_ori
                    referencia          = it_new-referencia    "*-CS2023000189-19.04.2023-#108709-JT
                  ) TO it_0066.

    CLEAR it_new.

  ENDMETHOD.

  METHOD save.

    DATA: tg_0066 TYPE TABLE OF zsdt0066.

    me->set_erros( ).

    CHECK tg_msg_ret[] IS INITIAL.

    FREE: it_save, it_edit.

*    it_save =
*      VALUE #( FOR ls_0066 IN it_0066 WHERE ( posnr IS INITIAL )
*                  ( CORRESPONDING #( ls_0066 ) )
*                  ( posnr = obj_fmlot->get_posnr( EXPORTING i_referencia = ls_0066-referencia ) )
*             ).

    LOOP AT it_0066 INTO  ws_0066 WHERE posnr IS INITIAL.
      ws_0066-posnr = obj_fmlot->get_posnr( EXPORTING i_referencia = ws_0066-referencia ).
      MOVE-CORRESPONDING ws_0066 TO wa_save.
      APPEND wa_save TO it_save.
    ENDLOOP.

    it_edit =
      VALUE #( FOR ls_0066 IN it_0066 WHERE ( color EQ 'C311' )
               ( CORRESPONDING #( ls_0066 ) )
             ).

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF it_edit[] IS NOT INITIAL.
      LOOP AT it_edit INTO DATA(wa_edit).
        LOOP AT t_zsdt0328   INTO w_zsdt0328 WHERE referencia = wa_edit-posnr.
          w_zsdt0328-nro_sol_ov = wa_edit-nro_sol_ov.
          w_zsdt0328-posnr      = wa_edit-posnr.
          MODIFY t_zsdt0328  FROM w_zsdt0328 INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    DELETE it_save WHERE posnr IS INITIAL.
    DELETE it_dele WHERE posnr IS INITIAL.

    APPEND LINES OF it_save[] TO tg_0066[].
    APPEND LINES OF it_edit[] TO tg_0066[].

*-CS2023000189-19.04.2023-#108709-JT-inicio
    LOOP AT tg_0066        INTO DATA(wg_0066) WHERE charg_ori = '*'.
      l_tabix = sy-tabix.
      READ TABLE t_zsdt0328 INTO w_zsdt0328 WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                                                     posnr      = wg_0066-posnr.
      wg_0066-charg_ori       = w_zsdt0328-charg_ori.
      MODIFY tg_0066       FROM wg_0066 INDEX l_tabix TRANSPORTING charg_ori.
    ENDLOOP.
*-CS2023000189-19.04.2023-#108709-JT-fim

    IF NOT it_dele IS INITIAL.

      MODIFY zsdt0066 FROM TABLE it_dele.

*-CS2023000189-04.09.2023-#122555-JT-inicio
      LOOP AT it_dele INTO DATA(w_dele).
        UPDATE zsdt0328 SET cancelado  = abap_true
                      WHERE nro_sol_ov = w_dele-nro_sol_ov
                        AND posnr      = w_dele-posnr.
      ENDLOOP.
*-CS2023000189-04.09.2023-#122555-JT-fim

      IF sy-subrc IS INITIAL.
        IF lines( it_dele[] ) > 1.
          MESSAGE |Documentos Deletados com Sucesso!| TYPE 'S'.
        ELSE.
          MESSAGE |Documento Deletado com Sucesso!| TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE |Documentos não foram Deletados!| TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.
      FREE it_dele[].
      WAIT UP TO 2 SECONDS.

      IF tg_0066[] IS INITIAL.
        obj_fmlot->get_dados( ).
      ENDIF.

    ENDIF.

    IF it_alt_qtd[] IS NOT INITIAL.
      tg_0066[] = it_alt_qtd[].
      CHECK obj_fmlot->atualiza_qtd( tg_0066[] ) IS INITIAL.
    ENDIF.

*   CHECK tg_0066[] IS NOT INITIAL.

    IF tg_0066[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Lote não foi Alterado!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    MODIFY zsdt0066 FROM TABLE tg_0066.

    IF sy-subrc IS INITIAL.
*-CS2023000189-19.04.2023-#108709-JT-inicio
      LOOP AT tg_0066 INTO wg_0066.
        LOOP AT t_zsdt0328   INTO w_zsdt0328 WHERE referencia = wg_0066-posnr.
          w_zsdt0328-nro_sol_ov = wg_0066-nro_sol_ov.
          w_zsdt0328-posnr      = wg_0066-posnr.
          MODIFY t_zsdt0328  FROM w_zsdt0328 INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.

      LOOP AT tg_0066         INTO wg_0066.
        UPDATE zsdt0328 SET cancelado  = abap_true  "*-CS2023000189-04.09.2023-#122555-JT
                      WHERE nro_sol_ov = wg_0066-nro_sol_ov
                        AND posnr      = wg_0066-posnr.
*       DELETE FROM zsdt0328 WHERE nro_sol_ov = wg_0066-nro_sol_ov  "*-CS2023000189-04.09.2023-#122555-JT
*                              AND posnr      = wg_0066-posnr.
      ENDLOOP.

      LOOP AT tg_0066      INTO wg_0066.
        LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE nro_sol_ov = wg_0066-nro_sol_ov
                                             AND posnr      = wg_0066-posnr.
          MODIFY zsdt0328  FROM w_zsdt0328.
        ENDLOOP.
      ENDLOOP.

      COMMIT WORK.

*-CS2023000189-19.04.2023-#108709-JT-fim

*-CS2023000189-#126959-07.11.2023-JT-inicio
*-------------------------------------------
*-envia trace cotton
*-------------------------------------------
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = 'Aguarde...Integrando...'.

      LOOP AT tg_0066 INTO wg_0066.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = wg_0066-nro_sol_ov
            i_posnr      = wg_0066-posnr
            i_acao       = 'C'
          EXCEPTIONS
            OTHERS       = 1.
      ENDLOOP.
*-CS2023000189-#126959-07.11.2023-JT-fim

      obj_fmlot->set_log( tg_0066 ).
      MODIFY zsdt0083 FROM TABLE tg_save_log[].

      MESSAGE |Documentos Salvos com Sucesso!| TYPE 'S'.
      obj_fmlot->freetable( ).
      obj_fmlot->get_dados( ).

    ENDIF.

    FREE: it_dele, tg_0066, tg_save_log, it_alt_qtd[].

  ENDMETHOD.

  METHOD act_edit_lote.

    CHECK obj_fmlot->get_index( ) IS NOT INITIAL.

    IF it_0066[ obj_fmlot->get_index( ) ]-vbeln IS NOT INITIAL.
      MESSAGE |Formação de Lote já possui Ordem Gerada!| TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR: it_new, lv_acao.
      EXIT.
    ENDIF.

    TRY .
        ASSIGN it_0066[ obj_fmlot->get_index( ) ] TO FIELD-SYMBOL(<f0066>).
        IF <f0066> IS ASSIGNED.
          <f0066>-color = 'C311'.
          it_new = <f0066>.
          obj_fmlot->set_desc( ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        CLEAR it_new.
    ENDTRY.

    IF it_new-posnr IS INITIAL.
      MESSAGE |Nr. do Documento não pode ser Editado!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD get_index.

    CLEAR l_value_edit.

    CALL METHOD obj_alv->get_selected_rows
      IMPORTING
        et_index_rows = it_index.

    CASE input.
      WHEN 'BNT_LTO1' OR 'BNT_QTD'.

        IF it_index[] IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Selecione ao menos uma Linha!|.
          CLEAR lv_acao.
          EXIT.
        ENDIF.

        r_value      = it_index[ 1 ]-index.
        l_value_edit = r_value.

      WHEN 'BNT_TRACE'.
        IF lines( it_index[] ) EQ 1.
          r_value = it_index[ 1 ].
        ELSE.
          FREE: it_index[].
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Selecione Somente uma Linha para Reenvio!|.
        ENDIF.

      WHEN OTHERS.

        IF lines( it_index[] ) EQ 1.
          r_value      = it_index[ 1 ].
          l_value_edit = r_value.
        ELSE.
          FREE: it_index[].
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Selecione Somente uma Linha para Edição!|.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD act_dele_lote.
    TRY .
        CHECK obj_fmlot->confirm(
                                obj_fmlot->remove_zero(
                                                      it_0066[ obj_fmlot->get_index( ) ]-posnr
                                                      )
                               ) EQ 1.
      CATCH cx_sy_itab_line_not_found.
        EXIT.
    ENDTRY.

    IF it_0066[ obj_fmlot->get_index( ) ]-vbeln IS INITIAL.

      it_0066[ obj_fmlot->get_index( ) ]-status = 'D'.
      it_0066[ obj_fmlot->get_index( ) ]-color = 'C600'.

      APPEND CORRESPONDING #( it_0066[ obj_fmlot->get_index( ) ] ) TO it_dele.
    ELSE.
      MESSAGE |Formação de Lote já possui Ordem Gerada!| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

*-CS2023000189-19.04.2023-#108709-JT-inicio
  METHOD act_reenvia_trace.

    DATA: l_nro_sol_ov TYPE zsdt0066-nro_sol_ov,
          l_posnr      TYPE zsdt0066-posnr.

    CHECK obj_fmlot->get_index( 'BNT_TRACE' ) IS NOT INITIAL.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando...'.

    CHECK it_0066[ obj_fmlot->get_index( 'BNT_TRACE' ) ]-vbeln IS NOT INITIAL.

    l_nro_sol_ov = it_0066[ obj_fmlot->get_index( ) ]-nro_sol_ov.
    l_posnr      = it_0066[ obj_fmlot->get_index( ) ]-posnr.

    DATA(l_task) = 'TRACE_ORDEM_VENDA' && l_nro_sol_ov && l_posnr.

    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
      EXPORTING
        i_nro_sol_ov = l_nro_sol_ov
        i_posnr      = l_posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.

  ENDMETHOD.
*-CS2023000189-19.04.2023-#108709-JT-fim

  METHOD confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = |Deseja Deletar o Item { input }?|
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = r_value.

  ENDMETHOD.

  METHOD remove_zero.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = r_value.

  ENDMETHOD.

  METHOD modify_alv.

    CHECK lv_acao CS 'EDIT'.

    LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<f0066>) WHERE posnr EQ it_new-posnr.
      CHECK NOT <f0066>-posnr IS INITIAL.
      MOVE-CORRESPONDING it_new TO <f0066>.
    ENDLOOP.

  ENDMETHOD.

  METHOD liberar_lote.

    obj_fmlot->get_index( ).

    CHECK NOT it_index[] IS INITIAL.

    DATA(wa_del) = it_0066[ it_index[ 1 ] ].

    CHECK wa_del-nro_sol_ov IS NOT INITIAL
            OR wa_del-posnr IS NOT INITIAL
        OR wa_del-instrucao IS NOT INITIAL.

    UPDATE zsdt0066 SET status = 'L'
           WHERE nro_sol_ov EQ wa_del-nro_sol_ov
                  AND posnr EQ wa_del-posnr
              AND instrucao EQ wa_del-instrucao.
    IF sy-subrc IS INITIAL.
      MESSAGE |Solicitação { wa_del-nro_sol_ov } - { wa_del-posnr } Liberada!| TYPE 'S'.
      obj_fmlot->get_dados( ).
    ENDIF.

  ENDMETHOD.

  METHOD check_no_out .

    CLEAR r_value.
    CHECK line_exists( fields[ field = input ] ).
    r_value = abap_true.

  ENDMETHOD.

  METHOD no_out.

    APPEND VALUE #( field = 'MANDT' )        TO fields.
    APPEND VALUE #( field = 'STATUS' )       TO fields.
    APPEND VALUE #( field = 'STATUS_TRACE' ) TO fields.
    APPEND VALUE #( field = 'STATUS_FORM' )  TO fields.
    APPEND VALUE #( field = 'USNAM' )        TO fields.
    APPEND VALUE #( field = 'DATA_ATUAL' )   TO fields.
    APPEND VALUE #( field = 'HORA_ATUAL' )   TO fields.
    APPEND VALUE #( field = 'COLOR' )        TO fields.

  ENDMETHOD.

  METHOD split.

    SPLIT input AT '#' INTO TABLE t_string.

  ENDMETHOD.

  METHOD freetable.

    FREE: it_save,
          it_0066,
          it_0051,
          it_new,
          it_edit,
          it_dele,
          it_0066_old,
          t_zsdt0328,
          it_log_old,
          it_alt_qtd,
          tg_save_log,
          wa_0066_old,
          wa_0066,
          wa_variant,
          wa_layout,
          wa_stable,
          it_fcat,
          tg_msg_ret,
          wg_mensagem,
          lv_acao,
          cont,
          str_l,
          campo,
          tam.

    UNASSIGN: <fs_campo>,
              <fs_campo1>.

  ENDMETHOD.

  METHOD  en_de_queue.

    MOVE-CORRESPONDING obj_fmlot->get_header( nro_sol_ov = s_solov-low
                                            ) TO it_0051.

    DATA function TYPE string.
    function = |{ dir }QUEUE_EZSDT0066|.

    CALL FUNCTION function
      EXPORTING
        nro_sol_ov     = it_0051-nro_sol_ov
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CHECK sy-subrc IS NOT INITIAL.
    MESSAGE |Nº de Solicitação Bloqueado pelo Usuário { sy-msgv1 }| TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO CURRENT TRANSACTION.

  ENDMETHOD.

  METHOD scrtext.
    CLEAR r_value.
    CASE input.
      WHEN 'WERKS_DESC'.    r_value = 'Desc. Centro'.
      WHEN 'MATNR_DESC'.    r_value = 'Desc. Material'.
      WHEN 'TERMINAL_DESC'. r_value = 'Desc. Terminal'.
      WHEN 'PONTO_C_DESC'.  r_value = 'Desc. Ponto Coleta.'.
      WHEN 'LENTREGA_DESC'. r_value = 'Desc. Local de Entrega'.
      WHEN 'KUNNR_DESC'.    r_value = 'Desc. Cliente'.
      WHEN 'AVISO'.         r_value = 'Aviso'.
    ENDCASE.

  ENDMETHOD.

  METHOD check_botao.

*    Verifica se existe Algum Vbeln Preenchido Se Existir Exibe o Botão ALterar Qtd
    DATA(qtd) = REDUCE i( INIT x = 0 FOR ls IN it_0066
                        WHERE ( vbeln IS NOT INITIAL )
                            NEXT x = x + 1 ).

    IF qtd IS INITIAL.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'BNT_QTD'.
            screen-invisible = 1.
            MODIFY SCREEN.
          WHEN OTHERS.

            CHECK lv_acao CS 'BNT_'.

            IF screen-name CS 'BNT_' AND
               screen-name NE lv_acao .
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.

        ENDCASE.
      ENDLOOP.

    ELSE.
      CHECK lv_acao CS 'BNT_'.

      LOOP AT SCREEN.
        IF screen-name CS 'BNT_' AND
           screen-name NE lv_acao .
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD alterar_qtd.

    CHECK obj_fmlot->get_index( sy-ucomm ) IS NOT INITIAL.

    IF it_0066[ obj_fmlot->get_index( ) ]-vbeln IS INITIAL.
      MESSAGE |Formação de Lote não possui Ordem Gerada!| TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR lv_acao.
      EXIT.
    ENDIF.

    it_new = it_0066[ obj_fmlot->get_index( sy-ucomm ) ].

    me->at_qtd = it_new-zmeng.

  ENDMETHOD.

  METHOD set_lock_unlock.

    CHECK lv_acao EQ 'BNT_QTD'.

    LOOP AT SCREEN.
      IF screen-name CS 'IT_NEW-'.
        CASE screen-name.
          WHEN 'IT_NEW-ZMENG'.
          WHEN 'IT_NEW-VOLUM'.
          WHEN OTHERS.
            screen-input = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_zmeng.

    DATA: l_total TYPE vbfa-rfmng.

    FREE: r_erro, l_total.

    SELECT *
      FROM vbfa
      INTO TABLE @DATA(t_vbfa)
     WHERE vbelv   = @it_new-vbeln
       AND vbtyp_n = 'J'
       AND vbtyp_v = 'C'.

    IF sy-subrc = 0.
      LOOP AT t_vbfa INTO DATA(w_vbfa).
        l_total = l_total + w_vbfa-rfmng.
      ENDLOOP.

      IF it_new-zmeng < l_total.
        MESSAGE s024(sd) WITH 'Quantidade da Formação de Lote  é menor '
                              'que total de Remessas Geradas: '  l_total DISPLAY LIKE 'E'.
*       it_new-zmeng = me->at_qtd.
        r_erro       = abap_true.
        EXIT.           "*-CS2023000189-19.04.2023-#108709-JT
      ENDIF.

*     IF me->at_qtd > it_new-zmeng.
*       MESSAGE 'Não é possivel Diminiur a Quantidade da Formação de Lote!' TYPE 'S' DISPLAY LIKE 'E'.
**      it_new-zmeng = me->at_qtd.
*       r_erro       = abap_true.
*       EXIT.           "*-CS2023000189-19.04.2023-#108709-JT
*     ENDIF.
    ENDIF.

    LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<f0066>)
            WHERE nro_sol_ov EQ it_new-nro_sol_ov
              AND posnr EQ it_new-posnr
              AND vbeln IS NOT INITIAL.
      <f0066> = CORRESPONDING #( it_new ).
    ENDLOOP.

  ENDMETHOD.

  METHOD atualiza_qtd.

    CALL FUNCTION 'ZSDMF002_ATUALI_OV_SOLICITACAO'
      IMPORTING
        erro              = return
      TABLES
        ti_form_lote      = input
      EXCEPTIONS
        ov_nao_encontrada = 1
        OTHERS            = 2.

  ENDMETHOD.

ENDCLASS.

***********************************************************************************************
* SELEÇÃO DE DADOS
***********************************************************************************************
START-OF-SELECTION.

  obj_fmlot->en_de_queue( input = s_solov-low
                            dir = 'EN' ).
  CHECK obj_fmlot->get_dados( ) IS INITIAL.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

*-CS2023000189-19.04.2023-#108709-JT-inicio
* IF sy-ucomm <> 'BNT_EDIT'.
  LOOP AT SCREEN.
    IF screen-name = 'IT_NEW-VOLUM'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
* ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'CANCEL'.
      obj_fmlot->get_dados( ).
      CLEAR it_new.
    WHEN 'SAVE'.
      obj_fmlot->save( ).
      obj_fmlot->get_dados( ).
      CLEAR it_new.
    WHEN 'BNT_ADD'.
      lv_acao = sy-ucomm.
      obj_fmlot->act_new_lote( ).
    WHEN 'BNT_EDIT'.
      lv_acao = sy-ucomm.
      obj_fmlot->act_edit_lote( ).
    WHEN 'BNT_DEL'.
      lv_acao = sy-ucomm.
      obj_fmlot->act_dele_lote( ).
*-CS2023000189-19.04.2023-#108709-JT-inicio
    WHEN 'BNT_TRACE'.
      lv_acao = sy-ucomm.
      obj_fmlot->act_reenvia_trace( ).
      obj_fmlot->get_dados_trace( ).
      CLEAR it_new.
*-CS2023000189-19.04.2023-#108709-JT-fim
    WHEN 'SHOW_MSGRE'.
      obj_fmlot->show_msgre( ).
    WHEN 'BNT_LTO'.
      lv_acao = sy-ucomm.
      obj_fmlot->liberar_lote( ).
    WHEN 'BNT_QTD'.
      lv_acao = sy-ucomm.
      obj_fmlot->alterar_qtd( ).
    WHEN 'REFRESH'.
      obj_fmlot->get_dados( ).
      CLEAR it_new.
    WHEN 'BTNF4'.
      obj_fmlot->get_instrucao_f4( ).
      obj_fmlot->set_desc( ).

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_fcat OUTPUT.

  FREE it_fcat.

  ASSIGN 'TY_0066' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  DATA(lines_fcat) = lines( it_fcat[] ).

  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

    IF <fcat>-fieldname EQ 'VBELN'.
      <fcat>-hotspot = abap_true.
    ENDIF.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF <fcat>-fieldname EQ 'NRO_SOL_OV' OR
       <fcat>-fieldname EQ 'INSTRUCAO'.
      <fcat>-hotspot = abap_true.
    ENDIF.
    IF <fcat>-fieldname EQ 'STATUS_TRACE'.
      <fcat>-icon    = abap_true.
      <fcat>-hotspot = abap_true.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<f66>).

      campo = <fcat>-fieldname.
      ASSIGN COMPONENT campo OF STRUCTURE <f66> TO <fs_campo1>.

      WRITE <fs_campo1> TO str_l.
      IF strlen( str_l ) > tam.
        tam = strlen( str_l ).
      ENDIF.

    ENDLOOP.

    cont = 0.

    DO .
      IF lines_fcat < cont.
        EXIT.
      ENDIF.

      ADD 1 TO cont.
      campo = |TEXT-A{ cont }|.
      ASSIGN (campo) TO <fs_campo>.

      obj_fmlot->split( <fs_campo> ).
      IF obj_fmlot->t_string[ 1 ]-stn EQ <fcat>-fieldname.

        IF obj_fmlot->t_string[ 1 ]-stn EQ <fcat>-fieldname.
          <fcat>-scrtext_s = <fcat>-scrtext_m = <fcat>-scrtext_l = obj_fmlot->t_string[ 2 ]-stn.
          <fcat>-col_pos = obj_fmlot->t_string[ 3 ]-stn.
        ENDIF.

        IF <fcat>-inttype EQ 'P'.
          <fcat>-intlen = <fcat>-outputlen = obj_fmlot->t_string[ 4 ]-stn.
        ELSE.
          IF tam IS INITIAL.
            <fcat>-intlen = <fcat>-outputlen = obj_fmlot->t_string[ 4 ]-stn.
          ELSE.
            <fcat>-outputlen =  COND #( WHEN tam >= 200 THEN obj_fmlot->t_string[ 4 ]-stn ELSE tam  ).    "TAM  + 1.
            <fcat>-intlen = <fcat>-outputlen.
          ENDIF.

        ENDIF.

        <fcat>-no_out =
        SWITCH #( obj_fmlot->t_string[ 5 ]-stn
                                              WHEN abap_true THEN obj_fmlot->t_string[ 5 ]-stn
                                              ELSE abap_false ).
        CLEAR tam.
        EXIT.

      ENDIF.

    ENDDO.
  ENDLOOP.

  DELETE it_fcat WHERE col_pos = 0.

  CHECK obj_alv IS NOT INITIAL.

  CALL METHOD obj_alv->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fcat.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POPULA_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE popula_obj OUTPUT.

  obj_fmlot->check_botao( ).
  obj_fmlot->set_lock_unlock( ).

  IF obj_cont IS INITIAL.

    CREATE OBJECT obj_cont
      EXPORTING
        container_name = 'CC_FORMACAODELOTE'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = obj_cont
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    obj_fmlot->set_layout( ).

    CREATE OBJECT obj_events.

    SET HANDLER: obj_events->on_click FOR obj_alv.


    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = wa_variant
        i_save                        = abap_true
      CHANGING
        it_outtab                     = it_0066[]
        it_fieldcatalog               = it_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.

********&---------------------------------------------------------------------*
********&      Module  CHECK_OBRIGATORIO  INPUT
********&---------------------------------------------------------------------*
********       text
********----------------------------------------------------------------------*
MODULE check_obrigatorio INPUT.

  obj_fmlot->set_desc( ).
  obj_fmlot->modify_alv( ).
*  OBJ_FMLOT->SET_ERROS( ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  F4_INSTRUCAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_instrucao INPUT.
  obj_fmlot->get_0045( i_seq = obj_fmlot->f4_instrucao( ) ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_0045  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_0045 INPUT.
  obj_fmlot->get_0045( i_ins = it_new-instrucao ).
  obj_fmlot->set_desc( ).
*  OBJ_FMLOT->SET_ERROS( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_ZMENG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zmeng INPUT.

  PERFORM f_check_zmeng.  "*-CS2023000189-04.09.2023-#122555-JT-inicio

ENDMODULE.

*-CS2023000189-04.09.2023-#122555-JT-inicio
FORM f_check_zmeng.

*  FREE: IT_ALT_QTD.

  obj_fmlot->set_desc( ).
  DATA(_erro) = obj_fmlot->check_zmeng( ).

  CHECK lv_acao EQ 'BNT_QTD'.
  CHECK _erro = abap_false.

  APPEND CORRESPONDING #( it_new ) TO it_alt_qtd.

ENDFORM.
*-CS2023000189-04.09.2023-#122555-JT-fim
