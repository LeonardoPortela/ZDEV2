CLASS zcl_pm_data_equipament DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_pm_data_equipament .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_pm_data_equipament IMPLEMENTATION.


  METHOD zif_pm_data_equipament~check_categoria.

    TYPES: BEGIN OF y_zval_abast,
             zval TYPE equi-eqtyp,
           END OF y_zval_abast.

    TYPES: BEGIN OF y_zval_imple,
             zval TYPE equi-eqart,
           END OF y_zval_imple.

    TYPES:BEGIN OF y_param_abast,
            zval  TYPE ztparam-zval,
            const TYPE ztparam-const,
          END OF y_param_abast.

    TYPES: BEGIN OF y_param_imple,
             zval  TYPE ztparam-zval,
             const TYPE ztparam-const,
           END OF y_param_imple.

    DATA: lc_tp_obj    TYPE ztparam-param VALUE 'TP_OBJ',
          ls_tp_implem TYPE ztparam-param VALUE 'TP_IMPLEM',
          lc_x(1)      VALUE 'X'.

    DATA: it_zval_abast  TYPE TABLE OF y_zval_abast,
          it_zval_imple  TYPE TABLE OF y_zval_imple,
          rg_abast       TYPE RANGE OF ztparam-zval,
          rg_imple       TYPE RANGE OF ztparam-zval,
          it_param_imple TYPE TABLE OF y_param_imple,
          it_param_abast TYPE TABLE OF y_param_abast.



    r_if_pm_data_equipament = me.



    me->zif_pm_data_equipament~at_check_categoria = abap_false.
    me->zif_pm_data_equipament~at_check_impl = abap_false.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_eqtyp ) TO rg_abast.

    FREE: it_param_abast[].
    SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param_abast
    WHERE param EQ lc_tp_obj
    AND zval    IN rg_abast
    AND abastec EQ lc_x.

    IF it_param_abast IS NOT INITIAL.
      me->zif_pm_data_equipament~at_check_categoria = abap_true.
    ENDIF.

    "=======================================================================================
    APPEND VALUE #( sign = 'I' option = 'EQ' low = i_eqart ) TO rg_imple.

    FREE: it_param_imple[].
    SELECT zval const
    FROM ztparam
    INTO CORRESPONDING FIELDS OF TABLE it_param_imple
    WHERE param EQ ls_tp_implem
      AND zval  IN rg_imple.

    "Se tiver informações na tabela é porque o equipamento é um implemento, senão tiver é porque não é um implemento.
    IF it_param_imple IS NOT INITIAL.
      me->zif_pm_data_equipament~at_check_impl = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD zif_pm_data_equipament~criar_doc_para_ponto_medicao.

    r_if_pm_data_equipament  = me.

    CHECK i_pos_contador IS NOT INITIAL AND i_point IS NOT INITIAL.

    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point    = i_point
        secondary_index      = ' '
        reading_date         = i_idate
        reading_time         = i_itime
        short_text           = 'Automação criar eqpto IE01'
        reader               = sy-uname
        origin_indicator     = ' '
        reading_after_action = ' '
        recorded_value       = i_pos_contador
        recorded_unit        = ' '
        difference_reading   = ' '
        code_version         = ' '
        "USER_DATA            = ' '
        check_custom_duprec  = ' '
        with_dialog_screen   = ' '
        prepare_update       = 'X'
        commit_work          = 'X'
        wait_after_commit    = 'X'
      IMPORTING
        measurement_document = me->zif_pm_data_equipament~at_doc_medicao
      EXCEPTIONS
        no_authority         = 1
        point_not_found      = 2
        index_not_unique     = 3
        type_not_found       = 4
        point_locked         = 5
        point_inactive       = 6
        timestamp_in_future  = 7
        timestamp_duprec     = 8
        unit_unfit           = 9
        value_not_fltp       = 10
        value_overflow       = 11
        value_unfit          = 12
        value_missing        = 13
        code_not_found       = 14
        notif_type_not_found = 15
        notif_prio_not_found = 16
        notif_gener_problem  = 17
        update_failed        = 18
        invalid_time         = 19
        invalid_date         = 20
        OTHERS               = 21.



  ENDMETHOD.


  METHOD zif_pm_data_equipament~criar_ordem_manutencao.

    TYPES: BEGIN OF ty_selc_plan,
             warpl TYPE vimplastat-warpl,
             mptyp TYPE vimplastat-mptyp,
             strat TYPE vimplastat-strat,
             objnr TYPE vimplastat-objnr,
           END OF ty_selc_plan.

    DATA: lv_orderid            TYPE aufnr,
          lv_refnum             TYPE ifrefnum,
          lv_oper_no            TYPE objidext,
          wa_return             TYPE bapiret2,
          numero_ordem_v2       TYPE char1,
          lv_empresa            TYPE bukrs,
          _method               TYPE swo_method,
          at_orderid            TYPE aufnr VALUE 1,
          at_refnum             TYPE ifrefnum VALUE 1,
          at_seqopr             TYPE ifrefnum,
          at_oper_no            TYPE objidext,
          wa_methods            TYPE bapi_alm_order_method,
          wa_header             TYPE bapi_alm_order_headers_i,
          wa_operation          TYPE bapi_alm_order_operation,
          wa_data_general       TYPE bapi_itob,
          wa_data_generalx      TYPE bapi_itobx,
          wa_data_specific      TYPE bapi_itob_eq_only,
          wa_data_specificx     TYPE bapi_itob_eq_onlyx,
          wa_return_bapi_eqmt   TYPE bapireturn,

          wa_notifheader_export TYPE bapi2080_nothdre,
          wa_notifheader        TYPE bapi2080_nothdri,
          notif_type            TYPE char2.


    DATA: it_methods      TYPE TABLE OF bapi_alm_order_method,
          it_header       TYPE TABLE OF bapi_alm_order_headers_i,
          it_header_up    TYPE TABLE OF bapi_alm_order_headers_up,
          it_operation    TYPE TABLE OF bapi_alm_order_operation,
          it_return       TYPE TABLE OF bapiret2,
          t_return        TYPE TABLE OF bapiret2,
          it_selc_plan    TYPE TABLE OF ty_selc_plan,
          it_notification TYPE STANDARD TABLE OF bapi2080_1.

    DATA: it_nota TYPE zpmt0020_t.

    DATA t_text        TYPE TABLE OF bapi_alm_text.
    DATA t_text_lines  TYPE TABLE OF bapi_alm_text_lines.
    DATA t_tab_text    TYPE TABLE OF string.
    DATA w_tab_text    TYPE string.


    r_if_pm_data_equipament = me.

    FREE: it_methods, it_header, it_operation, it_return.
    CLEAR: wa_return, me->zif_pm_data_equipament~at_numero_ordem, wa_header.

    at_orderid = |{ at_orderid ALPHA = IN }|.
    at_refnum  = |{ at_refnum  ALPHA = IN }|.

    at_orderid+0(1) = '%'.
    _method = 'CREATE'.

    at_oper_no = at_orderid.
    at_oper_no+12(4) = '0010'.


    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - início
    IF order_type = 'ZPM5'. "Este tipo de ordem precisa de uma nota associada.

      wa_notifheader-funct_loc  = funct_loc.
      wa_notifheader-equipment  = equipment.
      wa_notifheader-short_text = short_text.
      wa_notifheader-priority   = '4'.
      wa_notifheader-code_group = 'F0000010'.
      wa_notifheader-coding     = '0020'.
      wa_notifheader-reportedby = sy-uname.
      wa_notifheader-maintplant = maintplant.
      wa_notifheader-planplant  = planplant.
      wa_notifheader-plangroup  = plangroup.
      wa_notifheader-pm_wkctr   = 'OFICINA'.
      notif_type = 'Z9'.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          notif_type         = notif_type
          notifheader        = wa_notifheader
          task_determination = ' '
        IMPORTING
          notifheader_export = wa_notifheader_export
        TABLES
          return             = it_return.

      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number      = wa_notifheader_export-notif_no
        IMPORTING
          notifheader = wa_notifheader_export
        TABLES
          return      = it_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = wa_return.
      WAIT UP TO 5 SECONDS.

    ENDIF.
    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    it_methods = VALUE bapi_alm_order_method_t( ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey  = at_orderid ) ).
    APPEND VALUE #( refnumber = at_refnum objecttype = 'OPERATION' method = _method  objectkey  = at_oper_no ) TO it_methods.

    IF order_type <> 'SI01'. " "FF - USER STORY 76104 - 02.02.2024
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) TO it_methods.
    ENDIF.

    APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) TO it_methods.

    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - inicio
    IF order_type = 'ZPM5'.
      CLEAR it_methods[].
      CONCATENATE at_orderid wa_notifheader_export-notif_no INTO DATA(at_notif_no).
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'CREATETONOTIF'  objectkey = at_notif_no ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE'        objectkey = at_orderid  ) TO it_methods.
      APPEND VALUE #( refnumber = at_refnum objecttype = ''          method = 'SAVE'           objectkey = at_notif_no ) TO it_methods.

      wa_header-orderid = at_orderid.
      wa_header-notif_type = 'Z9'.

      APPEND VALUE #( orderid  = at_orderid
                      notif_no = abap_true )
                TO it_header_up.

    ENDIF.
    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - fim


    SELECT SINGLE bukrs
      INTO lv_empresa
      FROM j_1bbranch
      WHERE branch EQ planplant.

*    "Bucas local de instalação.
*    z_seleciona_local_tranf(
*      EXPORTING
*        werks = plant
*      IMPORTING
*        local = DATA(local)
*    ).
*
*    IF local IS INITIAL.
*      local = funct_loc.
*    ENDIF.


*    wa_header-orderid       = lv_orderid.
    wa_header-order_type    = order_type.
    wa_header-funct_loc     = funct_loc.
    wa_header-short_text    = short_text.
    wa_header-planplant     = planplant.
    wa_header-loc_comp_code = lv_empresa.
    wa_header-bus_area      = bus_area.
    wa_header-mn_wk_ctr     = mn_wk_ctr.
    wa_header-plant         = plant.
    wa_header-maintplant    = maintplant.
    wa_header-loc_bus_area  = loc_bus_area.
    wa_header-plangroup     = plangroup.
    wa_header-equipment     = equipment.
    wa_header-costcenter    = costcenter.
    wa_header-start_date    = sy-datum.
    wa_header-priority      = priority.
    wa_header-sortfield     = sortfield.
    wa_header-estimated_costs = estimated_costs. "FF - USER STORY 76104 - 02.02.2024
    APPEND wa_header TO it_header.
    CLEAR wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
    wa_operation-description = description.
    APPEND wa_operation TO it_operation.
    CLEAR wa_operation.

    IF text_longo IS NOT INITIAL.

      text_longo = |{ short_text } { text_longo }|.
      CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
        EXPORTING
          i_string         = text_longo
          i_tabline_length = 132
        TABLES
          et_table         = t_tab_text.

      DESCRIBE TABLE t_tab_text LINES DATA(l_lines).

      APPEND VALUE #( refnumber   = at_refnum
                      objecttype  = 'TEXT'
                      method      = 'CREATE'
                      objectkey   = at_orderid )
                TO it_methods.

      APPEND VALUE #( orderid     = at_orderid
                      langu       = sy-langu
                      textstart   = 1
                      textend     = l_lines )
                TO t_text.

*      APPEND VALUE #( tdformat  = '*'
*                        tdline    =  short_text )
*                  TO t_text_lines.

      LOOP AT t_tab_text INTO w_tab_text.
        APPEND VALUE #( tdformat  = '*'
                        tdline    = w_tab_text )
                  TO t_text_lines.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
      TABLES
        it_methods    = it_methods
        it_header     = it_header
        it_header_up  = it_header_up
        it_operation  = it_operation
        it_text       = t_text
        it_text_lines = t_text_lines
        return        = it_return.

    CLEAR: wa_return.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 02 SECONDS.

    READ TABLE it_return INTO wa_return WITH KEY number = '112'.

    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - inicio
    IF wa_return-message_v2 IS INITIAL.
      READ TABLE it_return INTO wa_return WITH KEY number = '126'.
    ENDIF.
    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - fim

    CLEAR: me->zif_pm_data_equipament~at_numero_ordem.
    me->zif_pm_data_equipament~at_numero_ordem = wa_return-message_v2.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = me->zif_pm_data_equipament~at_numero_ordem
      IMPORTING
        output = me->zif_pm_data_equipament~at_numero_ordem.

    CLEAR: e_ordem.
    e_ordem = me->zif_pm_data_equipament~at_numero_ordem.

    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - inicio
*    IF order_type = 'ZPM5'.
*      "Preenche o numero da ordem na operação
*      DATA: lt_oprol3 TYPE TABLE OF bapi_alm_olist_relation,
*            lt_return TYPE TABLE OF bapiret2.
*
*      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*        EXPORTING
*          number   = e_ordem
*        TABLES
*          et_oprol = lt_oprol3
*          return   = lt_return.
*
*      free: it_nota.
*      it_nota = VALUE #( ( aufnr = me->zif_pm_data_equipament~at_numero_ordem
*                         qmnum = wa_notifheader_export-notif_no ) ).
*
*      CALL FUNCTION 'ZPM_ATUALIZA_OBJETOS_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
*        EXPORTING
*          i_aufnr = e_ordem
*          i_notas = it_nota "wa_notifheader_export-notif_no
*        TABLES
*          i_oprol = lt_oprol3.
*
*    ENDIF.
*    "#FF 12/09/2023 - Ajustes Ordens tipo ZPM5 - fim



  ENDMETHOD.


  METHOD zif_pm_data_equipament~criar_planos_manut.

    DATA: ws_header    TYPE mplan_mpla,
          it_items     TYPE TABLE OF mplan_mpos,
          it_cycles    TYPE TABLE OF mplan_mmpt,
          it_return    TYPE TABLE OF bapiret2,
          it_switch    TYPE TABLE OF mplan_numberswitch,
          it_longtexts TYPE TABLE OF  mplan_longtext,
          vg_type      TYPE mplan_object_type,
          vg_plnal     TYPE plko-plnal,
          wa_return    TYPE bapiret2.

    r_if_pm_data_equipament = me.

    CHECK me->zif_pm_data_equipament~at_dados_param_plano IS NOT INITIAL AND me->zif_pm_data_equipament~at_ponto_rec_transf IS NOT INITIAL.

    FREE: me->zif_pm_data_equipament~at_plano.

    LOOP AT me->zif_pm_data_equipament~at_dados_param_plano ASSIGNING FIELD-SYMBOL(<ls_param_plano>).

      me->zif_pm_data_equipament~get_dados_param_plano(
        EXPORTING
          i_locas    = <ls_param_plano>-locas    " Conjunto para a localização do ponto de medição
        IMPORTING
          t_zpmt0075 = DATA(t_zpmt0075)   " Cadastro de parametros para criar planos de manut.equipament
      ).

      IF t_zpmt0075 IS INITIAL.
        CONTINUE.
      ENDIF.

*      Check o tipo ponto de medição.

      LOOP AT t_zpmt0075 ASSIGNING FIELD-SYMBOL(<wa_zpmt0075>).


        "===============================================================================================
        "Preenchendo cabecalho do plano.
        ws_header = VALUE #(
                            mandt    = sy-mandt
*                          mpla_upd = abap_true
                            ersdt = sy-datum
                            ernam = sy-uname
                            aedat = sy-datum
                            wptxt = <wa_zpmt0075>-wptxt
                            abrho = <wa_zpmt0075>-abrho
                            tplnr = <ls_param_plano>-tplnr
                            equnr = <ls_param_plano>-equnr
                            vspos = <wa_zpmt0075>-vspos
                            vsneg = <wa_zpmt0075>-vsneg
                            topos = <wa_zpmt0075>-topos
                            toneg = <wa_zpmt0075>-toneg
                            sfakt = <wa_zpmt0075>-sfakt
                            horiz = <wa_zpmt0075>-horiz
                            mptyp = <wa_zpmt0075>-mptyp
                            stich = SWITCH #( <wa_zpmt0075>-zeieh WHEN 'H' THEN '3' WHEN 'KM' THEN '3' ELSE '1' )
                         call_confirm = <wa_zpmt0075>-call_confirm
*                          szaeh = me->zif_pm_data_equipament~at_pos_contador
*                          stadt = ''  "data de inicio
        ).

        "==============================================================================================
        "Itens do plano manutenção.

        IF <ls_param_plano>-tplnr IS NOT INITIAL.
          SELECT SINGLE *
          FROM iloa
          INTO @DATA(_iloa)
            WHERE tplnr EQ @<ls_param_plano>-tplnr.
        ENDIF.

        IF  _iloa IS NOT INITIAL.
          DATA(_iloan) = _iloa-iloan.
        ENDIF.

        FREE: it_items.

        "Seleção centro de trabalho.
        SELECT SINGLE * FROM crhd INTO @DATA(_crhb) WHERE arbpl EQ @<wa_zpmt0075>-gewerk AND werks EQ @<ls_param_plano>-werks.

        "Pegar o numerador do grupo.
        CLEAR: vg_plnal.
        SELECT SINGLE plnal FROM plko INTO vg_plnal
        WHERE iwerk EQ <ls_param_plano>-werks
        AND plnnr EQ <wa_zpmt0075>-plnnr
        AND loekz NE abap_true.
        IF sy-subrc NE 0.
          me->zif_pm_data_equipament~grava_logs(
            EXPORTING
              i_tipo_msg = 'E'    " Ctg.mens.: S sucesso, E erro, W aviso, I inform., A cancel.
              i_mensagem = 'Plano manutenção-> ID da lista de tarefas não existe para falial' && <ls_param_plano>-werks " Texto de mensagem
          ).
          CONTINUE.
        ENDIF.

        it_items = VALUE #( (
                            mandt = sy-mandt
*                         action = '1'
                           iloan = _iloan
                           equnr = <ls_param_plano>-equnr
                           bautl = <wa_zpmt0075>-locas
                           aedat = sy-datum
                           aenam = sy-uname
                           iwerk = <ls_param_plano>-werks
                           gewrk = _crhb-objid
                           gsber = <ls_param_plano>-werks
                           auart = <wa_zpmt0075>-auart
                           ilart = <wa_zpmt0075>-ilart
                           wpgrp = <wa_zpmt0075>-wpgrp
                           priok = <wa_zpmt0075>-priok
                           plnty = <wa_zpmt0075>-plnty
                           plnnr = <wa_zpmt0075>-plnnr
                           plnal = vg_plnal
                           mityp = <wa_zpmt0075>-mptyp
                           ) ).


        "=========================================================================================================
        "Ciclos de manutenção.
        FREE: it_cycles.
        it_cycles = VALUE #( (
                          mandt       = sy-mandt
*                        action      = '1'
*                        warpl      = space
*                        nummer     = 01
*                        operator   = space
*                        ktextyzk   = e_zpmt0075-wptxt
                          zykl1      = <wa_zpmt0075>-zykl1
                          zeieh      = <wa_zpmt0075>-zeieh
                          pak_text   = <wa_zpmt0075>-pak_text
*                        langu      = sy-langu
                          point      = |{ me->zif_pm_data_equipament~at_ponto_rec_transf ALPHA = IN }|
*                        offset     = space
*                        inaktiv    = space
*                        nzaeh      = space
*                        zaehl      = space
                           ) ).


        "Tipo de objeto.
        CLEAR: vg_type.
*      vg_type = '3'.
*      it_switch = VALUE #( (
*                        objecttype = vg_type
*                         value_old = ''
*                         value_new = '' ) ).



        "======================================================================================
        "Execut BAPI.
        FREE:it_return.
        CALL FUNCTION 'MPLAN_CREATE'
          EXPORTING
            header       = ws_header
          IMPORTING
            number       = <ls_param_plano>-warpl
          TABLES
            numberswitch = it_switch
            items        = it_items
            cycles       = it_cycles
            longtexts    = it_longtexts
            return       = it_return.

        CLEAR: wa_return.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          IMPORTING
            return = wa_return.
        WAIT UP TO 03 SECONDS.

        READ TABLE it_return INTO DATA(ws_return) WITH KEY type = 'S'.
        IF sy-subrc EQ 0.
          APPEND VALUE #( warpl = <ls_param_plano>-warpl
                          equnr = <ls_param_plano>-equnr )
                          TO me->zif_pm_data_equipament~at_plano.

        ENDIF.
        CLEAR: _iloan, _iloa.
      ENDLOOP.
    ENDLOOP.

    SORT me->zif_pm_data_equipament~at_dados_param_plano BY warpl.
    DELETE me->zif_pm_data_equipament~at_dados_param_plano WHERE warpl IS INITIAL.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~criar_ponto_medicao.
    DATA: measurement_point     TYPE  imrc_point,
          complete_point        TYPE  imptt,
          transfer_take_up_docs TYPE  imrg_tab,
          max_severity          TYPE  symsgty,
          it_return             TYPE  bapiret2_t,
          vg_type               TYPE iref-actyp,
          vg_panel              TYPE t185-panel.

    DATA gc_x    TYPE char1 VALUE 'X'.

    DATA: measurement_point_type TYPE  imrc_mptyp,
          point_will_be_counter  TYPE  imrc_indct,
          ws_itob                TYPE itob,
          vg_objnr               TYPE  impt-mpobj,
          ls_rimr03              TYPE  rimr03,
          linear_data            TYPE  eaml_s_lfe_data_api,
          ls_impt                TYPE impt,
          ind_update             TYPE iref-iind.


    r_if_pm_data_equipament = me.

    CHECK me->zif_pm_data_equipament~at_zpmt0074 IS NOT INITIAL.

    LOOP AT me->zif_pm_data_equipament~at_zpmt0074 ASSIGNING FIELD-SYMBOL(<ls_zpmt0074>).

      CALL FUNCTION 'ITOB_EQUIPMENT_READ_SINGLE'
        EXPORTING
*         I_HANDLE       =
*         I_AUTH_TCODE   =
*         I_LOCK         =
          i_objnr        = i_equnr
        IMPORTING
          e_object_rec   = ws_itob
        EXCEPTIONS
          not_successful = 1
          OTHERS         = 2.

      CLEAR ls_rimr03.
      MOVE:

           <ls_zpmt0074>-locas     TO ls_rimr03-locas,
           <ls_zpmt0074>-begru     TO ls_rimr03-begru,
           <ls_zpmt0074>-atnam     TO ls_rimr03-atnam,
           <ls_zpmt0074>-codgr     TO ls_rimr03-codgr,
           <ls_zpmt0074>-indtr     TO ls_rimr03-indtr,
           <ls_zpmt0074>-casas_decimais     TO ls_rimr03-decim,
           <ls_zpmt0074>-pttxt     TO ls_rimr03-pttxt,
           <ls_zpmt0074>-psort     TO ls_rimr03-psort.

      MOVE-CORRESPONDING ls_rimr03 TO ls_impt.

      measurement_point_type = <ls_zpmt0074>-mptyp.
      point_will_be_counter  = <ls_zpmt0074>-indct.
      vg_type  = '3'.
      gc_x = 'X'.
      ind_update = ''.
      vg_panel = '1'.



      CALL FUNCTION 'MEASUREM_POINT_DIALOG_SINGLE'
        EXPORTING
          activity_type            = vg_type
          f_next_prev_active       = ' '
          indicator_initialize     = ' '
          measurement_point        = ' '
          measurement_point_type   = measurement_point_type
          point_will_be_counter    = point_will_be_counter
          measurement_point_object = ws_itob-objnr
          superior_object          = ' '
          object_key_extern        = ' '
          object_text              = ' '
          reference_or_copy_source = ' '
          panel                    = vg_panel
          indicator_irfmp          = ' '
          no_dialog                = gc_x
          is_rimr03                = ls_rimr03
          linear_data              = linear_data
        IMPORTING
          impt_wa                  = ls_impt
          indicator_update         = ind_update
        EXCEPTIONS
          imptt_not_found          = 1
          type_not_found           = 2
          object_not_found         = 3
          no_authority             = 4
          point_is_refmp           = 5
          point_is_not_refmp       = 6
          error_message            = 98
          OTHERS                   = 99.

      CASE sy-subrc.
        WHEN 0.

          CALL FUNCTION 'MEASUREM_DIALOG_UPDATE'
            EXPORTING
              messages_allowed = space
            EXCEPTIONS
              error_message    = 98
              OTHERS           = 99.

          CASE sy-subrc.
            WHEN 0.
*       OK
            WHEN 98.       "error msg should be filled

            WHEN 99.

          ENDCASE.
        WHEN 99.

        WHEN OTHERS.       "error msg should be filled

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~exec_transf_contador.

    TYPES: BEGIN OF y_msg,
             msg TYPE t100-text.
    TYPES END OF y_msg.


    DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
          t_messtab  TYPE TABLE OF          bdcmsgcoll,
          wa_bdcdata LIKE LINE OF           it_bdcdata.

    DATA: p_resp, check, p_erro(1).

    DATA: measurement_point     TYPE  imrc_point,
          complete_point        TYPE  imptt,
          transfer_take_up_docs TYPE  imrg_tab,
          max_severity          TYPE  symsgty,
          it_return             TYPE  bapiret2_t,
          zfval                 TYPE bdc_fval,
          it_msg                TYPE TABLE OF bdcmsgcoll,
          vg_data               TYPE char10,
          vg_hora               TYPE char08,
          tl_msg                TYPE TABLE OF y_msg.

    r_if_pm_data_equipament = me.


    CHECK me->zif_pm_data_equipament~at_ponto_rec_transf IS NOT INITIAL AND me->zif_pm_data_equipament~at_ponto_ref_transf IS NOT INITIAL.

    vg_data  = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
    vg_hora  = |{ sy-uzeit(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }|.

    FREE: it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = 'T'     fnam = 'IK02       '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '1110'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'IMPT-POINT             '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '/00                    '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPT-POINT '        fval = me->zif_pm_data_equipament~at_ponto_rec_transf        ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_SUBSCR '        fval = 'SAPLIMR4               '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '5110'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'IMPT-INDTR             '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '=ADPT                  '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPT-INDCT '        fval = 'X                      '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPT-INDTR '        fval = 'X                      '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '6110'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'RIMR0-MRMAC            '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '=TRCR                  '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPT-INDTR '        fval = 'X                      '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '6320'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'IMPH-DATLO             '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '=NEXT                  '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPH-TRANS '        fval = me->zif_pm_data_equipament~at_ponto_ref_transf        ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPH-DATLO '        fval = vg_data                                               ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPH-TIMLO '        fval = vg_hora                                               ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPH-DATHI '        fval = '31.12.9999             '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPH-TIMHI '        fval = '23:59:59               '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '6110'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'RIMR0-MRMAC            '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '=NEXT                  '                             ) TO it_bdcdata.
    APPEND VALUE #( program = 'SAPLIMR0'           dynpro = '5110'       dynbegin = 'X'     fnam = '           '        fval = '                       '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_CURSOR '        fval = 'IMPT-PSORT             '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'BDC_OKCODE '        fval = '=BU                    '                             ) TO it_bdcdata.
    APPEND VALUE #( program = '        '           dynpro = '    '       dynbegin = ' '     fnam = 'IMPT-INDCT '        fval = 'X                      '                             ) TO it_bdcdata.

    REFRESH it_msg.

    CALL TRANSACTION 'IK02' USING it_bdcdata
    MODE 'E'
    MESSAGES INTO it_msg
    UPDATE 'S'.


    IF line_exists( it_msg[ msgtyp = 'A' ] ).
      p_erro = abap_true.
    ELSE.
      IF line_exists( it_msg[ msgtyp = 'E' ] ).
        p_erro = abap_true.
      ENDIF.
    ENDIF.

    IF p_erro IS INITIAL.
      READ TABLE it_msg INTO DATA(_w_msg) INDEX 1.
      IF _w_msg IS NOT INITIAL.

        CLEAR: me->zif_pm_data_equipament~at_doc_medicao.

        "Criar documento medição.
        me->zif_pm_data_equipament~criar_doc_para_ponto_medicao(
          EXPORTING
            i_point                 =  |{ me->zif_pm_data_equipament~at_ponto_ref_transf ALPHA = IN }|   " Ponto medição
            i_idate                 =  sy-datum   " Data da medição
            i_itime                 =  sy-uzeit    " Hora da medição
            i_pos_contador          =  me->zif_pm_data_equipament~at_pos_contador    " Posição do contador
            ).

        "Criar documento medição.
        me->zif_pm_data_equipament~criar_doc_para_ponto_medicao(
          EXPORTING
            i_point                 =  |{ me->zif_pm_data_equipament~at_ponto_rec_transf ALPHA = IN }|   " Ponto medição
            i_idate                 =  sy-datum   " Data da medição
            i_itime                 =  sy-uzeit    " Hora da medição
            i_pos_contador          =  me->zif_pm_data_equipament~at_pos_contador    " Posição do contador
            ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~get_dados_eqpto.
    DATA: wa_return             TYPE bapiret2.

    r_if_pm_data_equipament = me.

    IF i_equnr IS NOT INITIAL.

      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
        EXPORTING
          equipment        = i_equnr
        IMPORTING
          data_general_exp = me->zif_pm_data_equipament~at_dados_eqpto
          return           = wa_return.

    ENDIF.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~get_dados_param_plano.

    r_if_pm_data_equipament = me.
    FREE: t_zpmt0075.
    IF i_locas IS NOT INITIAL AND me->zif_pm_data_equipament~at_tipo_contador IS NOT INITIAL.
      SELECT * FROM zpmt0075 INTO TABLE t_zpmt0075 WHERE locas EQ i_locas
        AND zeieh EQ  me->zif_pm_data_equipament~at_tipo_contador
        AND typbz EQ me->zif_pm_data_equipament~at_tipo_veiculo.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~get_instance.


    IF zif_pm_data_equipament~at_if_pm_data_equipament IS NOT BOUND.
      CREATE OBJECT zif_pm_data_equipament~at_if_pm_data_equipament
        TYPE zcl_pm_data_equipament.


*        CATCH zcx_integracao.    " .
    ENDIF.

    r_if_pm_data_equipament = zif_pm_data_equipament~at_if_pm_data_equipament.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~grava_logs.
    DATA: it_zpmr0005 TYPE TABLE OF zpmr0005.

    r_if_pm_data_equipament = me.

    IF i_mensagem IS NOT INITIAL AND i_tipo_msg IS NOT INITIAL.
      it_zpmr0005 = VALUE #( ( usuario = sy-uname
                            programa  = sy-tcode
                                data = sy-datum
                                hora = sy-uzeit
                            tipo_msg = i_tipo_msg
                            mensagem = i_mensagem
      ) ).

      IF  it_zpmr0005 IS NOT INITIAL.
        MODIFY  zpmr0005 FROM TABLE  it_zpmr0005.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~modificar_equipamento.
    DATA: wa_data_general     TYPE bapi_itob,
          wa_data_generalx    TYPE bapi_itobx,
          wa_data_specific    TYPE bapi_itob_eq_only,
          wa_data_specificx   TYPE bapi_itob_eq_onlyx,
          wa_return_bapi_eqmt TYPE bapireturn,
          wa_return           TYPE bapiret2.

    CLEAR: wa_data_general, wa_data_generalx, wa_data_specific,
           wa_data_specificx, wa_return.
    DATA: lv_empresa TYPE bukrs.

    r_if_pm_data_equipament = me.

    "Buscar empresa

    SELECT SINGLE bukrs
        INTO lv_empresa
        FROM j_1bbranch
        WHERE branch EQ planplant.


    wa_data_general-maintplant = maintplant.
    wa_data_general-bus_area   = bus_area  .
    wa_data_general-costcenter = costcenter.
    wa_data_general-comp_code  = lv_empresa.
    wa_data_general-planplant  = planplant.
    wa_data_general-work_ctr   = work_ctr.
    wa_data_general-standorder = standorder.
    wa_data_general-settlorder = settlorder.

    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-maintplant = 'X'.
    wa_data_generalx-bus_area   = 'X'.
    wa_data_generalx-costcenter = 'X'.
    wa_data_generalx-planplant  = 'X'.
    wa_data_generalx-work_ctr   = 'X'.

    IF wa_data_general-standorder IS NOT INITIAL.
      wa_data_generalx-standorder = 'X'.
    ENDIF.

    IF wa_data_general-settlorder IS NOT INITIAL.
      wa_data_generalx-settlorder = 'X'.
    ENDIF.

    CALL FUNCTION 'BAPI_EQUI_CHANGE'
      EXPORTING
        equipment      = equipment
        data_general   = wa_data_general
        data_generalx  = wa_data_generalx
        data_specific  = wa_data_specific
        data_specificx = wa_data_specificx
        valid_date     = sy-datum
        valid_time     = sy-uzeit
      IMPORTING
        return         = wa_return.
*        RETURN         = IT_RETURN.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    WAIT UP TO 04 SECONDS.


  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_dados_zpmt0074.

    r_if_pm_data_equipament = me.

    "Seleciona dados parametrizado para tipo equipamento, categoria, classe, modelo, fabricante.
    SELECT * FROM zpmt0074
      INTO TABLE me->zif_pm_data_equipament~at_zpmt0074
      WHERE fleet_cat EQ i_fleet_cat
        AND herst     EQ i_herst
        AND klasse    EQ i_klasse
        AND typbz     EQ i_typbz
        AND eqtyp     EQ i_eqtyp.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_data_eqpto.
    DATA: message TYPE itex132,
          zid     TYPE p,
          ws_equi TYPE equi.

    r_if_pm_data_equipament = me.

    CLEAR: ws_equi.
    CLEAR: zid. "Se não encontrar o equipamento, tentar 10 vezes.
    WHILE zid <= 10.
      CLEAR: ws_equi.
      SELECT SINGLE * FROM equi INTO ws_equi WHERE equnr EQ i_equi-equnr.
      IF sy-subrc NE 0.
        ADD 1 TO zid.
        WAIT UP TO 05 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.


    CHECK ws_equi IS NOT INITIAL.

    "Verifica se o equipamento possuem equipamento superior, caso não
*possua, segue para criar as ordens e atribuir ao equipamento,
*senão, localiza as ordens do equipamento superior e atribua
*a equipamento inferior cadastrado.

    "FF #188919 - inicio

    DATA lv_tp_veiculo TYPE zval.

    lv_tp_veiculo = i_equi-eqart.
    CONDENSE lv_tp_veiculo NO-GAPS.

    SELECT *
      FROM ztparam
      INTO @DATA(ls_dummy)
      UP TO 1 ROWS
      WHERE param = 'TP_IMPLEM'
        AND zval  = @lv_tp_veiculo.
    ENDSELECT.


    IF sy-subrc = 0. "Se achar dados no select acima, não criar ordem ZPM6, pois é um veículo sem tanque.

      DATA(lv_eqpto_sem_tanque) = abap_true.

    ELSE.

      lv_eqpto_sem_tanque = abap_false.

    ENDIF.

    "FF #188919 - fim

    CLEAR message.
    message = | Aguarde, criando ordens de manutenção|.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = message.

    IF i_equz-hequi IS INITIAL.

      me->zif_pm_data_equipament~check_categoria( i_eqtyp = i_equi-eqtyp i_eqart = i_equi-eqart ).

      IF me->zif_pm_data_equipament~at_check_categoria EQ abap_true.

        me->zif_pm_data_equipament~set_work_ctr( i_work_ctr = i_equz-gewrk i_werks = i_iloa-swerk
        )->criar_ordem_manutencao(
       order_type   = 'ZPM5'
       short_text   = 'ORDEM DE REMONTA'
       planplant    = i_iloa-swerk
       funct_loc    = i_iloa-tplnr
       bus_area     = i_iloa-swerk
       mn_wk_ctr    = me->zif_pm_data_equipament~at_work_ctr
       plant        = i_iloa-swerk
       maintplant   = i_iloa-swerk
       loc_bus_area = i_iloa-swerk
       plangroup    = 'ABS'
       equipment    = i_equi-equnr
       costcenter   = i_iloa-kostl
       pmacttype    = 'Z03'
       priority     = '4'
       activity     = '0010'
       control_key  = 'PM01'
       description  = 'ORDEM DE REMONTA'
       text_longo   = ''
       )->set_ordem_manutencao( IMPORTING e_aufnr = me->zif_pm_data_equipament~at_numero_ordem_lub
       ).



        IF me->zif_pm_data_equipament~at_numero_ordem IS INITIAL.
          me->zif_pm_data_equipament~set_work_ctr( i_work_ctr = i_equz-gewrk i_werks = i_iloa-swerk
         )->criar_ordem_manutencao(
        order_type   = 'ZPM5'
        short_text   = 'ORDEM DE REMONTA'
        planplant    = i_iloa-swerk
        funct_loc    = i_iloa-tplnr
        bus_area     = i_iloa-swerk
        mn_wk_ctr    = me->zif_pm_data_equipament~at_work_ctr
        plant        = i_iloa-swerk
        maintplant   = i_iloa-swerk
        loc_bus_area = i_iloa-swerk
        plangroup    = 'ABS'
        equipment    = i_equi-equnr
        costcenter   = i_iloa-kostl
        pmacttype    = 'Z03'
        priority     = '4'
        activity     = '0010'
        control_key  = 'PM01'
        description  = 'ORDEM DE REMONTA'
        text_longo   = ''
        )->set_ordem_manutencao( IMPORTING e_aufnr = me->zif_pm_data_equipament~at_numero_ordem_lub
        ).
        ENDIF.

        IF me->zif_pm_data_equipament~at_check_impl EQ abap_false AND lv_eqpto_sem_tanque = abap_false.

          me->zif_pm_data_equipament~set_work_ctr( i_work_ctr = i_equz-gewrk i_werks = i_iloa-swerk
             )->criar_ordem_manutencao(
             order_type   = 'ZPM6'
             short_text   = 'ABASTECIMENTO DE COMBUSTIVEL'
             planplant    = i_iloa-swerk
             funct_loc    = i_iloa-tplnr
             bus_area     = i_iloa-swerk
             mn_wk_ctr    = me->zif_pm_data_equipament~at_work_ctr
             plant        = i_iloa-swerk
             maintplant   = i_iloa-swerk
             loc_bus_area = i_iloa-swerk
             plangroup    = 'ABS'
             equipment    = i_equi-equnr
             costcenter   = i_iloa-kostl
             pmacttype    = 'Z11'
             priority     = '4'
             activity     = '0010'
             control_key  = 'PM01'
             description  = 'ABASTECIMENTO DE COMBUSTIVEL'
             text_longo   = ''
              )->set_ordem_manutencao( IMPORTING e_aufnr = me->zif_pm_data_equipament~at_numero_ordem_comb
              )->modificar_equipamento(
            EXPORTING
              equipment               =  i_equi-equnr   " Nº equipamento
              maintplant              =  i_iloa-swerk   " Centro de manutenção
              bus_area                =  i_iloa-swerk   " Divisão
              planplant               =  i_iloa-swerk   " Centro de planejamento de manutenção
              costcenter              =  i_iloa-kostl   " Centro de custo
              work_ctr                =  i_equz-gewrk   " Valor numérico de 8 dígitos
              standorder              =  me->zif_pm_data_equipament~at_numero_ordem_comb   " Nº ordem permanente
              settlorder              =  me->zif_pm_data_equipament~at_numero_ordem_lub   " Ordem p/apropriação de custos
             ).
        ELSE.

          me->zif_pm_data_equipament~modificar_equipamento(
            EXPORTING
              equipment               =  i_equi-equnr   " Nº equipamento
              maintplant              =  i_iloa-swerk   " Centro de manutenção
              bus_area                =  i_iloa-swerk   " Divisão
              planplant               =  i_iloa-swerk   " Centro de planejamento de manutenção
              costcenter              =  i_iloa-kostl   " Centro de custo
              work_ctr                =  i_equz-gewrk   " Valor numérico de 8 dígitos
              standorder              =  ''  " Nº ordem permanente
              settlorder              =  me->zif_pm_data_equipament~at_numero_ordem_lub   " Ordem p/apropriação de custos
             ).
        ENDIF.
      ENDIF.

    ELSE.

      me->zif_pm_data_equipament~get_dados_eqpto( i_equnr = i_equz-hequi
      )->modificar_equipamento(
      EXPORTING
        equipment               =  i_equi-equnr   " Nº equipamento
        maintplant              =  i_iloa-swerk   " Centro de manutenção
        bus_area                =  i_iloa-swerk   " Divisão
        planplant               =  i_iloa-swerk   " Centro de planejamento de manutenção
        costcenter              =  i_iloa-kostl   " Centro de custo
        work_ctr                =  i_equz-gewrk   " Valor numérico de 8 dígitos
        standorder              =  me->zif_pm_data_equipament~at_dados_eqpto-standorder   " Nº ordem permanente
        settlorder              =  me->zif_pm_data_equipament~at_dados_eqpto-settlorder   " Ordem p/apropriação de custos
      ).
    ENDIF.


    CLEAR message.
    message = | Aguarde, criando pontos medição|.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = message.

    "Criar pontos de medição.
    me->zif_pm_data_equipament~set_dados_zpmt0074(
      EXPORTING
        i_klasse                = i_itobattr-klasse  " Nº classe
        i_fleet_cat             = i_fleet-fleet_cat  " Tipo de veículo
        i_eqtyp                 = i_equi-eqtyp       " Categoria de equipamento
        i_herst                 = i_equi-herst       " Fabricante do imobilizado
        i_typbz                 = i_equi-typbz       " Denominação do tipo atribuído pelo fabricante
*        )->CRIAR_PONTO_MEDICAO( i_equnr =  i_equi-equnr ).
    )->shdb_ik01( i_equnr =  i_equi-equnr i_tplnr = i_iloa-tplnr i_werks = i_iloa-swerk ).
    me->zif_pm_data_equipament~at_tipo_veiculo = i_equi-typbz.

    "Criar planos de manutenção.
    me->zif_pm_data_equipament~criar_planos_manut(
    )->start_plan_manutencao( ).

    DATA(s_equnr) = |{ i_equi-equnr ALPHA = OUT }|.
    MESSAGE s024(sd) WITH |O equipamento foi criado sob o nº: { s_equnr }|.


  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_desc_eqpto.
    DATA : zktx01 TYPE c LENGTH 50.
    r_if_pm_data_equipament = me.

    CLEAR: e_ktx01, zktx01.

    zktx01 = |{ me->zif_pm_data_equipament~at_eartx } { me->zif_pm_data_equipament~at_desc_fabricante } { me->zif_pm_data_equipament~at_desc_modelo }|.

    IF  zktx01 IS NOT INITIAL.
      TRANSLATE zktx01 TO UPPER CASE.
      e_ktx01 = zktx01.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_desc_fabricante.

    r_if_pm_data_equipament = me.

    IF i_herst IS NOT INITIAL.
      SELECT SINGLE zdesc FROM zpmt0069 INTO ( me->zif_pm_data_equipament~at_desc_fabricante )
        WHERE herst EQ i_herst.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_desc_modelo.

    r_if_pm_data_equipament = me.

    IF i_typbz IS NOT INITIAL.
      SELECT SINGLE typbz FROM zpmt0070 INTO ( me->zif_pm_data_equipament~at_desc_modelo )
        WHERE typbz EQ i_typbz.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_eartx.
    DATA: ws_t370k_t TYPE t370k_t.

    r_if_pm_data_equipament = me.

    IF i_eqart IS NOT INITIAL.
      SELECT SINGLE eartx FROM t370k_t INTO ( me->zif_pm_data_equipament~at_eartx )
        WHERE eqart EQ i_eqart.
    ENDIF.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_ordem_manutencao.

    r_if_pm_data_equipament = me.

    e_aufnr = me->zif_pm_data_equipament~at_numero_ordem.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_parametros_plano.

    r_if_pm_data_equipament = me.

    IF i_equnr IS NOT INITIAL AND i_locas IS NOT INITIAL.
      APPEND VALUE #( equnr = i_equnr locas = i_locas tplnr = i_tplnr werks = i_werks ) TO  me->zif_pm_data_equipament~at_dados_param_plano.
    ENDIF.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_pos_contador.

    r_if_pm_data_equipament = me.
    IF i_pos_contador IS NOT INITIAL.

      REPLACE '.' IN i_pos_contador WITH ','.

      me->zif_pm_data_equipament~at_pos_contador = i_pos_contador.

    ELSE.
      me->zif_pm_data_equipament~at_pos_contador = '1,00'.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~set_work_ctr.

    r_if_pm_data_equipament = me.

    SELECT SINGLE *
    FROM crhd
    INTO @DATA(_crhd)
    WHERE objty EQ 'A'
      AND objid EQ @i_work_ctr
      AND werks EQ @i_werks.


    IF _crhd IS NOT INITIAL.
      me->zif_pm_data_equipament~at_work_ctr = _crhd-arbpl.
    ENDIF.

  ENDMETHOD.


  METHOD zif_pm_data_equipament~shdb_ik01.
    TYPES: BEGIN OF y_msg,
             msg TYPE t100-text.
    TYPES END OF y_msg.


    DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
          t_messtab  TYPE TABLE OF          bdcmsgcoll,
          wa_bdcdata LIKE LINE OF           it_bdcdata.

    DATA: p_resp, check, p_erro(1).

    DATA: measurement_point     TYPE  imrc_point,
          complete_point        TYPE  imptt,
          transfer_take_up_docs TYPE  imrg_tab,
          max_severity          TYPE  symsgty,
          it_return             TYPE  bapiret2_t,
          zfval                 TYPE bdc_fval,
          it_msg                TYPE TABLE OF bdcmsgcoll,
          tl_msg                TYPE TABLE OF y_msg.


    r_if_pm_data_equipament = me.

    CHECK me->zif_pm_data_equipament~at_zpmt0074 IS NOT INITIAL.

    LOOP AT me->zif_pm_data_equipament~at_zpmt0074 ASSIGNING FIELD-SYMBOL(<ls_zpmt0074>).

      CLEAR: zfval.
      zfval = <ls_zpmt0074>-casas_decimais.
      CONDENSE zfval NO-GAPS.

      FREE: it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = 'T'     fnam = 'IK01'         fval = ''                             ) TO it_bdcdata.
      APPEND VALUE #( program = 'SAPLIMR0'   dynpro = '1110'   dynbegin = 'X'     fnam = ''             fval = ''                             ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_CURSOR'   fval = 'IMPT-INDCT'                   ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_OKCODE'   fval = '/00'                          ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'RIMR0-MPOTY'  fval = 'IEQ'                          ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-MPTYP'   fval = <ls_zpmt0074>-mptyp            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-INDCT'   fval = <ls_zpmt0074>-indct            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_SUBSCR'   fval = 'SAPLIMR4  7502MPOBJ'          ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'EQUI-EQUNR'   fval = i_equnr                        ) TO it_bdcdata.
      APPEND VALUE #( program = 'SAPLIMR0'   dynpro = '5110'   dynbegin = 'X'     fnam = ''             fval = ''                             ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_CURSOR '  fval = 'RIMR0-PYEAC      '            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_OKCODE '  fval = '/00              '            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-PSORT '  fval = <ls_zpmt0074>-psort            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-PTTXT '  fval = <ls_zpmt0074>-pttxt            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-ATNAM '  fval = <ls_zpmt0074>-atnam            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-INDCT '  fval = <ls_zpmt0074>-indct            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-DECIM '  fval = zfval                          ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-EXPON '  fval = '0                '            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-CODGR '  fval = <ls_zpmt0074>-codgr            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-LOCAS '  fval = <ls_zpmt0074>-locas            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-BEGRU '  fval = <ls_zpmt0074>-begru            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-INDTR '  fval = <ls_zpmt0074>-indtr            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'RIMR0-CJUMC'  fval = <ls_zpmt0074>-cjumc            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'RIMR0-PYEAC'  fval = <ls_zpmt0074>-pyeac            ) TO it_bdcdata.
      APPEND VALUE #( program = 'SAPLIMR0'   dynpro = '5110'   dynbegin = 'X'     fnam = ''             fval = ''                             ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_CURSOR '  fval = 'IMPT-PSORT        '           ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'BDC_OKCODE '  fval = '=BU               '           ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-PSORT '  fval = <ls_zpmt0074>-psort            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-PTTXT '  fval = <ls_zpmt0074>-pttxt            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-ATNAM '  fval = <ls_zpmt0074>-atnam            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-INDCT '  fval = <ls_zpmt0074>-indct            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-DECIM '  fval = zfval                          ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-CODGR '  fval = <ls_zpmt0074>-codgr            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-LOCAS '  fval = <ls_zpmt0074>-locas            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-BEGRU '  fval = <ls_zpmt0074>-begru            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'IMPT-INDTR '  fval = <ls_zpmt0074>-indtr            ) TO it_bdcdata.
      APPEND VALUE #( program = ''           dynpro = ''       dynbegin = ''      fnam = 'RIMR0-CJUMC'  fval = <ls_zpmt0074>-cjumc            ) TO it_bdcdata.


      REFRESH it_msg.

      CALL TRANSACTION 'IK01' USING it_bdcdata
      MODE 'E'
      MESSAGES INTO it_msg
      UPDATE 'S'.


      IF line_exists( it_msg[ msgtyp = 'A' ] ).
        p_erro = abap_true.
      ELSE.
        IF line_exists( it_msg[ msgtyp = 'E' ] ).
          p_erro = abap_true.
        ENDIF.
      ENDIF.

      IF p_erro IS INITIAL.
        READ TABLE it_msg INTO DATA(_w_msg) INDEX 1.
        IF _w_msg IS NOT INITIAL.
          IF <ls_zpmt0074>-indtr_ref IS NOT INITIAL.
            me->zif_pm_data_equipament~at_ponto_ref_transf = _w_msg-msgv1.

            IF <ls_zpmt0074>-atnam EQ 'HORIMETRO' OR <ls_zpmt0074>-atnam EQ 'HORÍMETRO'.
              me->zif_pm_data_equipament~at_tipo_contador = 'H'.
            ELSE.
              IF <ls_zpmt0074>-atnam EQ 'ODOMETRO'.
                me->zif_pm_data_equipament~at_tipo_contador = 'KM'.
              ENDIF.
            ENDIF.
          ENDIF.

          IF <ls_zpmt0074>-indtr IS NOT INITIAL.
            me->zif_pm_data_equipament~at_ponto_rec_transf = _w_msg-msgv1.
          ENDIF.
        ENDIF.

        IF <ls_zpmt0074>-zcria IS NOT INITIAL.
          me->zif_pm_data_equipament~set_parametros_plano(
            EXPORTING
              i_locas                 = <ls_zpmt0074>-locas     " Conjunto para a localização do ponto de medição
              i_equnr                 = i_equnr    " Nº equipamento
              i_werks                 = i_werks
              i_tplnr                 = i_tplnr
          ).
        ENDIF.
      ENDIF.
    ENDLOOP.


    "Executa transferencia.
    IF me->zif_pm_data_equipament~at_ponto_rec_transf IS NOT INITIAL.
      me->zif_pm_data_equipament~exec_transf_contador( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~start_plan_manutencao.
    DATA: it_return   TYPE bapirettab,
          vg_szaeh    TYPE char22,
          it_zpmr0005 TYPE TABLE OF zpmr0005,
          vg_contador TYPE p DECIMALS 0.

    r_if_pm_data_equipament = me.

    CHECK me->zif_pm_data_equipament~at_dados_param_plano IS NOT INITIAL.

    LOOP AT me->zif_pm_data_equipament~at_plano ASSIGNING FIELD-SYMBOL(<ls_dados_plano>).

      CLEAR: vg_contador, vg_szaeh.
      REPLACE ',' IN me->zif_pm_data_equipament~at_pos_contador WITH '.'.
      vg_contador = me->zif_pm_data_equipament~at_pos_contador.
      vg_szaeh = vg_contador.
      CONDENSE vg_szaeh NO-GAPS.

      CALL FUNCTION 'MPLAN_START'
        EXPORTING
          warpl  = <ls_dados_plano>-warpl
*         STADT  =
          szaeh  = vg_szaeh
*         STIME  =
*         IV_DAT_FREI       =
        IMPORTING
          return = it_return.

      IF it_return IS NOT INITIAL.
        LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).

        ENDLOOP.
        APPEND VALUE #(
                              usuario = sy-uname
                              programa = |{ <ls_dados_plano>-warpl } IE01 / START PLANO MANUT|
                                  data = sy-datum
                                  hora = sy-uzeit
                              tipo_msg = <ls_return>-type
                              mensagem = |{ <ls_return>-message }{ <ls_return>-message_v1 } |

        ) TO it_zpmr0005.
      ENDIF.
    ENDLOOP.

    IF it_zpmr0005 IS NOT INITIAL.
      MODIFY zpmr0005 FROM TABLE it_zpmr0005.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD zif_pm_data_equipament~valida_capacidade_combustivel.

    DATA: lv_tolerancia          TYPE zpmr0001-tolerancia,
          lv_diferenca_permitida TYPE zpmr0001-tolerancia.


*** Inicio - Rubenilson - 06.08.2025 - BUG186983
    SELECT *
      FROM ztparam
      INTO @DATA(ls_param)
      UP TO 1 ROWS
      WHERE param = 'TP_IMPLEM'
        AND zval  = @i_cod_classe.
    ENDSELECT.
    IF sy-subrc IS NOT INITIAL.
*** Fim - Rubenilson - 06.08.2025 - BUG186983

      SELECT SINGLE tq_comb, tolerancia
        FROM zpmr0001
        WHERE class_oper = @i_cod_classe
          AND herst      = @i_fabricante
          AND typbz      = @i_modelo
        INTO @DATA(ls_0001).

      IF sy-subrc = 0.

        e_tq_comb = ls_0001-tq_comb.

        lv_diferenca_permitida = ls_0001-tq_comb * ls_0001-tolerancia / 100. "Diferença permitida de abastecimento.
        lv_tolerancia = ls_0001-tq_comb + lv_diferenca_permitida.

        IF i_qtd_abastec <= lv_tolerancia.
          e_dentro_da_tolerancia = abap_true. "Sim = X ou Não =  " ".
        ELSE.
          e_dentro_da_tolerancia = abap_false.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
