*&---------------------------------------------------------------------*
*&  Include           ZPMR0047_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

  ENDMETHOD.

  METHOD set_toolbar.

    IF sy-dynnr EQ '0100'.

      CLEAR wa_toolbar.
      wa_toolbar-function     = 'BTN_PLAN'.
      wa_toolbar-icon         =  icon_working_plan.
      wa_toolbar-quickinfo    = 'Atribuir Atividade'.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Atribuir Atividade'.
      APPEND wa_toolbar TO e_object->mt_toolbar.


      wa_toolbar-function     = 'BTN_ABOUT'.
      wa_toolbar-icon         = icon_information.
      wa_toolbar-butn_type    = 0.
      wa_toolbar-text         = 'Sobre'.
      APPEND wa_toolbar TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.

  METHOD get_ucomm.
    DATA: gs_eqp TYPE zpmt0022.

    CASE e_ucomm.

      WHEN 'BTN_PLAN'.

        CALL SCREEN 0200 STARTING AT 5 5.

    ENDCASE.


  ENDMETHOD.

  METHOD on_data_changed_finished.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '0100'
        i_show     = space
        i_repid    = sy-repid
      IMPORTING
        e_messagem = wa_mensagem
      TABLES
        it_msgs    = it_msg_return.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_double_click.


    CHECK e_row-rowtype(1) EQ space.

    PERFORM sel_dados  USING e_row e_column-fieldname.

*    CALL METHOD OBJ_ALV_STATUS->SELEC_TRANSACAO
*      EXPORTING
*        E_ROW    = E_ROW
*        E_COLUMN = E_COLUMN.
*    TRY .
*        DATA(WA_PLANOS) = T_PLANOS[ P_ROW ].
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.


  ENDMETHOD.

ENDCLASS.


CLASS dados_ordem DEFINITION.
  PUBLIC SECTION.

    METHODS: dados_operacao
      IMPORTING
        aufnr    TYPE aufnr
      EXPORTING
        hr_plan  TYPE arbeit
        hr_apont TYPE co_ismnw
        und      TYPE arbeite.


    METHODS: selec_ordem,
      selec_equip.

    METHODS: selec_transacao
      IMPORTING
        e_row    TYPE char05
        e_column TYPE char05.

    METHODS: z_modif_ordem IMPORTING
                                     orderid      TYPE aufnr
                                     order_type   TYPE aufart
                                     start_date   TYPE co_gstrp
*                                     FINISH_DATE  TYPE CO_GLTRP
                                     planplant    TYPE werks_d
                                     bus_area     TYPE werks_d
                                     funct_loc    TYPE tplnr
                                     plant        TYPE werks_d
                                     maintplant   TYPE werks_d
                                     loc_bus_area TYPE werks_d
                                     equipment    TYPE equnr
                                     activity     TYPE vornr
                                     control_key  TYPE steus
                                     priok        TYPE priok
                           EXPORTING gt_return    TYPE bapiret2_t.
ENDCLASS.

CLASS dados_ordem IMPLEMENTATION.

  METHOD selec_transacao.





  ENDMETHOD.


*  Seleção de dados da ordem de manutenção
  METHOD selec_ordem.
    DATA: t_zpmt0022 TYPE TABLE OF zpmt0022.
    FREE: gt_ordem, t_zpmt0022.
    DATA: lib TYPE char10.
    DATA: abe TYPE char10.

    DATA line_range LIKE LINE OF status_range.

    CLEAR: lib, abe.

    IF p_abe IS NOT INITIAL.
      abe = 'I0001'.
    ENDIF.

    IF p_lib IS NOT INITIAL.
      lib = 'I0002'.
    ENDIF.

    FREE gt_ordem.
    status_range = VALUE #( ( sign    = 'I' option  = 'EQ' low   = abe )
                            ( sign    = 'I' option  = 'EQ' low   = lib  ) ).

    SELECT dsequipe,inativo
      FROM zpmt0021
      INTO TABLE @DATA(_zpmt0021)
      WHERE inativo EQ @space.


    IF _zpmt0021 IS NOT INITIAL.
      LOOP AT _zpmt0021 ASSIGNING FIELD-SYMBOL(<w_eqp>).
        APPEND VALUE #( sign    = 'I' option  = 'EQ' low   = <w_eqp>-dsequipe ) TO status_eqp.
      ENDLOOP.
    ENDIF.

*
    SELECT *
    FROM zpmt0022
    INTO TABLE @DATA(_zpmt0022)
      WHERE dsequipe NOT IN @status_eqp.


    IF _zpmt0022 IS NOT INITIAL.
      LOOP AT _zpmt0022 ASSIGNING FIELD-SYMBOL(<lw_eqp>).
        DELETE FROM zpmt0022 WHERE aufnr EQ <lw_eqp>-aufnr.
        COMMIT WORK.
      ENDLOOP.
    ENDIF.

    SELECT *
    FROM viaufkst AS a
    INNER JOIN aufk AS b ON b~aufnr EQ a~aufnr
    INTO CORRESPONDING FIELDS OF TABLE gt_ordem
      WHERE a~werks IN p_werks
        AND a~equnr IN p_equnr
        AND a~aufnr IN p_aufnr
        AND a~auart IN p_auart
        AND a~kostl IN p_kostl
        AND a~erdat IN p_erdat.
*        AND B~PHAS1 EQ 'ABAP_TRUE'.

    SORT gt_ordem BY aufnr.

    IF gt_ordem IS NOT INITIAL.
      SELECT *
      FROM zpmt0022
      INTO TABLE t_zpmt0022
        FOR ALL ENTRIES IN gt_ordem
        WHERE aufnr EQ gt_ordem-aufnr.

      SELECT *
      FROM eqkt
      INTO TABLE @DATA(_eqkt)
        FOR ALL ENTRIES IN @gt_ordem
        WHERE equnr EQ @gt_ordem-equnr.


      SELECT *
      FROM tj02t AS a
      INNER JOIN jest AS b ON b~stat EQ a~istat
      INTO CORRESPONDING FIELDS OF TABLE gt_jest
      FOR ALL ENTRIES IN gt_ordem
      WHERE objnr EQ gt_ordem-objnr
      AND b~inact EQ abap_false
      AND a~spras EQ sy-langu.

      SORT gt_jest ASCENDING BY stat.
      DELETE gt_jest WHERE stat NOT IN status_range.

      LOOP AT gt_ordem ASSIGNING FIELD-SYMBOL(<w_ordem>).

        dados_operacao( EXPORTING aufnr = <w_ordem>-aufnr IMPORTING hr_plan = <w_ordem>-arbei
                                                                   hr_apont = <w_ordem>-ismnw
                                                                   und      = <w_ordem>-arbeh ).

*        Desvio de horas planejadas x Apontadas.
        IF <w_ordem>-arbei IS NOT INITIAL AND <w_ordem>-ismnw IS NOT INITIAL.
          <w_ordem>-desv = ( <w_ordem>-arbei - <w_ordem>-ismnw ).
        ENDIF.


        READ TABLE _eqkt INTO DATA(w_eqkt) WITH KEY equnr =  <w_ordem>-equnr.
        IF sy-subrc EQ 0.
          <w_ordem>-eqktx = w_eqkt-eqktx.
          CLEAR w_eqkt.
        ENDIF.

        READ TABLE gt_jest INTO DATA(_jest) WITH KEY objnr = <w_ordem>-objnr.
        IF sy-subrc EQ 0.
          <w_ordem>-txt04 = _jest-txt04.
          <w_ordem>-marc_status = abap_true.
        ENDIF.


        READ TABLE t_zpmt0022 INTO DATA(wa_zpmt0022) WITH KEY aufnr = <w_ordem>-aufnr BINARY SEARCH.

        IF sy-subrc EQ 0.
          <w_ordem>-dsequipe = wa_zpmt0022-dsequipe.
          IF <w_ordem>-dsequipe NE ' '.
            <w_ordem>-zstatus  = icon_green_light.
          ELSE.
            <w_ordem>-zstatus  = icon_red_light.
          ENDIF.
        ELSE.
          <w_ordem>-zstatus  = icon_red_light.
        ENDIF.
        CLEAR: wa_zpmt0022.
      ENDLOOP.
    ENDIF.

    DELETE gt_ordem WHERE marc_status NE abap_true.

    IF gt_ordem IS NOT INITIAL.
      CALL SCREEN 0100.
    ENDIF.
  ENDMETHOD.

  METHOD selec_equip.
    DATA: gs_eqp TYPE zpmt0022.
    DATA: gw_eqp    TYPE zpmt0021,
          lt_header TYPE TABLE OF caufvdb.

    REFRESH: it_selected_rows.
    FREE: it_msg_return.
    CLEAR: lines, wa_msg_return.

    IF zpmt0021-idequipe IS NOT INITIAL.

      CLEAR: gw_eqp.
      SELECT SINGLE *
      FROM zpmt0021
      INTO gw_eqp
        WHERE idequipe EQ zpmt0021-idequipe.
    ENDIF.

    CALL METHOD obj_alv_0110->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    DESCRIBE TABLE it_selected_rows LINES lines.

    IF ( lines IS INITIAL ).
      MESSAGE TEXT-004 TYPE 'I' DISPLAY LIKE 'E'.

    ELSE.
      LOOP AT it_selected_rows INTO wa_selected_rows.
        READ TABLE gt_ordem INTO DATA(gw_ordem) INDEX wa_selected_rows-index.

        IF sy-subrc EQ 0.

          gs_eqp-idequipe = gw_eqp-idequipe.
          gs_eqp-dsequipe = gw_eqp-dsequipe.
          gs_eqp-aufnr    = gw_ordem-aufnr.
          MODIFY zpmt0022 FROM gs_eqp.
          COMMIT WORK.

          SELECT SINGLE *
          FROM viaufkst
          INTO @DATA(lw_aufk)
          WHERE aufnr = @gw_ordem-aufnr.
          IF sy-subrc IS INITIAL.
            APPEND INITIAL LINE TO lt_header ASSIGNING FIELD-SYMBOL(<fs_header>).
            MOVE-CORRESPONDING lw_aufk TO <fs_header>.
          ENDIF.

          CALL FUNCTION 'ZPM_DISPARA_API_ORDEM'
            EXPORTING
              header    = lt_header
              id_equipe = zpmt0021-idequipe.


*          Modificar ordem -> data programação inicio e fim base.
          IF afko-gstrp IS NOT INITIAL.
            z_modif_ordem( EXPORTING
                           orderid      = gw_ordem-aufnr
                           order_type   = gw_ordem-auart
                           start_date   = afko-gstrp
*                           FINISH_DATE  = AFKO-GLTRP
                           planplant    = gw_ordem-werks
                           bus_area     = gw_ordem-werks
                           funct_loc    = gw_ordem-tplnr
                           plant        = gw_ordem-werks
                           maintplant   = gw_ordem-werks
                           loc_bus_area = gw_ordem-werks
                           equipment    = gw_ordem-equnr
                           priok        = gw_ordem-priok
                           activity     = '0010'
                           control_key  = 'PM01'
                            IMPORTING gt_return = DATA(t_return) ).

            LOOP AT t_return ASSIGNING FIELD-SYMBOL(<_return>).
              wa_msg_return-msg =  <_return>-message.
              APPEND wa_msg_return-msg TO it_msg_return.
            ENDLOOP.
          ENDIF.
          CLEAR: gs_eqp, gw_ordem.
        ENDIF.
      ENDLOOP.
      MESSAGE TEXT-007 TYPE 'I' DISPLAY LIKE 'S'.
      CALL METHOD selec_ordem.
      LEAVE TO SCREEN 0.

    ENDIF.
  ENDMETHOD.

  METHOD z_modif_ordem.

    CLEAR: it_methods, it_header, it_operation,
           it_return, wa_return.

    DATA: lv_orderid      TYPE aufnr,
          lv_refnum       TYPE ifrefnum,
          lv_oper_no      TYPE objidext,
          numero_ordem_v2 TYPE char10.

    DEFINE add_wa_methods.
      wa_methods-refnumber  = &1.
      wa_methods-objecttype = &2.
      wa_methods-method     = &3.
      wa_methods-objectkey  = &4.
      APPEND wa_methods TO it_methods.
      CLEAR wa_methods.
    END-OF-DEFINITION.

    lv_orderid = 1.
    lv_refnum  = 1.

    SHIFT lv_orderid RIGHT DELETING TRAILING space.
    TRANSLATE lv_orderid USING ' 0'.
    lv_orderid+0(1) = '%'.

    SHIFT lv_refnum RIGHT DELETING TRAILING space.
    TRANSLATE lv_refnum USING ' 0'.

    lv_oper_no = lv_orderid.
    lv_oper_no+12(4) = '0010'.

    add_wa_methods:
      lv_refnum    'HEADER'  'CHANGE' orderid,
*      LV_REFNUM 'OPERATION'  'CHANGE' LV_OPER_NO,
*      LV_REFNUM    'HEADER' 'RELEASE' ORDERID,
             ''          ''    'SAVE' orderid.


*    WA_HEADER-SCHED_TYPE   = ABAP_FALSE.
*    WA_HEADER-AUTOSCHED    = ABAP_FALSE.
*    WA_HEADER-CAP_REQMTS   = ABAP_FALSE.
*    WA_HEADER-SCHEDULING_EXACT_BREAK_TIMES = ABAP_FALSE.
*    WA_HEADER-PRIORITY     = PRIOK.
    wa_header-orderid      = orderid.
    wa_header-order_type   = order_type.
    wa_header-funct_loc    = funct_loc.
    wa_header-start_date   = start_date.
    wa_header-finish_date  = start_date.
    wa_header-planplant    = planplant.
    wa_header-bus_area     = bus_area.
    wa_header-plant        = plant.
    wa_header-maintplant   = maintplant.
    wa_header-equipment    = equipment.
*    WA_HEADER-START_DATE   = SY-DATUM.
    APPEND wa_header TO it_header.
    CLEAR wa_header.

    wa_operation-activity    = activity.
    wa_operation-control_key = control_key.
*    WA_OPERATION-DESCRIPTION = DESCRIPTION.
    APPEND wa_operation TO it_operation.
    CLEAR wa_operation.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
      TABLES                                "#EC CI_USAGE_OK[2438131]
        it_methods   = it_methods
        it_header    = it_header
        it_operation = it_operation
        return       = it_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.

    gt_return = it_return.


  ENDMETHOD.

  METHOD dados_operacao.
    DATA: gt_afvv TYPE TABLE OF afvv.
    DATA: gt_afru TYPE TABLE OF afru.

    DATA(laufnr) = |{ aufnr ALPHA = IN }|.
    und = 'MIN'.

    SELECT *
    FROM afvv AS a
    INNER JOIN afko AS b ON b~aufpl EQ a~aufpl
      INTO CORRESPONDING FIELDS OF TABLE gt_afvv
      WHERE b~aufnr EQ laufnr.

    IF gt_afvv IS NOT INITIAL.
      LOOP AT gt_afvv ASSIGNING FIELD-SYMBOL(<l_afvv>).
        IF <l_afvv>-arbeh EQ 'MIN'.
          ADD <l_afvv>-arbei TO hr_plan.

        ELSEIF <l_afvv>-arbeh EQ 'H'.
          <l_afvv>-arbei = ( <l_afvv>-arbei * 60 ).
          ADD <l_afvv>-arbei TO hr_plan.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT *
    FROM afru
      INTO TABLE gt_afru
      WHERE aufnr EQ laufnr.

    IF gt_afru IS NOT INITIAL.
      LOOP AT gt_afru ASSIGNING FIELD-SYMBOL(<w_afru>).
        IF <w_afru>-ismne EQ 'MIN'.
          ADD <w_afru>-ismnw TO hr_apont.
        ELSEIF <w_afru>-ismne EQ 'H'.
          <w_afru>-ismnw = ( <w_afru>-ismnw * 60 ).
          ADD <w_afru>-ismnw TO hr_apont.
        ENDIF.
      ENDLOOP.
    ENDIF.




  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST001'.
  SET TITLEBAR 'SET01'.



  IF ( obj_custom_0110 IS INITIAL ).


    PERFORM alv_preenche_cat USING:

* 1 'BUKRS     ' ''   '60'  ''  ''   ''  'Empresa        '  ''  ''  '',
  2 'WERKS     ' ''   '60'  ''  ''   ''  'Centro         '  ''  ''  '',
  3 'TPLNR     ' ''   '30'  ''  ''   ''  'Local Inst     '  ''  ''  '',
  4 'EQUNR     ' ''   '10'  ''  ''   ''  'Equipamento    '  ''  ''  'X',
  5 'EQKTX     ' ''   '50'  ''  ''   ''  'Texto equip    '  ''  ''  ' ',
  6 'AUART     ' ''   '20'  ''  ''   ''  'Tipo de ordem  '  ''  ''  '',
  7 'AUFNR     ' ''   '20'  ''  ''   ''  'Ordem          '  ''  ''  'X',
  8 'KTEXT     ' ''   '50'  ''  ''   ''  'Texto breve    '  ''  ''  '',
 09 'ARBEI     ' ''   '20'  ''  ''   'X'  'Hr planejadas  '  ''  ''  '',
 10 'ISMNW     ' ''   '20'  ''  ''   'X'  'Hr apontadas   '  ''  ''  '',
 11 'ARBEH     ' ''   '20'  ''  ''   ''  'Und            '  ''  ''  '',
 12 'DESV      ' ''   '20'  ''  ''   'X'  'Desvio         '  ''  ''  '',
 13 'ERDAT     ' ''   '20'  ''  ''   ''  'Data de entrada'  ''  ''  '',
 14 'GSTRP     ' ''   '20'  ''  ''   ''  'Inicio base    '  ''  ''  'X',
 15 'GLTRP     ' ''   '20'  ''  ''   ''  'Fim base       '  ''  ''  'X',
 16 'DSEQUIPE  ' ''   '20'  ''  ''   ''  'Equipe         '  ''  ''  '',
 17 'ZSTATUS   ' ''   '20'  ''  ''   ''  'Status planj   '  ''  ''  '',
 18 'TXT04     ' ''   '20'  ''  ''   ''  'Status ordem   '  ''  ''  ''.


    CREATE OBJECT obj_custom_0110
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0110
      EXPORTING
        i_parent          = obj_custom_0110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.


    SET HANDLER:
        lcl_event_handler=>on_data_changed  FOR obj_alv_0110,
*        LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR OBJ_ALV_0110,
        lcl_event_handler=>on_double_click  FOR obj_alv_0110,
        lcl_event_handler=>set_toolbar      FOR obj_alv_0110,
        lcl_event_handler=>get_ucomm        FOR obj_alv_0110.


    CALL METHOD obj_alv_0110->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        it_toolbar_excluding          = gt_exc_button
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = it_fcat
        it_outtab                     = gt_ordem
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.



    CALL METHOD obj_alv_0110->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ENDIF.

  CALL METHOD obj_alv_0110->refresh_table_display
    EXPORTING
      is_stable = wa_stable.


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

    WHEN 'SHOW_MSG'.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen   = '100'
          i_show     = 'X'
          i_repid    = sy-repid
        IMPORTING
          e_messagem = wa_mensagem
        TABLES
          it_msgs    = it_msg_return.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
