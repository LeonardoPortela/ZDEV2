*  ***********************************************************************
*                ******************************************              *
*                *                 AMAGGI                 *              *
*                *       CONFIDENCIAL E PROPRIETÁRIO      *              *
*                *      TODOS OS DIREITOS RESERVADOS      *              *
*                ******************************************              *
*  ***********************************************************************
*   Projeto       : Projeto Controle Ferramentaria                       *
*   Objetivo      : Devolução e Estorno de Emprestimo                    *
*   Analista      : Alexandre Suzan WAGON                                *
*   Desenvolvedor : Alexandre Suzan WAGON                                *
*   Data          : 27/11/2020                                           *
*   Transação     : ZPMR0067                                             *
*   Observação    : Devolver ou Estornar emprestimo                      *
*  ----------------------------------------------------------------------*
  REPORT zpmr0067 MESSAGE-ID z_mm NO STANDARD PAGE HEADING.

  TABLES: mkpf,
          qmel,
          mara,
          pa0001,
          zpmt0040.
*  ----------------------------------------------------------------------*
*   Tipos
*  ----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_reg,
           w_conteudo TYPE max_segm,
         END   OF ty_reg,

         BEGIN OF yg_alv,
           qmnum         TYPE zpmt0040-qmnum,
           mat_doc       TYPE zpmt0040-mat_doc,
           doc_year      TYPE zpmt0040-doc_year,
           bldat         TYPE bldat,
           data_cri      TYPE sy-datum,
           pernr         TYPE pa0001-pernr,
           name1(40)     TYPE c,
           werks         TYPE werks_d,                       " Centro
           matnr         TYPE matnr,                         " Material
           descr(40)     TYPE c,
           equnr         TYPE zpmt0040-equnr, "US - 77011 - CBRAND
           eqktx         TYPE eqkt-eqktx,  "US - 77011 - CBRAND
           labst_ini     TYPE c LENGTH 16,                   " Quantidade inicial
           labst_emp     TYPE c LENGTH 16,                   " Quantidade emprestado
           labst         TYPE zpmt0040-labst,                   " Quantidade
           labst_dev     TYPE c LENGTH 16,                   " Quantidade devolvida
           labst_atual   TYPE c LENGTH 16,                   " Quantidade atual
           proces        TYPE c LENGTH 01,                        " Processado
           obs(50)       TYPE c,
           motv_desc(50) TYPE c,
         END OF yg_alv.

*  ----------------------------------------------------------------------*
*   Tabelas
*  ----------------------------------------------------------------------*
  DATA: tg_alv      TYPE TABLE OF yg_alv,
        tg_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat.

  DATA: tg_zpmt0043 TYPE TABLE OF zpmt0043.
  DATA: eg_zpmt0043 TYPE zpmt0043.

  DATA: tg_zpmt0040 TYPE TABLE OF zpmt0040.

*  ----------------------------------------------------------------------*
*   Estruturas
*  ----------------------------------------------------------------------*
  DATA: eg_layout     TYPE lvc_s_layo.
  DATA: eg_alv TYPE yg_alv.

  DATA: eg_zpmt0040 TYPE zpmt0040.

*  ----------------------------------------------------------------------*
*   Variáveis
*  ----------------------------------------------------------------------*
  DATA:  vg_code TYPE sy-ucomm.

  DATA: gv_url_digital_left  TYPE char255,
        gv_url_digital_right TYPE char255.

*  ----------------------------------------------------------------------*
*   Constantes
*  ----------------------------------------------------------------------*
  CONSTANTS:
    cg_erro_id TYPE c LENGTH 08 VALUE '@S_TL_R@',
    cg_suce_id TYPE c LENGTH 08 VALUE '@S_TL_G@'.

*  ----------------------------------------------------------------------*
*   ALV
*  ----------------------------------------------------------------------*
  DATA: og_alv   TYPE REF TO cl_gui_alv_grid,
        o_cont   TYPE REF TO cl_gui_docking_container,
        o_parent TYPE REF TO cl_gui_container.

*local class to handle semantic checks
  CLASS lcl_event_receiver DEFINITION DEFERRED.

  DATA: g_verifier TYPE REF TO lcl_event_receiver.

*  &---------------------------------------------------------------------*
*  &      CLASS: LCL_EVENT DEFINITION
*  &---------------------------------------------------------------------*
  CLASS lcl_event_receiver DEFINITION.                      "#EC *
    PUBLIC SECTION.

      DATA: error_in_data TYPE c.

      CLASS-METHODS:
        handle_toolbar
          FOR EVENT toolbar OF cl_gui_alv_grid
          IMPORTING e_object," e_interactive,
        handle_menu_button
          FOR EVENT menu_button OF cl_gui_alv_grid
          IMPORTING e_object e_ucomm,
        handle_user_command
          FOR EVENT user_command OF cl_gui_alv_grid
          IMPORTING e_ucomm.

      METHODS:
        handle_data_changed
          FOR EVENT data_changed OF cl_gui_alv_grid
          IMPORTING er_data_changed.


    PRIVATE SECTION.

      METHODS:
        perform_semantic_checks
          IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  ENDCLASS.                    "lcl_event DEFINITION
*  &---------------------------------------------------------------------*
*  &      CLASS: lcl_event_receiver IMPLEMENTATION
*  &---------------------------------------------------------------------*
  CLASS lcl_event_receiver IMPLEMENTATION.

    METHOD handle_data_changed.
*
      DATA: ls_good TYPE lvc_s_modi,
            l_price TYPE s_price,
            ls_new  TYPE lvc_s_moce.

      error_in_data = space.

* check mt_good_cells semantically
      CALL METHOD perform_semantic_checks( er_data_changed ).

      IF error_in_data = 'X'.
        CALL METHOD er_data_changed->display_protocol.
      ENDIF.

    ENDMETHOD.

    METHOD handle_toolbar.
      PERFORM zf_handle_toolbar USING e_object.
    ENDMETHOD.                    "handle_toolbar

    METHOD handle_menu_button.
      PERFORM zf_handle_menu_button USING e_ucomm
                                          e_object.
    ENDMETHOD.                    "handle_menu_button

    METHOD handle_user_command.
      PERFORM zf_handle_user_command USING e_ucomm.
    ENDMETHOD.                    "handle_user_command


*---------------------------------------------------------
    METHOD perform_semantic_checks.
      DATA: ls_good     TYPE lvc_s_modi,
            l_labst     TYPE zpmt0040-labst,
            l_labst_emp TYPE zpmt0040-labst.
*          L_DESCR TYPE MAKTX.
*
*    DATA: L_NAME TYPE PA0001-ENAME.
*
*    DATA: LS_OUTTAB LIKE LINE OF GT_OUTTAB.
*
*    DATA: L_FIREDATE TYPE P0000-BEGDA.
*
      LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
        CASE ls_good-fieldname.

          WHEN  'LABST'.

            CALL METHOD pr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = ls_good-fieldname
              IMPORTING
                e_value     = l_labst.

            CALL METHOD pr_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'LABST_EMP'
              IMPORTING
                e_value     = l_labst_emp.

            IF l_labst IS NOT INITIAL AND l_labst <= 0.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = '0K'
                  i_msgno     = '000'
                  i_msgty     = 'E'
                  i_msgv1     = TEXT-m02
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.

              error_in_data = 'X'.

            ENDIF.
            IF l_labst IS NOT INITIAL AND l_labst > l_labst_emp.

              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = '0K'
                  i_msgno     = '000'
                  i_msgty     = 'E'
                  i_msgv1     = TEXT-m03
                  i_fieldname = ls_good-fieldname
                  i_row_id    = ls_good-row_id.

              error_in_data = 'X'.

            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDMETHOD.


  ENDCLASS.                    "lcl_event IMPLEMENTATION
  DATA v_event TYPE REF TO lcl_event_receiver.

*  ----------------------------------------------------------------------*
*   SELECTION-SCREEN
*  ----------------------------------------------------------------------*
  SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO-EXTENSION NO INTERVALS OBLIGATORY,
                    s_qmnum FOR qmel-qmnum NO-EXTENSION NO INTERVALS,
                    s_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS,
                    s_equnr FOR zpmt0040-equnr NO-EXTENSION NO INTERVALS.

    SELECT-OPTIONS  s_budat FOR mkpf-budat.
    SELECTION-SCREEN SKIP.

  SELECTION-SCREEN: END OF BLOCK b1.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_qmnum-low.

    PERFORM qmnum_f4 CHANGING s_qmnum-low.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pernr-low.

    PERFORM pernr_f4 CHANGING s_pernr-low.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_matnr-low.

    PERFORM matnr_f4 CHANGING s_matnr-low.

*  ----------------------------------------------------------------------*
  START-OF-SELECTION.
*  ----------------------------------------------------------------------*
    PERFORM: zf_seleciona_dados.
    PERFORM: zf_exibe_alv.


*  &---------------------------------------------------------------------*
*  &      Form  zf_exibe_alv
*  &---------------------------------------------------------------------*
  FORM zf_exibe_alv.
    CALL SCREEN 0100.
  ENDFORM.                    "zf_exibe_alv
*  &---------------------------------------------------------------------*
*  &      Form  zf_handle_toolbar
*  &---------------------------------------------------------------------*
  FORM zf_handle_toolbar USING e_object TYPE REF TO cl_alv_event_toolbar_set.

    DATA: el_toolbar TYPE stb_button.

    CLEAR el_toolbar.
    el_toolbar-butn_type  = 3.
    APPEND el_toolbar TO e_object->mt_toolbar.

    CLEAR el_toolbar.
    el_toolbar-function   = 'SELECT'.
    el_toolbar-icon       = icon_select_all.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo  = 'Marcar'(t10).
    el_toolbar-disabled   = space.
    APPEND el_toolbar TO e_object->mt_toolbar.

    CLEAR el_toolbar.
    el_toolbar-function   = 'DESELECT'.
    el_toolbar-icon       = icon_deselect_all.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo  = 'Desmarcar'(t11).
    el_toolbar-disabled   = space.
    APPEND el_toolbar TO e_object->mt_toolbar.


    CLEAR el_toolbar.
    el_toolbar-function   = 'DEVOL'.
    el_toolbar-icon       = icon_complete.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Efetuar Devolução'(t42).
    el_toolbar-text      = 'Efetuar Devolução'(t42).
    APPEND el_toolbar TO e_object->mt_toolbar.

    CLEAR el_toolbar.
    el_toolbar-function   = 'ESTOR'.
    el_toolbar-icon       = icon_storno.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Efetuar Estorno'(t40).
    el_toolbar-text      = 'Efetuar Estorno'(t40).
    APPEND el_toolbar TO e_object->mt_toolbar.

    CLEAR el_toolbar.
    el_toolbar-function   = 'DESC'.
    el_toolbar-icon       = icon_delete.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Efetuar Descarte'(t43).
    el_toolbar-text      = 'Efetuar Descarte'(t43).
    APPEND el_toolbar TO e_object->mt_toolbar.

    CLEAR el_toolbar.
    el_toolbar-function   = 'IMPR'.
    el_toolbar-icon       = icon_print.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Impressão Termo Devolução'(t44).
    el_toolbar-text      = 'Impressão Termo Devolução'(t44).
    APPEND el_toolbar TO e_object->mt_toolbar.

*    CLEAR el_toolbar.
*    el_toolbar-function   = 'IMPR_DEV'.
*    el_toolbar-icon       = icon_print.
*    el_toolbar-butn_type  = 0.
*    el_toolbar-quickinfo = 'Impressão Termo Devolução'(t44).
*    el_toolbar-text      = 'Impressão Termo Devolução'(t44).
*    APPEND el_toolbar TO e_object->mt_toolbar.

  ENDFORM.                    "zf_handle_toolbar
*  &---------------------------------------------------------------------*
*  &      Form  zf_handle_menu_button
*  &---------------------------------------------------------------------*
  FORM zf_handle_menu_button  USING e_ucomm  TYPE        sy-ucomm
                                    e_object TYPE REF TO cl_ctmenu.
    CASE e_ucomm.
      WHEN 'SELECT'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'SELECT'
            text  = 'Select All'(t14).
      WHEN 'DESELECT'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'DESELECT'
            text  = 'De Select All'(t15).
      WHEN 'INSERT'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'INSERT'
            text  = 'Carga Materiais'(t16).
    ENDCASE.
  ENDFORM.                    "zf_handle_menu_button
*  &---------------------------------------------------------------------*
*  &      Form  zf_handle_user_command
*  &---------------------------------------------------------------------*
  FORM zf_handle_user_command USING x_ucomm.

    DATA: tl_alv    TYPE TABLE OF yg_alv,
          tl_item   TYPE TABLE OF bapi2017_gm_item_create,
          tl_return TYPE TABLE OF bapiret2.

    FIELD-SYMBOLS:
      <fl_alv>  TYPE yg_alv,
      <fl_item> TYPE bapi2017_gm_item_create.

    DATA: el_header TYPE bapi2017_gm_head_01,
          el_code   TYPE bapi2017_gm_code,
          el_return TYPE bapiret2.

    DATA: vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
          vl_doc_year TYPE bapi2017_gm_head_ret-doc_year.

    DATA: tl_notifitem   TYPE TABLE OF  bapi2080_notitemi,
          tl_notifitem_x TYPE TABLE OF  bapi2080_notitemi_x,
          tl_notifcaus   TYPE TABLE OF  bapi2080_notcausi,
          tl_notifcaus_x TYPE TABLE OF  bapi2080_notcausi_x,
          tl_notifactv   TYPE TABLE OF  bapi2080_notactvi,
          tl_notifactv_x TYPE TABLE OF  bapi2080_notactvi_x,
          tl_notiftask   TYPE TABLE OF  bapi2080_nottaski,
          tl_notiftask_x TYPE TABLE OF  bapi2080_nottaski_x.

    DATA: el_notifheader   TYPE bapi2080_nothdri,
          el_notifheader_x TYPE  bapi2080_nothdri_x.

    DATA: el_notifheader_export TYPE bapi2080_nothdre.
    DATA: el_maintactytype_export TYPE  ila.

    DATA: el_syststat TYPE bapi2080_notsti.

    DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041.
    DATA: ls_zpmt0041 TYPE zpmt0041.

    DATA: lv_answer TYPE c.
    DATA: biometry              TYPE REF TO zcl_biometry.

    DATA: ls_alv      TYPE yg_alv.

    CASE x_ucomm.
      WHEN 'SELECT'.
        UNASSIGN <fl_alv>.
        LOOP AT tg_alv ASSIGNING <fl_alv>.
          CHECK <fl_alv>-proces IS INITIAL.
          MOVE abap_true TO <fl_alv>-proces.
        ENDLOOP.

      WHEN 'DESELECT'.
        UNASSIGN <fl_alv>.
        LOOP AT tg_alv ASSIGNING <fl_alv>.
          <fl_alv>-proces = ''.
        ENDLOOP.

      WHEN 'DEVOL'.
        IF tg_alv[] IS NOT INITIAL.
**********************************************************************"146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
*          read table TG_ALV into LS_ALV index 1.
*
*          call function 'POPUP_TO_CONFIRM'
*            exporting
*              TITLEBAR       = 'Atenção'
*              TEXT_QUESTION  = 'Utilizar biometria?'
*              TEXT_BUTTON_1  = 'Sim'(P01)
*              TEXT_BUTTON_2  = 'Não'(P02)
*            importing
*              ANSWER         = LV_ANSWER
*            exceptions
*              TEXT_NOT_FOUND = 1
*              others         = 2.
*          if LV_ANSWER = '1'.
*            BIOMETRY                   = new ZCL_BIOMETRY( ).
*            clear: GV_URL_DIGITAL_RIGHT,
*                   GV_URL_DIGITAL_LEFT.
*
*            try.
*                read table TG_ZPMT0043 into EG_ZPMT0043 with key EMPRESTADOR = LS_ALV-PERNR.
*                if SY-SUBRC = 0.
*                  call method BIOMETRY->READ_DIGITAL(
*                    exporting
*                      REGISTRATION = EG_ZPMT0043-SUBSTITUTO
*                    receiving
*                      RESULT       = data(_RESULT_SUB) ).
*
*                  data(_URLR_SUB) =
*                                         BIOMETRY->GET_DIGITAL_AS_URL_IMAGE2( IMAGE_XSTRING = _RESULT_SUB-IM_POLEGAR_DIREITO ).
*                  GV_URL_DIGITAL_RIGHT = _URLR_SUB.
*
*                  data(_URLL_SUB) =
*                            BIOMETRY->GET_DIGITAL_AS_URL_IMAGE2( IMAGE_XSTRING = _RESULT_SUB-IM_POLEGAR_ESQUERDO ).
*                  GV_URL_DIGITAL_LEFT = _URLL_SUB.
*
*                else.
*                  call method BIOMETRY->READ_DIGITAL(
*                    exporting
*                      REGISTRATION = LS_ALV-PERNR
*                    receiving
*                      RESULT       = data(_RESULT) ).
*                  data(_URLR) =
*                                         BIOMETRY->GET_DIGITAL_AS_URL_IMAGE2( IMAGE_XSTRING = _RESULT-IM_POLEGAR_DIREITO ).
*                  GV_URL_DIGITAL_RIGHT = _URLR.
*
*                  data(_URLL) =
*                            BIOMETRY->GET_DIGITAL_AS_URL_IMAGE2( IMAGE_XSTRING = _RESULT-IM_POLEGAR_ESQUERDO ).
*                  GV_URL_DIGITAL_LEFT = _URLL.
*                endif.
*
*              catch ZCX_BIOMETRY.
*                message text-012 type 'S' display like 'E'.
*
*            endtry.
*          endif.
*
*
*          perform Z_DEVOLUCAO.

          DATA: _resultado TYPE char1,
                _msg       TYPE string.

          CALL FUNCTION 'ZPM_CHECK_AUTHORIZE'
            EXPORTING
              i_matricula = s_pernr-low
            IMPORTING
              e_result    = _resultado
              e_message   = _msg.

          IF sy-subrc = 0.
            IF _resultado = 'S'.
              MESSAGE _msg TYPE 'I' DISPLAY LIKE _resultado.
              PERFORM z_devolucao.
            ELSE.
              MESSAGE _msg TYPE 'I' DISPLAY LIKE _resultado.
              EXIT.
            ENDIF.
          ENDIF.
*          PERFORM z_devolucao.
**********************************************************************

        ENDIF.
      WHEN 'DESC'.

        PERFORM z_descarte.

      WHEN 'ESTOR'.

        PERFORM z_estorno.

      WHEN 'IMPR'.

        PERFORM z_call_form.

      WHEN 'IMPR_DEV'.

        PERFORM z_call_form_dev.


    ENDCASE.

    REFRESH tg_alv.
    CLEAR eg_alv.

    PERFORM: zf_seleciona_dados.

    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDFORM.                    "zf_handle_user_command
*  &---------------------------------------------------------------------*
*  &      Module  STATUS_0100  OUTPUT
*  &---------------------------------------------------------------------*
  MODULE status_0100 OUTPUT.

    DATA tl_exclude TYPE ui_functions.                      "#EC *

    DATA el_variant TYPE disvariant.                        "#EC *

    SET PF-STATUS 'ZALV_STANDARD'.

    IF NOT o_cont IS INITIAL.
      CALL METHOD og_alv->refresh_table_display
        EXCEPTIONS
          finished = 1
          OTHERS   = 2.
    ELSE.
      CREATE OBJECT o_cont
        EXPORTING
          side      = cl_gui_docking_container=>dock_at_top
          repid     = sy-repid
          dynnr     = '0100'
          extension = 1000
        EXCEPTIONS
          OTHERS    = 6.

      o_parent = o_cont.

      CREATE OBJECT og_alv
        EXPORTING
          i_parent = o_parent.

      CREATE OBJECT g_verifier.
      SET HANDLER g_verifier->handle_data_changed FOR og_alv.

      PERFORM: zf_monta_fieldcat,
               zf_layout,
               zf_exclude_tb_functions TABLES tl_exclude.

      el_variant-report   = sy-repid.
      el_variant-username = sy-uname.

      CREATE OBJECT v_event.
      SET HANDLER v_event->handle_toolbar
                  v_event->handle_menu_button
                  v_event->handle_user_command FOR ALL INSTANCES.

      eg_layout-cwidth_opt = 'X'.
      eg_layout-stylefname = 'CELLTAB'.

      CALL METHOD og_alv->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = tl_exclude
*         is_variant           = el_variant
          is_layout            = eg_layout
*         i_save               = 'A'
        CHANGING
          it_outtab            = tg_alv
          it_fieldcatalog      = tg_fieldcat.

      CALL METHOD og_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD og_alv->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      CALL METHOD og_alv->set_toolbar_interactive.
    ENDIF.

  ENDMODULE.                 " STATUS_0100  OUTPUT
*  &---------------------------------------------------------------------*
*  &      Form  zf_monta_fieldcat
*  &---------------------------------------------------------------------*
  FORM zf_monta_fieldcat.

    DATA el_fieldcat TYPE lvc_s_fcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'PROCES'.
    el_fieldcat-tabname   = 'TG_ALV'.
    el_fieldcat-coltext   = 'Selecionar'.
    el_fieldcat-checkbox  = abap_true.
    el_fieldcat-edit      = abap_true.
    el_fieldcat-outputlen = '10'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'BLDAT'.
    el_fieldcat-coltext   = 'Dt.Emprest.'.
    el_fieldcat-outputlen = '10'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'PERNR'.
    el_fieldcat-coltext   = 'No.Pessoal'.
    el_fieldcat-outputlen = '12'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'NAME1'.
    el_fieldcat-coltext   = 'Desc.No.Pessoal'.
    el_fieldcat-outputlen = '30'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'MATNR'.
    el_fieldcat-coltext   = 'Material'.
    el_fieldcat-outputlen = '15'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'DESCR'.
    el_fieldcat-coltext   = 'Descrição'.
    el_fieldcat-outputlen = '30'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'EQUNR'.
    el_fieldcat-coltext   = 'Equipamento'.
    el_fieldcat-outputlen = '15'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'EQKTX'.
    el_fieldcat-coltext   = 'Descrição'.
    el_fieldcat-outputlen = '30'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.



*    CLEAR el_fieldcat.
*    el_fieldcat-fieldname = 'LABST_INI'.
*    el_fieldcat-ref_table = 'MARD'.
*    el_fieldcat-ref_field = 'LABST'.
*    el_fieldcat-coltext   = 'Estoque Inicial'.
*    el_fieldcat-outputlen = '8'.
*    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'LABST_EMP'.
    el_fieldcat-ref_table = 'MARD'.
    el_fieldcat-ref_field = 'LABST'.
    el_fieldcat-coltext   = 'Qtde Emprestada'.
    el_fieldcat-outputlen = '8'.
    APPEND el_fieldcat TO tg_fieldcat.

***********************************************************************"146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
*    CLEAR el_fieldcat.
*    el_fieldcat-fieldname = 'LABST'.
*    el_fieldcat-tabname   = 'TG_ALV'.
*    el_fieldcat-coltext   = 'Qtde a Devolv.'.
*    "el_fieldcat-edit      = abap_true. "US - 77011 - CBRAND
*    el_fieldcat-edit      = abap_false.
*    APPEND el_fieldcat TO tg_fieldcat.
***********************************************************************

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'DATA_CRI'.
    el_fieldcat-tabname   = 'TG_ALV'.
    el_fieldcat-coltext   = 'Data'.
    el_fieldcat-outputlen = '10'.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'LABST_ATUAL'.
    el_fieldcat-ref_table = 'MARD'.
    el_fieldcat-ref_field = 'LABST'.
    el_fieldcat-coltext   = 'Estoque Atual'.
    el_fieldcat-outputlen = '8'.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'MOTV_DESC'.
    el_fieldcat-tabname   = 'TG_ALV'.
    el_fieldcat-coltext   = 'Motivo Descarte'.
    el_fieldcat-outputlen = '50'.
    el_fieldcat-edit      = abap_true. "US - 77011 - CBRAND
    APPEND el_fieldcat TO tg_fieldcat.


  ENDFORM.                    "zf_monta_fieldcat
*  &---------------------------------------------------------------------*
*  &      Form  zf_layout
*  &---------------------------------------------------------------------*
  FORM zf_layout.
    eg_layout-zebra = abap_true.
  ENDFORM.                    " zf_layout
*  &---------------------------------------------------------------------*
*  &      Form  zf_exclude_tb_functions
*  &---------------------------------------------------------------------*
  FORM zf_exclude_tb_functions TABLES pt_exclude TYPE ui_functions.

    REFRESH pt_exclude.
    APPEND og_alv->mc_fc_detail            TO pt_exclude. "Botão Detalhe
    APPEND og_alv->mc_fc_refresh           TO pt_exclude. "Botão Refresh
    APPEND og_alv->mc_fc_loc_cut           TO pt_exclude. "Botão Recortar
    APPEND og_alv->mc_fc_loc_paste         TO pt_exclude. "Botão Colar com Sobregravação
    APPEND og_alv->mc_fc_loc_paste_new_row TO pt_exclude. "Botão Colar em Nova Linha
    APPEND og_alv->mc_fc_loc_copy_row      TO pt_exclude. "Botão Duplicar Linha
    APPEND og_alv->mc_fc_loc_append_row    TO pt_exclude. "Botão Anexar Linha
    APPEND og_alv->mc_fc_loc_insert_row    TO pt_exclude. "Botão Inserir Linha
    APPEND og_alv->mc_fc_loc_delete_row    TO pt_exclude. "Botão Deletar Linha
    APPEND og_alv->mc_fc_loc_copy          TO pt_exclude. "Botão Copiar Texto
    APPEND og_alv->mc_fc_loc_undo          TO pt_exclude. "Botão Anular
    APPEND og_alv->mc_fc_graph             TO pt_exclude. "Botão Grafico
    APPEND og_alv->mc_fc_info              TO pt_exclude. "Botão Help
    APPEND og_alv->mc_fc_send              TO pt_exclude.
    APPEND og_alv->mc_fc_separator         TO pt_exclude.
    APPEND og_alv->mc_fc_sort              TO pt_exclude.
    APPEND og_alv->mc_fc_sort_asc          TO pt_exclude.
    APPEND og_alv->mc_fc_sort_dsc          TO pt_exclude.
    APPEND og_alv->mc_fc_subtot            TO pt_exclude.
    APPEND og_alv->mc_fc_sum               TO pt_exclude.
    APPEND og_alv->mc_fc_to_office         TO pt_exclude.
    APPEND og_alv->mc_fc_to_rep_tree       TO pt_exclude.
    APPEND og_alv->mc_fc_unfix_columns     TO pt_exclude.
    APPEND og_alv->mc_fc_url_copy_to_clipboard   TO pt_exclude.
    APPEND og_alv->mc_fc_variant_admin     TO pt_exclude.
    APPEND og_alv->mc_fc_views             TO pt_exclude.
    APPEND og_alv->mc_fc_view_crystal      TO pt_exclude.
    APPEND og_alv->mc_fc_view_excel        TO pt_exclude.
    APPEND og_alv->mc_fc_view_grid         TO pt_exclude.
    APPEND og_alv->mc_fc_view_lotus        TO pt_exclude.
    APPEND og_alv->mc_ly_no_insert_rows    TO pt_exclude.
    APPEND og_alv->mc_mb_export            TO pt_exclude.
    APPEND og_alv->mc_mb_filter            TO pt_exclude.
    APPEND og_alv->mc_mb_paste             TO pt_exclude.
    APPEND og_alv->mc_mb_subtot            TO pt_exclude.
    APPEND og_alv->mc_mb_sum               TO pt_exclude.
    APPEND og_alv->mc_mb_variant           TO pt_exclude.
    APPEND og_alv->mc_mb_view              TO pt_exclude.
    APPEND og_alv->mc_fc_print             TO pt_exclude.
    APPEND og_alv->mc_fc_find              TO pt_exclude.
    APPEND og_alv->mc_fc_find_more         TO pt_exclude.

  ENDFORM.                                        " EXCLUDE_TB_FUNCTIONS .
*  &---------------------------------------------------------------------*
*  &      Module  USER_COMMAND_0100  INPUT
*  &---------------------------------------------------------------------*
  MODULE user_command_0100 INPUT.

    CALL METHOD og_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter "trigger event after ENTER is pressed
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    CLEAR vg_code.
    vg_code = sy-ucomm.
    CASE vg_code.
      WHEN 'BACK' OR 'LEAVE'.
        CLEAR vg_code.
        LEAVE TO SCREEN 0.
      WHEN 'CANCEL'.
        CLEAR vg_code.
        LEAVE PROGRAM.
    ENDCASE.

  ENDMODULE.                 " USER_COMMAND_0100  INPUT

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GRAVA_LOG
*  &---------------------------------------------------------------------*
  FORM zf_seleciona_dados.

    TYPES: BEGIN OF ty_0041,
             werks        TYPE zpmt0041-werks,
             matnr        TYPE zpmt0041-matnr,
             pernr        TYPE zpmt0041-pernr,
             qmnum        TYPE zpmt0041-qmnum,
             qmnum_emp    TYPE zpmt0041-qmnum,
             mat_doc_emp  TYPE zpmt0041-mat_doc,
             doc_year_emp TYPE zpmt0041-doc_year,
             labst        TYPE zpmt0041-labst,
             estorno      TYPE zpmt0041-estorno,
             descarte     TYPE zpmt0041-descarte,
           END OF ty_0041.

    TYPES: BEGIN OF ty_sum,
             werks TYPE zpmt0039-werks,
             matnr TYPE zpmt0039-matnr,
             labst TYPE zpmt0039-labst,
             equnr TYPE zpmt0039-equnr, "US - 77011 - CBRAND
           END OF ty_sum.

    TYPES: BEGIN OF ty_maktx,
             matnr TYPE mara-matnr,
             maktx TYPE makt-maktx,
           END OF ty_maktx.

    TYPES: BEGIN OF ty_eqkt,
             equnr TYPE eqkt-equnr,
             eqktx TYPE eqkt-eqktx,
           END OF ty_eqkt.

    TYPES: BEGIN OF ty_pa0001,
             pernr TYPE pa0001-pernr,
             ename TYPE pa0001-ename,
           END OF ty_pa0001.

    DATA: lt_pa0001 TYPE TABLE OF ty_pa0001,
          ls_pa0001 TYPE ty_pa0001.

    DATA: lt_estoque_inicial TYPE TABLE OF ty_sum,
          ls_estoque_inicial TYPE ty_sum.

    DATA: lt_zpmt0039 TYPE TABLE OF zpmt0039,
          ls_zpmt0039 TYPE zpmt0039.

    DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041,
          ls_zpmt0041 TYPE zpmt0041.

    DATA: ls_zpmt0040 TYPE zpmt0040.

    DATA: lt_tot_0041 TYPE TABLE OF ty_0041,
          ls_tot_0041 TYPE ty_0041.

    DATA: lt_zpmt0043 TYPE TABLE OF zpmt0043,
          ls_zpmt0043 TYPE zpmt0043.

    DATA: lt_zpmt0044 TYPE TABLE OF zpmt0044,
          ls_zpmt0044 TYPE zpmt0044.

    DATA: lt_maktx TYPE TABLE OF ty_maktx,
          ls_maktx TYPE ty_maktx.

    DATA: lt_eqkt TYPE TABLE OF ty_eqkt,
          ls_eqkt TYPE ty_eqkt.

    DATA: lt_mard TYPE TABLE OF mard,
          ls_mard TYPE mard.

    DATA: lv_qtde_emp   TYPE zpmt0040-labst.

* US - 77011 - Inicio -  CBRAND.
    SELECT * INTO TABLE tg_zpmt0043
    FROM zpmt0043
      WHERE substituto IN s_pernr.

    LOOP AT lt_zpmt0043 INTO ls_zpmt0043 WHERE dt_inicio >= sy-datum
                                          AND dt_fim <= sy-datum.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_zpmt0043-emprestador ) TO  s_pernr.
      CLEAR: ls_zpmt0043.
    ENDLOOP.

    CLEAR: tg_zpmt0043.
    SELECT * INTO TABLE tg_zpmt0043
      FROM zpmt0043
        WHERE superior IN s_pernr.

    LOOP AT lt_zpmt0043 INTO ls_zpmt0043 WHERE dt_inicio >= sy-datum
                                          AND dt_fim <= sy-datum.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_zpmt0043-emprestador ) TO  s_pernr.
      CLEAR:ls_zpmt0043.
    ENDLOOP.
* US - 77011 - Fim -  CBRAND.

    SELECT * INTO TABLE lt_zpmt0039
      FROM zpmt0039
      WHERE matnr IN s_matnr
        AND equnr IN s_equnr. "US - 77011 - Cbrand.

    SORT lt_zpmt0039 BY equnr. "US - 77011 - Cbrand.
    DELETE lt_zpmt0039 WHERE equnr IS INITIAL.

    SORT lt_zpmt0039 BY mat_doc.
    DELETE lt_zpmt0039 WHERE mat_doc IS INITIAL.

    SORT lt_zpmt0039 BY matnr.

    LOOP AT lt_zpmt0039 INTO ls_zpmt0039.
      MOVE-CORRESPONDING ls_zpmt0039 TO ls_estoque_inicial.
      COLLECT ls_estoque_inicial INTO lt_estoque_inicial.
    ENDLOOP.

    SORT lt_estoque_inicial BY matnr.

    IF lt_estoque_inicial[] IS NOT INITIAL.
      SELECT matnr maktx FROM makt INTO TABLE lt_maktx
               FOR ALL ENTRIES IN lt_estoque_inicial
                    WHERE matnr = lt_estoque_inicial-matnr
                      AND spras = sy-langu.
      SORT lt_maktx BY matnr.

      SELECT equnr eqktx FROM eqkt INTO TABLE lt_eqkt
         FOR ALL ENTRIES IN lt_estoque_inicial
              WHERE equnr = lt_estoque_inicial-equnr
                AND spras = sy-langu.
      SORT lt_eqkt BY equnr.

      SELECT * FROM mard INTO TABLE lt_mard
               FOR ALL ENTRIES IN lt_estoque_inicial
                    WHERE matnr = lt_estoque_inicial-matnr
                      AND werks = lt_estoque_inicial-werks.
      SORT lt_mard BY matnr werks.

    ENDIF.

    SELECT * INTO TABLE tg_zpmt0040
      FROM zpmt0040
      WHERE pernr IN s_pernr
        AND qmnum IN s_qmnum
        AND matnr IN s_matnr
        AND budat IN s_budat
        AND equnr IN s_equnr. "US - 77011 - Cbrand

    SORT tg_zpmt0040 BY equnr. "US - 77011 - Cbrand.
    DELETE tg_zpmt0040 WHERE equnr IS INITIAL.

    SORT tg_zpmt0040 BY mat_doc.
    DELETE tg_zpmt0040 WHERE mat_doc IS INITIAL.

    SORT tg_zpmt0040 BY werks
                        matnr.

    IF tg_zpmt0040[] IS NOT INITIAL.
      SELECT pernr ename INTO TABLE lt_pa0001
        FROM pa0001
        FOR ALL ENTRIES IN tg_zpmt0040
        WHERE pernr = tg_zpmt0040-pernr.
      SORT lt_pa0001 BY pernr.
    ENDIF.

    SELECT * INTO TABLE lt_zpmt0041
      FROM zpmt0041
      WHERE pernr IN s_pernr
        AND qmnum IN s_qmnum
        AND matnr IN s_matnr
        AND budat IN s_budat
        AND equnr IN s_equnr. "US - 77011 - Cbrand.

    SORT lt_zpmt0041 BY equnr. "US - 77011 - Cbrand.
    DELETE lt_zpmt0041 WHERE equnr IS INITIAL.

    SELECT * INTO TABLE tg_zpmt0043
    FROM zpmt0043
      WHERE dt_inicio >= sy-datum
        AND dt_fim <= sy-datum.

    LOOP AT lt_zpmt0041 INTO ls_zpmt0041.
      MOVE-CORRESPONDING ls_zpmt0041 TO ls_tot_0041.
      COLLECT ls_tot_0041 INTO lt_tot_0041.
    ENDLOOP.

    SORT lt_zpmt0041 BY werks
                        matnr
                        pernr
                        qmnum
                        equnr.

    SORT tg_zpmt0040 BY werks
                        matnr
                        equnr.


    SELECT * FROM zpmt0044 INTO TABLE lt_zpmt0044
     FOR ALL ENTRIES IN tg_zpmt0040
          WHERE matnr = tg_zpmt0040-matnr
            AND werks = tg_zpmt0040-werks
            AND equnr = tg_zpmt0040-equnr
            AND descarte = 'X'.

    SORT lt_zpmt0044 BY werks
                        matnr
                        equnr.



    LOOP AT lt_estoque_inicial INTO ls_estoque_inicial.
      READ TABLE tg_zpmt0040 TRANSPORTING NO FIELDS WITH KEY werks = ls_estoque_inicial-werks
                                                             matnr = ls_estoque_inicial-matnr
                                                             equnr = ls_estoque_inicial-equnr BINARY SEARCH. "US - 77011 - Cbrand.
      IF sy-subrc = 0.

        LOOP AT tg_zpmt0040 INTO eg_zpmt0040 FROM sy-tabix.
          IF eg_zpmt0040-werks NE ls_estoque_inicial-werks OR
             eg_zpmt0040-matnr NE ls_estoque_inicial-matnr OR
             eg_zpmt0040-equnr NE ls_estoque_inicial-equnr. "US - 77011 - Cbrand.
            EXIT.
          ENDIF.

*** BUG - 78755 - CBRAND - Inicio.
          READ TABLE lt_zpmt0044 INTO ls_zpmt0044 WITH KEY werks = eg_zpmt0040-werks
                                                           matnr = eg_zpmt0040-matnr
                                                           equnr = eg_zpmt0040-equnr
                                                           descarte = 'X' BINARY SEARCH.
          IF ls_zpmt0044 IS NOT INITIAL.
            CLEAR:ls_zpmt0044.
            EXIT.
          ELSE.
*** BUG - 78755 - CBRAND - Fim.
            eg_alv-qmnum     = eg_zpmt0040-qmnum.
            eg_alv-mat_doc   = eg_zpmt0040-mat_doc.
            eg_alv-doc_year  = eg_zpmt0040-doc_year.
            eg_alv-bldat     = eg_zpmt0040-budat.
            eg_alv-data_cri  = eg_zpmt0040-data_cri.
            eg_alv-pernr     = eg_zpmt0040-pernr.
            READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = eg_zpmt0040-pernr BINARY SEARCH.
            IF sy-subrc = 0.
              eg_alv-name1     = ls_pa0001-ename.
            ENDIF.
            eg_alv-werks     = eg_zpmt0040-werks.
            eg_alv-matnr     = eg_zpmt0040-matnr.
            eg_alv-equnr     = eg_zpmt0040-equnr.
            READ TABLE lt_maktx INTO ls_maktx WITH KEY matnr = eg_zpmt0040-matnr BINARY SEARCH.
            IF sy-subrc = 0.
              eg_alv-descr     = ls_maktx-maktx.
            ENDIF.

            READ TABLE lt_eqkt  INTO ls_eqkt  WITH KEY equnr = eg_zpmt0040-equnr BINARY SEARCH.
            IF sy-subrc = 0.
              eg_alv-eqktx     = ls_eqkt-eqktx.
            ENDIF.

            eg_alv-labst_ini = ls_estoque_inicial-labst.
            eg_alv-labst_emp = eg_zpmt0040-labst.
            eg_alv-labst_dev = 0.
            READ TABLE lt_zpmt0041 TRANSPORTING NO FIELDS WITH KEY werks  = eg_zpmt0040-werks
                                                             matnr        = eg_zpmt0040-matnr
                                                             pernr        = eg_zpmt0040-pernr
                                                             qmnum        = eg_zpmt0040-qmnum
                                                             equnr        = eg_zpmt0040-equnr BINARY SEARCH. "US - 77011 - Cbrand.
            IF sy-subrc = 0.
              LOOP AT lt_zpmt0041 INTO ls_zpmt0041 FROM sy-tabix.
                IF ls_zpmt0041-werks         NE eg_zpmt0040-werks
                 OR ls_zpmt0041-matnr        NE eg_zpmt0040-matnr
                 OR ls_zpmt0041-pernr        NE eg_zpmt0040-pernr
                 OR ls_zpmt0041-qmnum        NE eg_zpmt0040-qmnum
                 OR ls_zpmt0041-equnr        NE eg_zpmt0040-equnr. "US - 77011 - Cbrand.
                  EXIT.
                ENDIF.

                IF ls_zpmt0041-estorno IS NOT INITIAL AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                ENDIF.
                IF ls_zpmt0041-descarte IS NOT INITIAL AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                ENDIF.
                IF ls_zpmt0041-descarte IS INITIAL AND ls_zpmt0041-estorno IS INITIAL AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                ENDIF.

                eg_alv-labst_dev = eg_alv-labst_dev + ls_zpmt0041-labst.
              ENDLOOP.

            ENDIF.

            READ TABLE lt_mard INTO ls_mard WITH KEY matnr = eg_alv-matnr
                                                     werks = eg_alv-werks BINARY SEARCH.
            IF sy-subrc = 0.
              eg_alv-labst_atual = ls_mard-labst.
            ELSE.
              eg_alv-labst_atual = eg_alv-labst_ini - eg_alv-labst_dev.
            ENDIF.
            IF eg_alv-labst_emp = 0.
              CLEAR eg_alv.
              CONTINUE.
            ENDIF.
**********************************************************************"146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
            IF eg_alv-labst IS INITIAL.
              eg_alv-labst = 1.
            ENDIF.
**********************************************************************
            APPEND eg_alv TO tg_alv.
            CLEAR eg_alv.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDFORM.

*  &---------------------------------------------------------------------*
*  &      Form  Z_CALL_FORM                                              *
*  &---------------------------------------------------------------------*
*                              Chama Formulário                          *
*  ----------------------------------------------------------------------*
  FORM z_call_form.

    DATA: vl_formname TYPE tdsfname,
          vl_name     TYPE rs38l_fnam.

    DATA: lt_zpms0040 TYPE TABLE OF zpms0040,
          ls_zpms0040 TYPE zpms0040.

    DATA: ls_alv TYPE yg_alv.

    DATA: lv_empregado(100) TYPE c.
    DATA: lv_dt_admissao(100) TYPE c.
    DATA: lv_unidade(100) TYPE c.
    DATA: lv_funcao(100) TYPE c.
    DATA: lv_setor(100) TYPE c.

    DATA: lt_pa0001 TYPE TABLE OF pa0001.
    DATA: ls_pa0001 TYPE pa0001.

    LOOP AT  tg_alv INTO eg_alv WHERE proces = 'X'.

      MOVE-CORRESPONDING eg_alv TO ls_zpms0040.
      ls_zpms0040-labst = eg_alv-labst_emp.
      ls_zpms0040-budat = eg_alv-bldat.
      APPEND ls_zpms0040 TO lt_zpms0040.

    ENDLOOP.

    CONCATENATE eg_alv-pernr  '-' eg_alv-name1 INTO lv_empregado SEPARATED BY space.

    IF lt_zpms0040[] IS NOT INITIAL.
      vl_formname = 'ZPMF0006'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = vl_formname
        IMPORTING
          fm_name            = vl_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.

      IF eg_alv-pernr IS NOT INITIAL.
        SELECT * INTO TABLE lt_pa0001
          FROM pa0001
          WHERE pernr = eg_alv-pernr.

        SORT  lt_pa0001 BY begda.
        READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
        lv_dt_admissao = ls_pa0001-begda+6(2)  && '/' &&  ls_pa0001-begda+4(2) && '/' && ls_pa0001-begda(4).
        lv_empregado = ls_pa0001-ename.

        SORT  lt_pa0001 BY endda DESCENDING.
        READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.

        SELECT SINGLE orgtx INTO lv_setor
          FROM t527x
          WHERE orgeh = ls_pa0001-orgeh
            AND sprsl = sy-langu
            AND endda > sy-datum.

        SELECT SINGLE stltx INTO lv_funcao
          FROM t513s
          WHERE stell = ls_pa0001-stell
            AND sprsl = sy-langu
            AND endda > sy-datum.

        CONDENSE lv_funcao.

      ENDIF.

      IF eg_alv-werks IS NOT INITIAL.
        SELECT SINGLE name1 INTO lv_unidade
          FROM t001w
          WHERE werks = eg_alv-werks.
        CONCATENATE eg_alv-werks '-' lv_unidade INTO lv_unidade SEPARATED BY space.
      ENDIF.
      CALL FUNCTION vl_name
        EXPORTING
          p_unidade        = lv_unidade
          p_empregado      = lv_empregado
          p_funcao         = lv_funcao
          p_setor          = lv_setor
          p_dt_admissao    = lv_dt_admissao
        TABLES
          t_emprest        = lt_zpms0040
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ENDFORM.                    " Z_CALL_FORM

  FORM z_devolucao.

    DATA: tl_alv    TYPE TABLE OF yg_alv,
          tl_item   TYPE TABLE OF bapi2017_gm_item_create,
          tl_return TYPE TABLE OF bapiret2.

    FIELD-SYMBOLS:
      <fl_alv>  TYPE yg_alv,
      <fl_item> TYPE bapi2017_gm_item_create.

    DATA: el_header TYPE bapi2017_gm_head_01,
          el_code   TYPE bapi2017_gm_code,
          el_return TYPE bapiret2.

    DATA: vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
          vl_doc_year TYPE bapi2017_gm_head_ret-doc_year.

    DATA: tl_notifitem   TYPE TABLE OF  bapi2080_notitemi,
          tl_notifitem_x TYPE TABLE OF  bapi2080_notitemi_x,
          tl_notifcaus   TYPE TABLE OF  bapi2080_notcausi,
          tl_notifcaus_x TYPE TABLE OF  bapi2080_notcausi_x,
          tl_notifactv   TYPE TABLE OF  bapi2080_notactvi,
          tl_notifactv_x TYPE TABLE OF  bapi2080_notactvi_x,
          tl_notiftask   TYPE TABLE OF  bapi2080_nottaski,
          tl_notiftask_x TYPE TABLE OF  bapi2080_nottaski_x.

    DATA: el_notifheader   TYPE bapi2080_nothdri,
          el_notifheader_x TYPE  bapi2080_nothdri_x.

    DATA: el_notifheader_export TYPE bapi2080_nothdre.
    DATA: el_maintactytype_export TYPE  ila.

    DATA: el_syststat TYPE bapi2080_notsti.

    DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041.
    DATA: ls_zpmt0041 TYPE zpmt0041.

    DATA: ls_header  TYPE alm_me_notif_header,
          ls_not_exp TYPE bapi2080_nothdre.

    DATA: lt_ret TYPE bapiret2_tab,
          ls_ret TYPE bapiret2.

    DATA: ls_viqmel     TYPE viqmel.

    DATA: lt_notification_partner TYPE TABLE OF  alm_me_partner_key_struct.
    DATA: ls_notification_partner TYPE alm_me_partner_key_struct.

    DATA: lv_tabix TYPE sy-tabix.

*
    LOOP AT tg_alv ASSIGNING <fl_alv>.
*
      IF <fl_alv>-proces = abap_false OR <fl_alv>-labst IS INITIAL OR <fl_alv>-qmnum IS INITIAL.
        CONTINUE.
      ENDIF.

*      IF <fl_alv>-labst = <fl_alv>-labst_emp.
      el_notifheader-coding     = '0020'.
      el_notifheader_x-coding = 'X'.

      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number               = <fl_alv>-qmnum
          notifheader          = el_notifheader
          notifheader_x        = el_notifheader_x
        IMPORTING
          notifheader_export   = el_notifheader_export
          maintactytype_export = el_maintactytype_export
        TABLES
          return               = tl_return.

      el_syststat-langu    = sy-langu.
      el_syststat-languiso = sy-langu.
      el_syststat-refdate  = sy-datum.
      el_syststat-reftime  = sy-uzeit.

      CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
        EXPORTING
          number   = <fl_alv>-qmnum
          syststat = el_syststat
        TABLES
          return   = tl_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = el_return.
*      ENDIF.

      CLEAR: el_header,
             el_code.
      el_header-pstng_date = sy-datum.
      el_header-doc_date   = <fl_alv>-bldat.
      el_header-pr_uname   = sy-uname.
      el_header-header_txt = 'DEVOLUÇÃO'.
      el_code-gm_code      = '06'.

      REFRESH tl_item.
      UNASSIGN <fl_item>.
      APPEND INITIAL LINE TO tl_item ASSIGNING <fl_item>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
      "      MOVE: <fl_alv>-matnr  TO <fl_item>-material,
      DATA(v_len12) = strlen( <fl_alv>-matnr ).
      IF v_len12 > 18.
        MOVE <fl_alv>-matnr TO <fl_item>-material_long .
      ELSE.
        MOVE <fl_alv>-matnr TO <fl_item>-material .
      ENDIF.
      MOVE:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                  <fl_alv>-werks  TO <fl_item>-plant,
                    'FER1'  TO <fl_item>-stge_loc,
                    'OF01'  TO <fl_item>-move_stloc,
                  <fl_alv>-labst_emp  TO <fl_item>-entry_qnt.
*                <fl_alv>-meins  TO <fl_item>-entry_uom.
*  *
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_item>-material
        IMPORTING
          output = <fl_item>-material.


      MOVE 'ZF4' TO <fl_item>-move_type.

      CLEAR: vl_mat_doc,
             vl_doc_year.

      REFRESH tl_return.

      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = el_header
          goodsmvt_code    = el_code
        IMPORTING
          materialdocument = vl_mat_doc
          matdocumentyear  = vl_doc_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      READ TABLE tl_return INTO el_return
                           WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        <fl_alv>-proces = ''.


        CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
          TABLES
            it_return = tl_return.


      ELSE.

        READ TABLE tg_zpmt0040 INTO eg_zpmt0040 WITH KEY qmnum = <fl_alv>-qmnum
                                                         matnr = <fl_alv>-matnr
                                                         mat_doc = <fl_alv>-mat_doc
                                                         doc_year = <fl_alv>-doc_year.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING eg_zpmt0040 TO ls_zpmt0041.
          ls_zpmt0041-qmnum_emp = <fl_alv>-qmnum.
          ls_zpmt0041-mat_doc_emp = <fl_alv>-mat_doc.
          ls_zpmt0041-doc_year_emp = <fl_alv>-doc_year.
          ls_zpmt0041-mat_doc = vl_mat_doc.
          ls_zpmt0041-doc_year = vl_doc_year.
          ls_zpmt0041-data_cri = sy-datum.
          ls_zpmt0041-hora_cri = sy-uzeit.
          ls_zpmt0041-usuario_cri = sy-uname.
          ls_zpmt0041-url_digital_right = gv_url_digital_right.
          ls_zpmt0041-url_digital_left = gv_url_digital_left.
          ls_zpmt0041-labst = <fl_alv>-labst.
          CONCATENATE ls_zpmt0041-observ '/ GOODSMVT :' vl_mat_doc '-' vl_doc_year INTO ls_zpmt0041-observ.
          APPEND ls_zpmt0041 TO lt_zpmt0041.
          CLEAR ls_zpmt0041.
        ENDIF.

        IF <fl_alv>-labst < <fl_alv>-labst_emp.

          PERFORM z_call_form_dev.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          WAIT UP TO 5 SECONDS.

          CLEAR: el_header,
                 el_code.
          el_header-pstng_date = sy-datum.
          el_header-doc_date   = sy-datum.
          el_header-pr_uname   = sy-uname.
          el_header-header_txt = 'DEVOLUÇÃO PARCIAL'.

          REFRESH tl_item.
          UNASSIGN <fl_item>.
          APPEND INITIAL LINE TO tl_item ASSIGNING <fl_item>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
          "          MOVE: <fl_alv>-matnr  TO <fl_item>-material,
          DATA(v_len9) = strlen( <fl_alv>-matnr ).
          IF v_len9 > 18.
            MOVE <fl_alv>-matnr TO <fl_item>-material_long .
          ELSE.
            MOVE <fl_alv>-matnr TO <fl_item>-material .
          ENDIF.
          MOVE:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                          <fl_alv>-werks  TO <fl_item>-plant,
                          'FER1'  TO <fl_item>-stge_loc,
                          'OF01'  TO <fl_item>-move_stloc.
          <fl_item>-entry_qnt = <fl_alv>-labst_emp - <fl_alv>-labst.
          <fl_item>-entry_uom = eg_zpmt0040-meins.
*
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = <fl_item>-material
            IMPORTING
              output = <fl_item>-material.


          MOVE 'ZF3' TO <fl_item>-move_type.
          el_code-gm_code      = '04'.

          CLEAR vl_mat_doc.
          CLEAR vl_doc_year.
          REFRESH tl_return.

          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = el_header
              goodsmvt_code    = el_code
            IMPORTING
              materialdocument = vl_mat_doc
              matdocumentyear  = vl_doc_year
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          eg_zpmt0040-mat_doc = vl_mat_doc.
          eg_zpmt0040-doc_year = vl_doc_year.
          eg_zpmt0040-data_cri = sy-datum.
          eg_zpmt0040-hora_cri = sy-uzeit.
          eg_zpmt0040-usuario_cri = sy-uname.
          eg_zpmt0040-labst = <fl_alv>-labst_emp - <fl_alv>-labst_emp .
          MODIFY zpmt0040 FROM eg_zpmt0040.

          ls_header-short_text = 'EMPRESTIMO DE FERRAMENTAS'.
          ls_header-cat_type   = 'D'.         " codificação
          ls_header-code_group = 'F0000060'.   "
          ls_header-coding     = '0010'.      "
          ls_header-notif_type = 'FR'.
          ls_header-reportedby = sy-uname.

          REFRESH lt_notification_partner.
          ls_notification_partner-partner_role = 'VW'.
          ls_notification_partner-partner_key = 'K5' &&  <fl_alv>-pernr.
          APPEND ls_notification_partner TO lt_notification_partner.


          CALL FUNCTION 'ALM_ME_NOTIFICATION_CREATE' "#EC CI_USAGE_OK[2438006]
            EXPORTING
              notification_header         = ls_header
              notif_type                  = 'FR'
              i_partner_tpa_key           = 'X'
            IMPORTING
              notification_export         = ls_not_exp
            TABLES
              notification_partner        = lt_notification_partner
              return                      = lt_ret
            EXCEPTIONS
              error_in_input_data         = 1
              notification_already_exists = 2
              task_not_rel_or_compl       = 3
              user_status_not_changed     = 4
              OTHERS                      = 5.


          CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
            EXPORTING
              i_qmnum  = ls_not_exp-notif_no
              i_commit = 'X'
            IMPORTING
              e_viqmel = ls_viqmel
            TABLES
              return   = lt_ret.

          READ TABLE lt_ret INTO ls_ret WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            READ TABLE tg_zpmt0040 INTO eg_zpmt0040 WITH KEY qmnum = <fl_alv>-qmnum
                                                             matnr = <fl_alv>-matnr
                                                             mat_doc = <fl_alv>-mat_doc
                                                             doc_year = <fl_alv>-doc_year.
            IF sy-subrc = 0.
              eg_zpmt0040-labst = <fl_alv>-labst_emp - <fl_alv>-labst.
              eg_zpmt0040-qmnum = ls_viqmel-qmnum.
              eg_zpmt0040-observ = 'NOTA QM :' &&  ls_viqmel-qmnum.

              MODIFY zpmt0040 FROM eg_zpmt0040.
            ENDIF.
          ELSE.
            CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
              TABLES
                it_return = lt_ret.
*            gt_outtab-process = ''.
*            gt_outtab-observ = ls_ret-message.
*            MODIFY gt_outtab FROM gt_outtab INDEX lv_tabix.
*            READ TABLE lt_zpmt0040 INTO ls_zpmt0040 WITH KEY matnr = gt_outtab-matnr.
*            IF sy-subrc = 0.
*              ls_zpmt0040-qmnum = ''.
*              ls_zpmt0040-observ = ls_ret-message.
*              MODIFY lt_zpmt0040 FROM ls_zpmt0040 INDEX sy-tabix.
*            ENDIF.
          ENDIF.

        ENDIF.

        <fl_alv>-labst_dev = <fl_alv>-labst_dev + <fl_alv>-labst.
        <fl_alv>-labst_atual = <fl_alv>-labst_atual + <fl_alv>-labst.
        <fl_alv>-labst_emp = <fl_alv>-labst_emp - <fl_alv>-labst.

        <fl_alv>-proces = ''.
        <fl_alv>-labst = 0.
        MESSAGE s000(z_mm) WITH 'Devolução efetuada com sucesso.'.
      ENDIF.


    ENDLOOP.

    IF lt_zpmt0041[] IS NOT INITIAL.
      MODIFY zpmt0041 FROM TABLE lt_zpmt0041.
      COMMIT WORK.
    ENDIF.

    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDFORM.

  FORM z_estorno.

    DATA: tl_alv    TYPE TABLE OF yg_alv,
          tl_item   TYPE TABLE OF bapi2017_gm_item_create,
          tl_return TYPE TABLE OF bapiret2.

    FIELD-SYMBOLS:
      <fl_alv>  TYPE yg_alv,
      <fl_item> TYPE bapi2017_gm_item_create.

    DATA: el_header TYPE bapi2017_gm_head_01,
          el_code   TYPE bapi2017_gm_code,
          el_return TYPE bapiret2.

    DATA: vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
          vl_doc_year TYPE bapi2017_gm_head_ret-doc_year.

    DATA: tl_notifitem   TYPE TABLE OF  bapi2080_notitemi,
          tl_notifitem_x TYPE TABLE OF  bapi2080_notitemi_x,
          tl_notifcaus   TYPE TABLE OF  bapi2080_notcausi,
          tl_notifcaus_x TYPE TABLE OF  bapi2080_notcausi_x,
          tl_notifactv   TYPE TABLE OF  bapi2080_notactvi,
          tl_notifactv_x TYPE TABLE OF  bapi2080_notactvi_x,
          tl_notiftask   TYPE TABLE OF  bapi2080_nottaski,
          tl_notiftask_x TYPE TABLE OF  bapi2080_nottaski_x.

    DATA: el_notifheader   TYPE bapi2080_nothdri,
          el_notifheader_x TYPE  bapi2080_nothdri_x.

    DATA: el_notifheader_export TYPE bapi2080_nothdre.
    DATA: el_maintactytype_export TYPE  ila.

    DATA: el_syststat TYPE bapi2080_notsti.

    DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041.
    DATA: ls_zpmt0041 TYPE zpmt0041.

*
    LOOP AT tg_alv ASSIGNING <fl_alv>.
*
      IF <fl_alv>-proces = abap_false OR <fl_alv>-labst = 0.
        CONTINUE.
      ENDIF.
      IF <fl_alv>-labst_emp NE <fl_alv>-labst.
        MESSAGE s000(z_mm) WITH 'Estorno parcial não permitido.' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.


      el_notifheader-coding     = '0020'.
      el_notifheader_x-coding = 'X'.

      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number               = <fl_alv>-qmnum
          notifheader          = el_notifheader
          notifheader_x        = el_notifheader_x
        IMPORTING
          notifheader_export   = el_notifheader_export
          maintactytype_export = el_maintactytype_export
        TABLES
          return               = tl_return.

      el_syststat-langu    = sy-langu.
      el_syststat-languiso = sy-langu.
      el_syststat-refdate  = sy-datum.
      el_syststat-reftime  = sy-uzeit.

      CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
        EXPORTING
          number   = <fl_alv>-qmnum
          syststat = el_syststat
        TABLES
          return   = tl_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = el_return.

      CLEAR: el_header,
             el_code.
      el_header-pstng_date = sy-datum.
      el_header-doc_date   = <fl_alv>-bldat.
      el_header-pr_uname   = sy-uname.
      el_header-header_txt = 'ESTORNO'.
      el_code-gm_code      = '06'.

      REFRESH tl_item.
      UNASSIGN <fl_item>.
      APPEND INITIAL LINE TO tl_item ASSIGNING <fl_item>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
      "      MOVE: <fl_alv>-matnr  TO <fl_item>-material,
      DATA(v_len6) = strlen( <fl_alv>-matnr ).
      IF v_len6 > 18.
        MOVE <fl_alv>-matnr TO <fl_item>-material_long .
      ELSE.
        MOVE <fl_alv>-matnr TO <fl_item>-material .
      ENDIF.
      MOVE:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                  <fl_alv>-werks  TO <fl_item>-plant,
                    'FER1'  TO <fl_item>-stge_loc,
                    'OF01'  TO <fl_item>-move_stloc,
                  <fl_alv>-labst  TO <fl_item>-entry_qnt.


      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_item>-material
        IMPORTING
          output = <fl_item>-material.


      MOVE 'ZF4' TO <fl_item>-move_type.

      CLEAR: vl_mat_doc,
             vl_doc_year.

      REFRESH tl_return.

      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = el_header
          goodsmvt_code    = el_code
        IMPORTING
          materialdocument = vl_mat_doc
          matdocumentyear  = vl_doc_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      READ TABLE tl_return INTO el_return
                           WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        <fl_alv>-proces = ''.

        CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
          TABLES
            it_return = tl_return.

      ELSE.

        READ TABLE tg_zpmt0040 INTO eg_zpmt0040 WITH KEY qmnum = <fl_alv>-qmnum
                                                         matnr = <fl_alv>-matnr
                                                         mat_doc = <fl_alv>-mat_doc
                                                         doc_year = <fl_alv>-doc_year.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING eg_zpmt0040 TO ls_zpmt0041.
          ls_zpmt0041-qmnum_emp = <fl_alv>-qmnum.
          ls_zpmt0041-mat_doc_emp = <fl_alv>-mat_doc.
          ls_zpmt0041-doc_year_emp = <fl_alv>-doc_year.
          ls_zpmt0041-mat_doc = vl_mat_doc.
          ls_zpmt0041-doc_year = vl_doc_year.
          ls_zpmt0041-data_cri = sy-datum.
          ls_zpmt0041-hora_cri = sy-uzeit.
          ls_zpmt0041-usuario_cri = sy-uname.
          ls_zpmt0041-estorno = 'X'.
          ls_zpmt0041-labst =  <fl_alv>-labst.
          CONCATENATE ls_zpmt0041-observ '/ GOODSMVT :' vl_mat_doc '-' vl_doc_year INTO ls_zpmt0041-observ.
          APPEND ls_zpmt0041 TO lt_zpmt0041.
          CLEAR ls_zpmt0041.
        ENDIF.
        <fl_alv>-labst_dev = <fl_alv>-labst_dev + <fl_alv>-labst.
        <fl_alv>-labst_atual = <fl_alv>-labst_atual + <fl_alv>-labst.
        <fl_alv>-labst_emp = <fl_alv>-labst_emp - <fl_alv>-labst.
        <fl_alv>-labst = 0.
        <fl_alv>-proces = ''.
        MESSAGE s000(z_mm) WITH 'Estorno efetuado com sucesso.'.
      ENDIF.

    ENDLOOP.

    IF lt_zpmt0041[] IS NOT INITIAL.
      MODIFY zpmt0041 FROM TABLE lt_zpmt0041.
      COMMIT WORK.
    ENDIF.

    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDFORM.

  FORM z_descarte.

    DATA: tl_alv    TYPE TABLE OF yg_alv,
          tl_item   TYPE TABLE OF bapi2017_gm_item_create,
          tl_return TYPE TABLE OF bapiret2.

    FIELD-SYMBOLS:
      <fl_alv>  TYPE yg_alv,
      <fl_item> TYPE bapi2017_gm_item_create.

    DATA: el_header TYPE bapi2017_gm_head_01,
          el_code   TYPE bapi2017_gm_code,
          el_return TYPE bapiret2.

    DATA: vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
          vl_doc_year TYPE bapi2017_gm_head_ret-doc_year.

    DATA: tl_notifitem   TYPE TABLE OF  bapi2080_notitemi,
          tl_notifitem_x TYPE TABLE OF  bapi2080_notitemi_x,
          tl_notifcaus   TYPE TABLE OF  bapi2080_notcausi,
          tl_notifcaus_x TYPE TABLE OF  bapi2080_notcausi_x,
          tl_notifactv   TYPE TABLE OF  bapi2080_notactvi,
          tl_notifactv_x TYPE TABLE OF  bapi2080_notactvi_x,
          tl_notiftask   TYPE TABLE OF  bapi2080_nottaski,
          tl_notiftask_x TYPE TABLE OF  bapi2080_nottaski_x.

    DATA: el_notifheader   TYPE bapi2080_nothdri,
          el_notifheader_x TYPE  bapi2080_nothdri_x.

    DATA: el_notifheader_export TYPE bapi2080_nothdre.
    DATA: el_maintactytype_export TYPE  ila.

    DATA: el_syststat TYPE bapi2080_notsti.

    DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041.
    DATA: ls_zpmt0041 TYPE zpmt0041.

    DATA: lwa_bdcdata   TYPE bdcdata,
          lwa_ctuparams TYPE ctu_params,
          lit_bdcdata   TYPE TABLE OF bdcdata,
          lit_msg       TYPE TABLE OF bdcmsgcoll,
          lwa_answer(1),
          gw_choice     TYPE sy-tabix.
*
    LOOP AT tg_alv ASSIGNING <fl_alv>.
*
      IF <fl_alv>-proces = abap_false OR <fl_alv>-labst = 0.
        CONTINUE.
      ENDIF.
      IF <fl_alv>-labst_emp NE <fl_alv>-labst.
        MESSAGE s000(z_mm) WITH 'Descarte parcial não permitido.' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      el_notifheader-coding     = '0040'.
      el_notifheader_x-coding = 'X'.

      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number               = <fl_alv>-qmnum
          notifheader          = el_notifheader
          notifheader_x        = el_notifheader_x
        IMPORTING
          notifheader_export   = el_notifheader_export
          maintactytype_export = el_maintactytype_export
        TABLES
          return               = tl_return.

      el_syststat-langu    = sy-langu.
      el_syststat-languiso = sy-langu.
      el_syststat-refdate  = sy-datum.
      el_syststat-reftime  = sy-uzeit.

      CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
        EXPORTING
          number   = <fl_alv>-qmnum
          syststat = el_syststat
        TABLES
          return   = tl_return.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = el_return.

      CLEAR: el_header,
             el_code.
      el_header-pstng_date = sy-datum.
      el_header-doc_date   = <fl_alv>-bldat.
      el_header-pr_uname   = sy-uname.
      el_header-header_txt = 'DANIFICADO'.
      el_code-gm_code      = '06'.

      REFRESH tl_item.
      UNASSIGN <fl_item>.
      APPEND INITIAL LINE TO tl_item ASSIGNING <fl_item>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
      "      MOVE: <fl_alv>-matnr  TO <fl_item>-material,
      DATA(v_len3) = strlen( <fl_alv>-matnr ).
      IF v_len3 > 18.
        MOVE <fl_alv>-matnr TO <fl_item>-material_long .
      ELSE.
        MOVE <fl_alv>-matnr TO <fl_item>-material .
      ENDIF.
      MOVE:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                  <fl_alv>-werks  TO <fl_item>-plant,
                    'OF01'  TO <fl_item>-stge_loc,
                    'FER1'  TO <fl_item>-move_stloc,
                  <fl_alv>-labst  TO <fl_item>-entry_qnt.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_item>-material
        IMPORTING
          output = <fl_item>-material.


      MOVE 'ZFB' TO <fl_item>-move_type.

      CLEAR: vl_mat_doc,
             vl_doc_year.

      REFRESH tl_return.

      SET UPDATE TASK LOCAL.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = el_header
          goodsmvt_code    = el_code
        IMPORTING
          materialdocument = vl_mat_doc
          matdocumentyear  = vl_doc_year
        TABLES
          goodsmvt_item    = tl_item
          return           = tl_return.

      READ TABLE tl_return INTO el_return
                           WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        <fl_alv>-proces = ''.

        CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
          TABLES
            it_return = tl_return.

      ELSE.

        READ TABLE tg_zpmt0040 INTO eg_zpmt0040 WITH KEY qmnum = <fl_alv>-qmnum
                                                         matnr = <fl_alv>-matnr
                                                         mat_doc = <fl_alv>-mat_doc
                                                         doc_year = <fl_alv>-doc_year.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING eg_zpmt0040 TO ls_zpmt0041.
          ls_zpmt0041-qmnum_emp = <fl_alv>-qmnum.
          ls_zpmt0041-mat_doc_emp = <fl_alv>-mat_doc.
          ls_zpmt0041-doc_year_emp = <fl_alv>-doc_year.
          ls_zpmt0041-mat_doc = vl_mat_doc.
          ls_zpmt0041-doc_year = vl_doc_year.
          ls_zpmt0041-data_cri = sy-datum.
          ls_zpmt0041-hora_cri = sy-uzeit.
          ls_zpmt0041-usuario_cri = sy-uname.
          ls_zpmt0041-descarte = 'X'.
          ls_zpmt0041-labst =  <fl_alv>-labst.
          CONCATENATE ls_zpmt0041-observ '/ GOODSMVT :' vl_mat_doc '-' vl_doc_year INTO ls_zpmt0041-observ.
          APPEND ls_zpmt0041 TO lt_zpmt0041.
          CLEAR ls_zpmt0041.
        ENDIF.
        <fl_alv>-labst_dev = <fl_alv>-labst_dev + <fl_alv>-labst.
        <fl_alv>-labst_atual = <fl_alv>-labst_atual + <fl_alv>-labst.
        <fl_alv>-labst_emp = <fl_alv>-labst_emp - <fl_alv>-labst.
        <fl_alv>-labst = 0.
        <fl_alv>-proces = ''.
*       MESSAGE s000(z_mm) WITH 'Descarte efetuado com sucesso.'.

*------------------------------------------
*------ efetua descarte
*------------------------------------------
        CLEAR lwa_bdcdata.
        lwa_bdcdata-program  = 'ZPMR0009'.
        lwa_bdcdata-dynpro   = '1000'.
        lwa_bdcdata-dynbegin = 'X'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_OKCODE'.
        lwa_bdcdata-fval = '=BUSCAR'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_CURSOR'.
        lwa_bdcdata-fval = 'GW_TELA-EQUNR'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TELA-EQUNR'.
        lwa_bdcdata-fval = <fl_alv>-equnr.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-program  = 'ZPMR0009'.
        lwa_bdcdata-dynpro   = '1000'.
        lwa_bdcdata-dynbegin = 'X'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_OKCODE'.
        lwa_bdcdata-fval = '=ELIMINAR'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_CURSOR'.
        lwa_bdcdata-fval = 'GW_TELA-EQUNR'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TELA-EQUNR'.
        lwa_bdcdata-fval = <fl_alv>-equnr.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-program  = 'ZPMR0009'.
        lwa_bdcdata-dynpro   = '3000'.
        lwa_bdcdata-dynbegin = 'X'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_OKCODE'.
        lwa_bdcdata-fval = '=ELIMINAR'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'BDC_CURSOR'.
        lwa_bdcdata-fval = 'GW_TL_ELIMINA-ARBPL'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TL_ELIMINA-CODE_ELIMINADOR'.
        lwa_bdcdata-fval = '0060'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TL_ELIMINA-CODE'.
        lwa_bdcdata-fval = '0110'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TL_ELIMINA-CODE_MOTIVO'.
        lwa_bdcdata-fval = '0060'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        CLEAR lwa_bdcdata.
        lwa_bdcdata-fnam = 'GW_TL_ELIMINA-ARBPL'.
        lwa_bdcdata-fval = 'FERRAMEN'.
        APPEND lwa_bdcdata TO lit_bdcdata.

        lwa_ctuparams-racommit = 'X'.
        lwa_ctuparams-dismode  = 'N'." 'A'.
        lwa_ctuparams-updmode  = 'S'.

        CALL TRANSACTION 'ZPM0022' USING lit_bdcdata OPTIONS FROM lwa_ctuparams
                                                    MESSAGES INTO lit_msg.

        READ TABLE lit_msg INTO DATA(lwa_msg) WITH KEY msgtyp = 'E'.

        IF sy-subrc = 0.
          LOOP AT lit_msg     INTO lwa_msg WHERE msgtyp = 'E'.
            CLEAR el_return.
            el_return-type       = lwa_msg-msgtyp.
            el_return-id         = lwa_msg-msgid.
            el_return-number     = lwa_msg-msgnr.
            el_return-message_v1 = lwa_msg-msgv1.
            el_return-message_v2 = lwa_msg-msgv2.
            el_return-message_v3 = lwa_msg-msgv3.
            el_return-message_v4 = lwa_msg-msgv4.
            APPEND el_return    TO tl_return.
          ENDLOOP.

          CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
            TABLES
              it_return = tl_return.
        ELSE.
          MESSAGE s000(z_mm) WITH 'Descarte efetuado com sucesso.'.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lt_zpmt0041[] IS NOT INITIAL.
      MODIFY zpmt0041 FROM TABLE lt_zpmt0041.
      COMMIT WORK.
    ENDIF.

    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

  ENDFORM.


*  &---------------------------------------------------------------------*
*  &      Form  Z_CALL_FORM                                              *
*  &---------------------------------------------------------------------*
*                              Chama Formulário                          *
*  ----------------------------------------------------------------------*
  FORM z_call_form_dev.

    DATA: vl_formname TYPE tdsfname,
          vl_name     TYPE rs38l_fnam.

    DATA: lt_zpms0041 TYPE TABLE OF zpms0041,
          ls_zpms0041 TYPE zpms0041.

    DATA: ls_alv TYPE yg_alv.

    DATA: lv_empregado(100) TYPE c.
    DATA: lv_dt_admissao(100) TYPE c.
    DATA: lv_unidade(100) TYPE c.
    DATA: lv_funcao(100) TYPE c.
    DATA: lv_setor(100) TYPE c.

    DATA: lt_pa0001 TYPE TABLE OF pa0001.
    DATA: ls_pa0001 TYPE pa0001.

    LOOP AT  tg_alv INTO eg_alv WHERE proces = 'X'.
      REFRESH lt_zpms0041.
      LOOP AT tg_alv INTO ls_alv WHERE qmnum = eg_alv-qmnum
                                   AND mat_doc = eg_alv-mat_doc
                                   AND doc_year = eg_alv-doc_year.
        IF ls_alv-labst_dev IS INITIAL.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING ls_alv TO ls_zpms0041.
        ls_zpms0041-labst = ls_alv-labst_emp.
        ls_zpms0041-budat = ls_alv-bldat.
        ls_zpms0041-labst_dev = ls_alv-labst.
        ls_zpms0041-budat_dev = sy-datum.
        APPEND ls_zpms0041 TO lt_zpms0041.
      ENDLOOP.

      CONCATENATE eg_alv-pernr  '-' eg_alv-name1 INTO lv_empregado SEPARATED BY space.

      IF lt_zpms0041[] IS NOT INITIAL.
        vl_formname = 'ZPMF0007'.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = vl_formname
          IMPORTING
            fm_name            = vl_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF eg_alv-pernr IS NOT INITIAL.
          SELECT * INTO TABLE lt_pa0001
            FROM pa0001
            WHERE pernr = eg_alv-pernr.

          SORT  lt_pa0001 BY begda.
          READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
          lv_dt_admissao = ls_pa0001-begda+6(2)  && '/' &&  ls_pa0001-begda+4(2) && '/' && ls_pa0001-begda(4).
          lv_empregado = ls_pa0001-ename.

          SORT  lt_pa0001 BY endda DESCENDING.
          READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.

          SELECT SINGLE orgtx INTO lv_setor
            FROM t527x
            WHERE orgeh = ls_pa0001-orgeh
              AND sprsl = sy-langu
              AND endda > sy-datum.

          SELECT SINGLE stltx INTO lv_funcao
            FROM t513s
            WHERE stell = ls_pa0001-stell
              AND sprsl = sy-langu
              AND endda > sy-datum.

          CONDENSE lv_funcao.

        ENDIF.

        IF eg_alv-werks IS NOT INITIAL.
          SELECT SINGLE name1 INTO lv_unidade
            FROM t001w
            WHERE werks = eg_alv-werks.
          CONCATENATE eg_alv-werks '-' lv_unidade INTO lv_unidade SEPARATED BY space.
        ENDIF.
        CALL FUNCTION vl_name
          EXPORTING
            p_unidade        = lv_unidade
            p_empregado      = lv_empregado
            p_funcao         = lv_funcao
            p_setor          = lv_setor
            p_dt_admissao    = lv_dt_admissao
          TABLES
            t_dev            = lt_zpms0041
          EXCEPTIONS
            formatting_error = 1
            internal_error   = 2
            send_error       = 3
            user_canceled    = 4
            OTHERS           = 5.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDFORM.                    " Z_CALL_FORM


  FORM qmnum_f4
    CHANGING
      cv_qmnum TYPE qmel-qmnum.

    TYPES: BEGIN OF ty_qmnum,
             qmnum TYPE zpmt0040-qmnum,
           END OF ty_qmnum.

    DATA:
      lt_values TYPE STANDARD TABLE OF ty_qmnum,
      lt_return TYPE STANDARD TABLE OF ddshretval
      .

    SELECT qmnum                                        "#EC CI_NOWHERE
      FROM zpmt0040
      INTO TABLE lt_values.

    SORT lt_values BY qmnum.

    DELETE ADJACENT DUPLICATES FROM lt_values COMPARING qmnum.
    DELETE lt_values WHERE qmnum IS INITIAL.

    IF lt_values[] IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'QMNUM'    " Name of field in VALUE_TAB
        value_org       = 'S'        " Value return: C: cell by cell, S: structured
      TABLES
        value_tab       = lt_values  " Table of values: entries cell by cell
        return_tab      = lt_return  " Return the selected value
      EXCEPTIONS
        parameter_error = 1          " Incorrect parameter
        no_values_found = 2          " No values found
        OTHERS          = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    cv_qmnum = ls_return-fieldval.

  ENDFORM.


  FORM pernr_f4
    CHANGING
      cv_pernr TYPE zpmt0040-pernr.

    TYPES: BEGIN OF ty_pernr,
             pernr TYPE zpmt0040-pernr,
           END OF ty_pernr.

    DATA:
      lt_values TYPE STANDARD TABLE OF ty_pernr,
      lt_return TYPE STANDARD TABLE OF ddshretval.

**********************************************************************"146631 CS2024000618 MEL. EMPRESTIMO FERRAMENTA PSA
*    select PERNR,
*       ENAME into table @data(LT_PNR)
*    from PA0001
*     where ENDDA >= @SY-DATUM.
*
*    sort LT_PNR by ENAME.
*
*    delete adjacent duplicates from LT_PNR comparing ENAME.
    SELECT DISTINCT
    a~pernr,a~cname,a~cpf_nr,
    CASE WHEN b~matricula IS NOT INITIAL THEN 'X' ELSE ' ' END AS biometria,
    CASE WHEN c~matricula IS NOT INITIAL THEN 'X' ELSE ' ' END AS senha

     FROM zhcmt0007 AS a
    LEFT JOIN zmmt0088 AS b ON a~pernr = b~matricula
    LEFT JOIN zmmt0120 AS c ON a~pernr = c~matricula
    WHERE a~fdate = '00000000'
    INTO TABLE @DATA(lt_pnr).


    SORT lt_pnr BY cname.
**********************************************************************
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'PERNR'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'CV_PERNR'
        value_org       = 'S'
      TABLES
        value_tab       = lt_pnr
        return_tab      = lt_return  " Return the selected value
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.


    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    cv_pernr = ls_return-fieldval.
* - US - 77011 - Inicio - CBRAND
*    TYPES: BEGIN OF ty_pernr,
*             pernr TYPE zpmt0040-pernr,
*           END OF ty_pernr.
*
*    DATA:
*      lt_values TYPE STANDARD TABLE OF ty_pernr,
*      lt_return TYPE STANDARD TABLE OF ddshretval
*      .
*
*    SELECT pernr                                        "#EC CI_NOWHERE
*      FROM zpmt0040
*      INTO TABLE lt_values.
*
*    SORT lt_values BY pernr.
*
*    DELETE ADJACENT DUPLICATES FROM lt_values COMPARING pernr.
*    DELETE lt_values WHERE pernr IS INITIAL.
*
*    IF lt_values[] IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*      EXPORTING
*        retfield        = 'PERNR'    " Name of field in VALUE_TAB
*        value_org       = 'S'        " Value return: C: cell by cell, S: structured
*      TABLES
*        value_tab       = lt_values  " Table of values: entries cell by cell
*        return_tab      = lt_return  " Return the selected value
*      EXCEPTIONS
*        parameter_error = 1          " Incorrect parameter
*        no_values_found = 2          " No values found
*        OTHERS          = 3.
*
*    IF sy-subrc NE 0.
*      RETURN.
*    ENDIF.
*
*    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
*    cv_pernr = ls_return-fieldval.
* - US - 77011 - Fim - CBRAND
  ENDFORM.


  FORM matnr_f4
    CHANGING
      cv_matnr TYPE zpmt0040-matnr.

    TYPES: BEGIN OF ty_matnr,
             matnr TYPE zpmt0040-matnr,
           END OF ty_matnr.

    DATA:
      lt_values TYPE STANDARD TABLE OF ty_matnr,
      lt_return TYPE STANDARD TABLE OF ddshretval
      .

    SELECT matnr                                        "#EC CI_NOWHERE
      FROM zpmt0040
      INTO TABLE lt_values.

    SORT lt_values BY matnr.

    DELETE ADJACENT DUPLICATES FROM lt_values COMPARING matnr.
    DELETE lt_values WHERE matnr IS INITIAL.

    IF lt_values[] IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'    " Name of field in VALUE_TAB
        value_org       = 'S'        " Value return: C: cell by cell, S: structured
      TABLES
        value_tab       = lt_values  " Table of values: entries cell by cell
        return_tab      = lt_return  " Return the selected value
      EXCEPTIONS
        parameter_error = 1          " Incorrect parameter
        no_values_found = 2          " No values found
        OTHERS          = 3.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
    cv_matnr = ls_return-fieldval.

  ENDFORM.
