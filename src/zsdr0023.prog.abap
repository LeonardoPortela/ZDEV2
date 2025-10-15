*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/02/2013                                              &*
*& Descrição: Cadastro de tipo de venda                               &*
*& Transação: zsdt0064                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         03.08.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  zsdr0023.

INCLUDE <icon>.
INCLUDE <cl_alv_control>.
TYPE-POOLS: vrm, ustyp, slis, f4typ .

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_esq_calc,
         nivel     TYPE zsdt0058-nivel,
         cod_fp    TYPE zsdt0058-cod_fp,
         bezei     TYPE zsdt0056-bezei,
         tipo_calc TYPE zsdt0070-tipo_calc,
*        TP_COD_FP TYPE ZSDT0056-TP_COD_FP,
         formula   TYPE zsdt0058-formula,
         style     TYPE lvc_t_styl,
         field     TYPE zsdt0058-field,
       END OF ty_esq_calc,

       BEGIN OF ty_aba,
         aba     TYPE zsdt0086-aba,
         campo   TYPE zsdt0086-campo,
         mark(1),
       END OF ty_aba,

       BEGIN OF ty_header.
         INCLUDE TYPE zsdt0057.
TYPES: desc_esq TYPE zsdt0058-bezei,
       END OF ty_header,

       BEGIN OF ty_0053,
         mandt          TYPE char1,
         nro_sol_ov     TYPE char1,
         posnr          TYPE char1,
         fixacao        TYPE char1,
         auart          TYPE char1,
         matnr          TYPE char1,
         werks          TYPE char1,
         ponto_c        TYPE char1,
         terminal       TYPE char1,
         lgort          TYPE char1,
         charg          TYPE char1,
         zmeng          TYPE char1,
         zieme          TYPE char1,
         dmbtr          TYPE char1,
         pmein          TYPE char1,
         brgew          TYPE char1,
         kursf          TYPE char1,
         volum          TYPE char1,
         voleh          TYPE char1,
         vlrtot         TYPE char1,
         valdt          TYPE char1,
         vbeln          TYPE char1,
         item_edit      TYPE char1,
         kunnr          TYPE char1,
         status         TYPE char1,
         status_itm     TYPE char1,
         contrato       TYPE char1,
         classificacao  TYPE char1,
         usnam          TYPE char1,
         data_atual     TYPE char1,
         hora_atual     TYPE char1,
         navio          TYPE char1,
         porto          TYPE char1,
         p_porto        TYPE char1,
         vlt_porto      TYPE char1,
         instrucao      TYPE char1,
         instrucao_ant  TYPE char1,
         doc_precedente TYPE char1,
         data_lib_frame TYPE char1,
         docnum_rt      TYPE char1,
         remessa_exp    TYPE char1,
         desc_absoluto  TYPE char1,
         kvgr3          TYPE char1,
         numero_ruc     TYPE char1,
         tp_ato         TYPE char1, "#113631-18.01.2024-JT-inicio
         nr_drawback    TYPE char1, "#113631-18.01.2024-JT-inicio
         qtd_drawback   TYPE char1, "#113631-18.01.2024-JT-inicio
       END OF ty_0053.

TYPES: BEGIN OF t_cursor,              "Typ für Cursor-Position
         fname LIKE d021s-fnam,        "Feldname
         pos   LIKE sy-stepl,            "Loop-Zeile auf akt. Seite
         value LIKE d021s-fnam,        "Inhalt des Dynprofeldes
         "Zusatzinfo für Cursor-Position in der Liste
         tc    LIKE dd04l-rollname,       "Table-Control-Name (tc+)
         tcsec LIKE dd04l-rollname,    "TC-Zusatzattr.name (tc+_sec)
         line  LIKE sy-stepl,           "Zeile in ITAB
       END OF t_cursor.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar    DEFINITION DEFERRED.
*CLASS:      lcl_alv_toolbar2   DEFINITION DEFERRED.
*CLASS:      lcl_alv_toolbar3   DEFINITION DEFERRED.

CONSTANTS: BEGIN OF c_tab_strip,
             tab0 LIKE sy-ucomm VALUE 'TAB_STRIP_FC0',
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAB_STRIP_FC5',
             tab6 LIKE sy-ucomm VALUE 'TAB_STRIP_FC6',
             tab7 LIKE sy-ucomm VALUE 'TAB_STRIP_FC7',
             tab8 LIKE sy-ucomm VALUE 'TAB_STRIP_FC8',
             tab9 LIKE sy-ucomm VALUE 'TAB_STRIP_FC9',
           END OF c_tab_strip.

CONTROLS:  tab_strip TYPE TABSTRIP.

DATA: BEGIN OF g_tab_strip,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0023',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip-tab1,
      END OF g_tab_strip.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_custom_container   TYPE REF TO cl_gui_custom_container,
      container1           TYPE REF TO cl_gui_custom_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,

      container2           TYPE REF TO cl_gui_custom_container,
      grid2                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar2         TYPE REF TO lcl_alv_toolbar,

      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x               TYPE c VALUE 'X',
           c_c               TYPE c VALUE 'C',
           c_r               TYPE c VALUE 'R',
           c_f               TYPE c VALUE 'F',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_obs(3)          TYPE c VALUE 'OBS',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_back(4)         TYPE c VALUE 'BACK',
           c_copy(4)         TYPE c VALUE 'COPY',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_bloq(4)         TYPE c VALUE 'BLOQ',
           c_param(5)        TYPE c VALUE 'PARAM',
           c_campo(5)        TYPE c VALUE 'CAMPO',
*           c_gerar(5)    type c value 'GERAR',
           c_atual(5)        TYPE c VALUE 'ATUAL',
*           c_print(5)    type c value 'PRINT',
           c_aprov(5)        TYPE c VALUE 'APROV',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_logist(6)       TYPE c VALUE 'LOGIST',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_dp_click(8)     TYPE c VALUE 'DP_CLICK',
           c_pf_adiant(9)    TYPE c VALUE 'PF_ADIANT',
           c_pf_totmov(9)    TYPE c VALUE 'PF_TOTMOV',
           c_pf_avencer(10)  TYPE c VALUE 'PF_AVENCER',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE',
           c_pf_vencidas(11) TYPE c VALUE 'PF_VENCIDAS'.
*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
*** Declaracoes referente a logica do programa ***
DATA: wg_header         TYPE ty_header, "  ZSDT0057,
      wa_0053           TYPE ty_0053,
*      WA_0053           TYPE ZSDT0053,
      it_0235           TYPE TABLE OF zsdt0235,
      it_tabela         TYPE TABLE OF zsdt0235,
      it_0235_save      TYPE TABLE OF zsdt0235,
      tg_esq_calc       TYPE TABLE OF ty_esq_calc WITH HEADER LINE,
      tg_esq_calc_aux   TYPE TABLE OF ty_esq_calc WITH HEADER LINE,
      tg_aba            TYPE TABLE OF ty_aba      WITH HEADER LINE,
      tg_texto          TYPE catsxt_longtext_itab,
      wg_texto          TYPE LINE OF catsxt_longtext_itab,
      wa_style          TYPE lvc_s_styl,
      style             TYPE lvc_t_styl WITH HEADER LINE,
      wg_desc_kunnr(47),
      wg_desc_auart(20),
      wg_desc_vkorg(20),
      wg_desc_vtweg(20),
      wg_desc_spart(27),
      wg_desc_vkgrp(27),
      wg_desc_vkbur(33),
      wg_desc_zlsch(23),
      wg_desc_zterm(23),
      wg_ucomm          TYPE sy-ucomm. " 28.08.2024 - 150033 - RAMON

*** Declaracoes referente ao template do programa ***
DATA: ok-code         TYPE sy-ucomm,
      wg_display,
      wg_acao(10),
      wg_flag,
      init,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell.

***** Funcao de Z_DOC_CHECK_NEW
DATA: x_field(30),
      wg_mensagem(30).
DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_cell    TYPE lvc_s_cell,
      tg_cell    TYPE lvc_t_cell.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl.


DATA: str   TYPE REF TO data,
      campo TYPE char20.
FIELD-SYMBOLS: <campo>  TYPE any.

* Cadastro de campos obrigatorios
DATA: BEGIN OF inttab OCCURS 100.
        INCLUDE STRUCTURE dfies.
DATA: END OF inttab.

DATA: BEGIN OF dynp_id,
        prog LIKE d020s-prog,
        dnum LIKE d020s-dnum,
      END OF dynp_id.

DATA: h LIKE d020s.
DATA: f LIKE d021s OCCURS 0 WITH HEADER LINE.
DATA: e LIKE d022s OCCURS 0 WITH HEADER LINE.
DATA: m LIKE d023s OCCURS 0 WITH HEADER LINE.

DATA: t_work   TYPE REF TO data,
      wa_dd04t TYPE dd04t,
      wl_f     LIKE LINE OF f.

FIELD-SYMBOLS: <t_itens>    TYPE STANDARD TABLE,
               <w_itens>    TYPE any,
               <w_header>   TYPE any,
               <fs_campo>   TYPE any,
               <fs_tabname> TYPE any.

TYPES : BEGIN OF ty_aba2,
          aba   TYPE zsdt0086-aba,
          campo TYPE zsdt0086-campo,
        END OF ty_aba2,

        BEGIN OF ty_campo,
          campo TYPE zsdt0086-campo,
          descr TYPE zfit0036-observacao,
        END OF ty_campo.
*****************************************************

DATA:      ok_code LIKE sy-ucomm.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid VALUE sy-repid.
*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
CALL SCREEN 100.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command_esq_calc FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,


      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor
  METHOD on_toolbar.
    DATA: wl_desactive.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA: tl_aba_aux LIKE TABLE OF tg_aba,
          wl_aba     LIKE LINE OF tg_aba,
          wl_lines   TYPE sy-tabix.
    REFRESH: tl_aba_aux.

    IF wg_header-tp_venda IS NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
          tl_aba_aux[] = tg_aba[].
          REFRESH: tg_aba.
          LOOP AT tl_aba_aux INTO wl_aba.
            APPEND wl_aba TO tg_aba.
          ENDLOOP.
          CLEAR: wl_aba.
          APPEND wl_aba TO tg_aba.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid2->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_aba INDEX wg_selectedcell-row_id-index.
          ENDLOOP.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command_esq_calc.
    DATA: tl_esq_calc_aux LIKE TABLE OF tg_esq_calc,
          wl_esq_calc     LIKE LINE OF tg_esq_calc,
          wl_lines        TYPE sy-tabix.
    REFRESH: tl_esq_calc_aux.
*   User Command Botões Incluidos
*    break abap.
    CASE e_ucomm.
      WHEN c_add.
        tl_esq_calc_aux[] = tg_esq_calc[].
        REFRESH: tg_esq_calc.
        LOOP AT tl_esq_calc_aux INTO wl_esq_calc.
          wl_esq_calc-nivel = sy-tabix.
          APPEND wl_esq_calc TO tg_esq_calc.
        ENDLOOP.
        DESCRIBE TABLE tg_esq_calc LINES wl_lines.
        CLEAR: wl_esq_calc.
        REFRESH: style, tg_esq_calc-style.
        wl_esq_calc-nivel = ( wl_lines + 1 ).

        wa_style-fieldname = 'FORMULA'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        wa_style-fieldname = 'FIELD'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_esq_calc-style.

        APPEND wl_esq_calc TO tg_esq_calc.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_del.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE tg_esq_calc INDEX wg_selectedcell-row_id-index.

        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.
*  ENDIF.
  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: formula TYPE emg_trdata.
    DATA: p_titulo TYPE sytitle VALUE 'Editar Fórmula'.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_cont  TYPE sy-tabix,
          wl_mod   TYPE sy-tabix,
          wl_pos   TYPE sy-tabix,
          wl_line  TYPE sy-tabix.




    CALL METHOD grid1->get_selected_cells
      IMPORTING
        et_cell = tg_selectedcell.

    LOOP AT tg_selectedcell INTO wg_selectedcell.
      READ TABLE tg_esq_calc INTO  tg_esq_calc INDEX wg_selectedcell-row_id-index.
      IF tg_esq_calc-tipo_calc EQ 'R'.
        formula = tg_esq_calc-formula.
        CALL FUNCTION 'ZSDMF009_INTERPRETA_COD_CALC'
          EXPORTING
            i_formula             = formula
            i_show                = 'X'
          EXCEPTIONS
            formula_inconsistente = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

                                                            "US: 30472
      IF tg_esq_calc-tipo_calc EQ 'C'.
        CLEAR: wl_texto, tl_texto[].
        wl_cont = strlen( tg_esq_calc-formula ).

        WHILE wl_pos < wl_cont.
          wl_line = wl_cont - wl_pos.
          IF wl_line >= 72.
            wl_line = 72.
          ENDIF.

          wl_texto = tg_esq_calc-formula+wl_pos(wl_line).
          ADD 72 TO wl_pos.
          IF wl_texto IS NOT INITIAL.
            APPEND wl_texto TO tl_texto.
          ENDIF.
          CLEAR: wl_texto.
        ENDWHILE.

        " 22.10.2024 - 154003 - RAMON -->

        DATA lt_text_aux TYPE TABLE OF txw_note.
        DATA lv_edit TYPE c.

        IF wg_acao = 'MODIF'.
          lv_edit = abap_true.
        ENDIF.

        LOOP AT tl_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).

          APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).

          <fs_aux>-line = <fs_texto>.

        ENDLOOP.

        CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
          EXPORTING
            edit_mode = lv_edit
          TABLES
            t_txwnote = lt_text_aux[].

* INICIO - IR237465 - 2000043090 - 16/05/2025 - STEFANINI
        IF lv_edit EQ abap_true.
          CLEAR tl_texto.
        ENDIF.
* FIM - IR237465 - 2000043090 - 16/05/2025 - STEFANINI

****        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
****          EXPORTING
****            im_title = p_titulo
*****           IM_DISPLAY_MODE  =
****          CHANGING
****            ch_text  = tl_texto.
        LOOP AT lt_text_aux ASSIGNING <fs_aux>.

          APPEND INITIAL LINE TO tl_texto ASSIGNING <fs_texto>.

          <fs_texto> = <fs_aux>-line.

        ENDLOOP.
        " 22.10.2024 - 154003 - RAMON --<

        CLEAR: tg_esq_calc-formula.
        LOOP AT tl_texto INTO wl_texto.
          IF sy-tabix EQ 1.
            tg_esq_calc-formula = wl_texto.
          ELSEIF sy-tabix GT 1.
            CONCATENATE tg_esq_calc-formula wl_texto INTO tg_esq_calc-formula SEPARATED BY space.
          ENDIF.
        ENDLOOP.

        MODIFY tg_esq_calc FROM tg_esq_calc INDEX wg_selectedcell-row_id-index.
      ENDIF.
*     <"fim 30472

    ENDLOOP.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD on_data_changed.
    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          vl_tabix     TYPE sy-tabix,
          vl_value     TYPE lvc_value,
          lv_value_aux TYPE lvc_value,
          wl_mara      TYPE mara,
          wl_0056      TYPE zsdt0056,
          wl_0070      TYPE zsdt0070.

    CLEAR: lv_value_aux.

    LOOP AT er_data_changed->mt_mod_cells
                             INTO ls_good
                             WHERE fieldname = 'COD_FP'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
*
      SELECT SINGLE *
        FROM zsdt0056
        INTO wl_0056 "LV_VALUE
         WHERE cod_fp EQ lv_value.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BEZEI'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TIPO_CALC'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TP_COD_FP'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'FIELD'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'FORMULA'
            i_value     = lv_value.

      ENDIF.


      lv_value = wl_0056-bezei.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BEZEI'
          i_value     = lv_value.

*      LV_VALUE = WL_0056-TP_COD_FP.
*      CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*        EXPORTING
*          I_ROW_ID    = LS_GOOD-ROW_ID
*          I_FIELDNAME = 'TP_COD_FP'
*          I_VALUE     = LV_VALUE.
*
*      IF  WL_0056-TP_COD_FP NE C_C.
*        LV_VALUE = WL_0056-TIPO_CALC.
*      ELSE.
      CLEAR lv_value.
*      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TIPO_CALC'
          i_value     = lv_value.


      CLEAR: lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'FORMULA'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_mod_cells
                             INTO ls_good
                             WHERE fieldname = 'FIELD'.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'COD_FP'
        IMPORTING
          e_value     = lv_value_aux.
      CONDENSE lv_value_aux NO-GAPS.
      wl_0070-cod_fp = lv_value_aux.

      lv_value = ls_good-value.
*      CONDENSE LV_VALUE NO-GAPS.
      SELECT SINGLE *
        FROM zsdt0070
        INTO wl_0070 "LV_VALUE
         WHERE cod_fp EQ wl_0070-cod_fp
           AND field  EQ lv_value.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: wl_0070.
      ENDIF.
      CLEAR: lv_value.
      lv_value = wl_0070-tipo_calc.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TIPO_CALC'
          i_value     = lv_value.

      CLEAR: lv_value.
      lv_value = wl_0070-field.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'FIELD'
          i_value     = lv_value.

      CLEAR: lv_value.
      IF wl_0070-tipo_calc EQ c_f.
        lv_value = wl_0070-vlr_fixo.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'FORMULA'
          i_value     = lv_value.

    ENDLOOP.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DATA_CHANGED


  METHOD on_data_changed2.

  ENDMETHOD.                    "ON_DATA_CHANGED2


  METHOD on_data_changed_finished2.


  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_f4.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.
    DATA: ls_modi TYPE lvc_s_modi.


    DATA: wl_return TYPE  ddshretval,
          wl_dselc  TYPE  dselc,

          tl_aba    TYPE TABLE OF ty_aba2,
          wl_aba    TYPE ty_aba2,

          tl_campo  TYPE TABLE OF ty_campo,
          wl_campo  TYPE ty_campo,

          tl_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc,

          wg_aba    LIKE LINE OF tg_aba.

    "ABAS do ZSDT0062
    REFRESH: tl_aba, tl_campo.

    wl_aba-aba = 'DADOS GERAIS'.
    wl_aba-campo = 'GERAL'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'CONDIÇÃO DE PAGAMENTO'.
    wl_aba-campo = ''.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'PREÇO'.
    wl_aba-campo = 'PRECO'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'PRODUTO/QUANTIDADE'.
    wl_aba-campo = 'ITENS'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'PAGAMENTO ANTECIPADO'.
    wl_aba-campo = 'PGT_ANT'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'LOGISTICA'.
    wl_aba-campo = 'LOGISTICA'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'ADIANTAMENTO EXTERNO'.
    wl_aba-campo = 'ADTO_EXT'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'INSTRUÇÃO'.
    wl_aba-campo = 'INSTRUCAO'.
    APPEND wl_aba TO tl_aba.

    wl_aba-aba = 'FORMAÇÃO DE LOTE'.
    wl_aba-campo = 'FORM_LOTE'.
    APPEND wl_aba TO tl_aba.

*    WL_ABA-ABA = 'BOTÃO OBSERVAÇÃO'.
*    WL_ABA-CAMPO = 'BTN_OBS'.
*    APPEND WL_ABA TO TL_ABA.
*
*    WL_ABA-ABA = 'BOTÃO LOGISTISTICA'.
*    WL_ABA-CAMPO = 'BTN_LOG'.
*    APPEND WL_ABA TO TL_ABA.

    IF e_fieldname = 'CAMPO'.
      READ TABLE tg_aba INTO wg_aba INDEX es_row_no-row_id.
      IF wg_aba-aba = 'DADOS GERAIS'.
        dynp_id-prog = 'ZSDR0022'.
        dynp_id-dnum = '0050'.
        IMPORT DYNPRO h f e m ID dynp_id.
        LOOP AT f INTO wl_f.
          IF wl_f-fnam+0(9) =  'WG_HEADER' AND wl_f-ityp = '0'.
            wl_campo-campo = wl_f-fnam.
            wl_campo-descr = wl_f-stxt.
            APPEND wl_campo TO tl_campo.
          ELSEIF wl_f-fnam = 'WG_HEADER-OBSERVACAO'.
            wl_campo-campo = wl_f-fnam.
            wl_campo-descr = 'Observação'.
            APPEND wl_campo TO tl_campo.
          ELSEIF wl_f-fnam = 'WG_HEADER-COMENT_LOGISTICA'.
            wl_campo-campo = wl_f-fnam.
            wl_campo-descr = 'Cmt. Logística'.
            APPEND wl_campo TO tl_campo.
          ENDIF.
        ENDLOOP.
      ELSEIF wg_aba-aba = 'CONDIÇÃO DE PAGAMENTO'.
        dynp_id-prog = 'ZSDR0022'.
        dynp_id-dnum = '0101'.
        IMPORT DYNPRO h f e m ID dynp_id.
        LOOP AT f INTO wl_f.
          IF wl_f-fnam+0(11) =  'WG_COND_PGT' AND wl_f-ityp = '0'.
            wl_campo-campo = wl_f-fnam.
            wl_campo-descr = wl_f-stxt.
            APPEND wl_campo TO tl_campo.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE tl_aba INTO wl_aba WITH KEY aba = wg_aba-aba.
        PERFORM listar_grid TABLES tl_campo USING wl_aba-campo wl_campo .
      ENDIF.
    ENDIF.

    CASE e_fieldname.
      WHEN 'ABA'.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'ABA'
            value_org       = 'S'
          TABLES
            value_tab       = tl_aba
            return_tab      = tl_return
            dynpfld_mapping = tl_dselc.

        READ TABLE tl_return INTO wl_return INDEX 1.
        IF sy-subrc = 0 AND wl_return-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'ABA'.
          ls_modi-value     = wl_return-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.
      WHEN 'CAMPO'.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CAMPO'
            value_org       = 'S'
          TABLES
            value_tab       = tl_campo
            return_tab      = tl_return
            dynpfld_mapping = tl_dselc.

        READ TABLE tl_return INTO wl_return INDEX 1.
        IF sy-subrc = 0 AND wl_return-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'CAMPO'.
          ls_modi-value     = wl_return-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.
    ENDCASE.



  ENDMETHOD. "on_f4



  METHOD on_data_changed_finished.
    DATA: wl_esq_calc LIKE LINE OF tg_esq_calc,
          tl_0056     TYPE TABLE OF zsdt0056,
          tl_0070     TYPE TABLE OF zsdt0070,
          wl_0056     TYPE zsdt0056,
          wl_0070     TYPE zsdt0070,
          ls_good     TYPE lvc_s_modi,
          p_2(13)     TYPE p DECIMALS 2,
          p_4(13)     TYPE p DECIMALS 4.

    CLEAR: wl_0056, wl_esq_calc, wl_0070.
    REFRESH: tl_0056, style, tl_0070.

    IF tg_esq_calc[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0056
        INTO TABLE tl_0056
         FOR ALL ENTRIES IN tg_esq_calc
         WHERE cod_fp EQ tg_esq_calc-cod_fp.

      SELECT *
        FROM zsdt0070
        INTO TABLE tl_0070
         FOR ALL ENTRIES IN tg_esq_calc
          WHERE cod_fp EQ tg_esq_calc-cod_fp
            AND field  EQ tg_esq_calc-field.


    ENDIF.

    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_esq_calc INTO wl_esq_calc INDEX ls_good-row_id.
      READ TABLE tl_0056 INTO wl_0056
          WITH KEY cod_fp = wl_esq_calc-cod_fp.
      IF sy-subrc IS INITIAL.
        MOVE: wl_0056-bezei     TO wl_esq_calc-bezei.

*        IF WL_0056-TP_COD_FP NE C_C.
*          MOVE: WL_0056-TIPO_CALC TO WL_ESQ_CALC-TIPO_CALC.
*
*          IF WL_ESQ_CALC-TIPO_CALC EQ C_F.
*            IF WL_0056-C_DECIMAIS EQ 2.
*              P_2 = WL_0056-VLR_FIXO.
*              WRITE P_2 TO WL_ESQ_CALC-FORMULA.
*            ELSEIF WL_0056-C_DECIMAIS EQ 4.
*              P_4 = WL_0056-VLR_FIXO.
*              WRITE P_4 TO WL_ESQ_CALC-FORMULA.
*            ENDIF.
*          ENDIF.
*        ELSE.
        READ TABLE tl_0070 INTO wl_0070
          WITH KEY cod_fp = wl_esq_calc-cod_fp
                   field  = wl_esq_calc-field.
        IF sy-subrc IS INITIAL.
          MOVE: wl_0070-tipo_calc TO wl_esq_calc-tipo_calc.

          IF wl_esq_calc-tipo_calc EQ c_f.
            IF wl_0070-c_decimais EQ 2.
              p_2 = wl_0070-vlr_fixo.
              WRITE p_2 TO wl_esq_calc-formula.
            ELSEIF wl_0070-c_decimais EQ 4.
              p_4 = wl_0070-vlr_fixo.
              WRITE p_4 TO wl_esq_calc-formula.
            ENDIF.
          ENDIF.

        ENDIF.
*        ENDIF.
        IF ls_good-fieldname NE 'FORMULA'.
          IF wl_esq_calc-tipo_calc NE c_f.
            CLEAR: wl_esq_calc-formula.
          ENDIF.
        ENDIF.
        CLEAR: wa_style.
        REFRESH: style, wl_esq_calc-style.
        wa_style-fieldname = 'FORMULA'.
        IF wl_esq_calc-tipo_calc EQ c_c
        OR wl_esq_calc-tipo_calc EQ c_r.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'FIELD'.
*        IF WL_ESQ_CALC-TP_COD_FP EQ C_C.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*        ELSE.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*        ENDIF.
        INSERT  wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE wl_esq_calc-style.

        MODIFY tg_esq_calc FROM wl_esq_calc INDEX ls_good-row_id.
        CLEAR: wl_0056.
      ENDIF.
    ENDLOOP.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_data_changed_finisheD
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
*MODULE TAB_STRIP_ACTIVE_TAB_SET OUTPUT.
*  PERFORM VERIFICA_ERROS.
*  TAB_STRIP-ACTIVETAB = G_TAB_STRIP-PRESSED_TAB.
*  CASE G_TAB_STRIP-PRESSED_TAB.
*    WHEN C_TAB_STRIP-TAB1.
*      G_TAB_STRIP-SUBSCREEN = '0101'.
*    WHEN C_TAB_STRIP-TAB2.
*      G_TAB_STRIP-SUBSCREEN = '0102'.
*    WHEN C_TAB_STRIP-TAB3.
*      G_TAB_STRIP-SUBSCREEN = '0103'.
*    WHEN C_TAB_STRIP-TAB4.
*      G_TAB_STRIP-SUBSCREEN = '0104'.
*    WHEN C_TAB_STRIP-TAB5.
*      G_TAB_STRIP-SUBSCREEN = '0105'.
*    WHEN OTHERS.
**&SPWIZARD:      DO NOTHING
*  ENDCASE.
*ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT
*
**&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
**&SPWIZARD: GETS ACTIVE TAB
*MODULE TAB_STRIP_ACTIVE_TAB_GET INPUT.
*  OK_CODE = SY-UCOMM.
*  CASE OK_CODE.
*    WHEN C_TAB_STRIP-TAB1.
*      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB1.
*    WHEN C_TAB_STRIP-TAB2.
*      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB2.
*    WHEN C_TAB_STRIP-TAB3.
*      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB3.
*    WHEN C_TAB_STRIP-TAB4.
*      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB4.
*    WHEN C_TAB_STRIP-TAB5.
*      G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB5.
*    WHEN OTHERS.
**&SPWIZARD:      DO NOTHING
*  ENDCASE.
*ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_tvak     TYPE tvak,
        wl_linha(6),
        tl_0070     TYPE TABLE OF zsdt0070 WITH HEADER LINE.

  REFRESH: tg_msg_ret, tl_0070.
  CLEAR: tg_msg_ret, tl_0070, wl_tvak, wl_linha.

  " 28.08.2024 - 150033 - RAMON -->
  CHECK wg_acao NE c_atual.

****        CASE wg_header-param_espec.

****WHEN 'U'.
  SELECT SINGLE *
    FROM t161
    INTO @DATA(wl_t161)
    WHERE bsart EQ @wg_header-auart.
****WHEN OTHERS.

  IF sy-subrc NE 0.

    SELECT SINGLE *
      FROM tvak
      INTO wl_tvak
       WHERE auart EQ wg_header-auart.

  ENDIF.
****ENDCASE.


  " 28.08.2024 - 150033 - RAMON --<





  IF tg_esq_calc[] IS NOT INITIAL.
    SELECT *
       FROM zsdt0070
       INTO TABLE tl_0070
        FOR ALL ENTRIES IN tg_esq_calc
        WHERE cod_fp EQ tg_esq_calc-cod_fp.

  ENDIF.

*** Desc. Tipo Venda (BEZEI)
  IF wg_header-bezei IS INITIAL.
    MOVE: 'WG_HEADER-BEZEI'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 'Desc. Tipo Venda.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
*  ELSEIF WL_TVKO-VKORG IS INITIAL.
*    MOVE: 'WG_HEADER-VKORG'       TO TG_MSG_RET-FIELD.
*    CONCATENATE 'Organização de Vendas' TEXT-E02 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ENDIF.
*
*** Tipo de Ordem de Venda (AUART)
  IF wg_header-auart IS INITIAL.
    MOVE: 'WG_HEADER-AUART'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 'Tipo de Ordem de Venda.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF ( wl_tvak-auart IS INITIAL ) AND ( wl_t161-bsart IS INITIAL ).
    MOVE: 'WG_HEADER-AUART'       TO tg_msg_ret-field.
    CONCATENATE 'Tipo de Ordem de Venda' TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*** Descrição Esquema de calculo (DESC_ESQ)
  IF wg_header-desc_esq IS INITIAL.
    MOVE: 'WG_HEADER-DESC_ESQ'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 'Descrição Esquema de calculo .' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

**  Quadro - Esquema de calculo >
**** Tabela TG_ESQ_CALC ()
  IF tg_esq_calc[] IS INITIAL.
*    MOVE: C_TAB_STRIP-TAB2          TO TG_MSG_RET-ABA.
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
    CONCATENATE TEXT-e06 'Esquema de Calculo' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

  LOOP AT tg_esq_calc.
    wl_linha = sy-tabix.

    tg_esq_calc_aux[] = tg_esq_calc[].

    DELETE tg_esq_calc_aux
      WHERE cod_fp NE tg_esq_calc-cod_fp
        AND field  NE tg_esq_calc-field.

    SORT tg_esq_calc_aux BY cod_fp field.

    DELETE ADJACENT DUPLICATES FROM      tg_esq_calc_aux
                               COMPARING cod_fp
                                         field.


    IF sy-subrc IS INITIAL.

      LOOP AT tg_esq_calc_aux.
        MOVE: 'COD_FP'                  TO tg_msg_ret-field,
*            C_TAB_STRIP-TAB2          TO TG_MSG_RET-ABA,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.


        CONCATENATE TEXT-e07 'Cod.Cad.item Linha:' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDLOOP.
    ENDIF.

*** Cod.Cad.item (COD_FP)
    IF tg_esq_calc-cod_fp IS INITIAL.
      MOVE: 'COD_FP'                   TO tg_msg_ret-field,
*            C_TAB_STRIP-TAB2          TO TG_MSG_RET-ABA,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 'Cod.Cad.item LINHA:' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tg_esq_calc-cod_fp IS NOT INITIAL.
      READ TABLE tl_0070
        WITH KEY cod_fp = tg_esq_calc-cod_fp.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'COD_FP'                   TO tg_msg_ret-field,
*              C_TAB_STRIP-TAB2          TO TG_MSG_RET-ABA,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE 'Cod.Cad.item' TEXT-e02 'LINHA:' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ELSEIF tl_0070-tipo_calc EQ c_c.
** Formula (FORMULA)
        IF tg_esq_calc-formula IS INITIAL.
          MOVE: 'FORMULA'                   TO tg_msg_ret-field,
*            C_TAB_STRIP-TAB2          TO TG_MSG_RET-ABA,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e01 'Formula LINHA:' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF wg_header-param_espec NE 'M' AND wg_header-param_espec NE 'Z'.

    IF wg_header-redist EQ 'X'.

      CONCATENATE TEXT-e08 'M e Z' INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.

    IF wg_header-lote_obg EQ 'X'.

      CONCATENATE TEXT-e09 'M e Z' INTO  tg_msg_ret-msg SEPARATED BY space.

    ENDIF.

  ENDIF.


  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
*     I_SHOW      = C_X
      i_repid     = sy-repid
*     I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm,

        tl_0056 TYPE TABLE OF zsdt0056 WITH HEADER LINE.

  REFRESH:tl_ucomm, tl_0056.
  CLEAR: tl_ucomm.

  APPEND: VALUE #( ucomm = c_param ) TO tl_ucomm,
          VALUE #( ucomm = c_campo ) TO tl_ucomm.

  IF tg_esq_calc[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0056
      INTO TABLE tl_0056
       FOR ALL ENTRIES IN tg_esq_calc
        WHERE cod_fp EQ tg_esq_calc-cod_fp.

  ENDIF.

  IF wg_acao NE c_add
  AND wg_acao NE c_modif.
    MOVE: c_save TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    IF grid1 IS NOT INITIAL.
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
      LOOP AT tg_esq_calc.
        REFRESH: style, tg_esq_calc-style.
        wa_style-fieldname = 'FORMULA'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'FIELD'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_esq_calc-style.

        MODIFY tg_esq_calc.
      ENDLOOP.
    ENDIF.



  ELSEIF wg_acao EQ c_add
      OR wg_acao EQ c_modif.

    MOVE: c_atual TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    IF grid1 IS NOT INITIAL.
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'COD_FP'
            OR fieldname EQ 'FORMULA'
            OR fieldname EQ 'FIELD'.

        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT tg_esq_calc.
        REFRESH: style, tg_esq_calc-style.
        wa_style-fieldname = 'FORMULA'.
        IF tg_esq_calc-tipo_calc EQ c_c
        OR tg_esq_calc-tipo_calc EQ c_r.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'FIELD'.
*        IF TG_ESQ_CALC-TP_COD_FP EQ C_C.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*        ELSE.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*        ENDIF.
        INSERT  wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_esq_calc-style.

        MODIFY tg_esq_calc.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF wg_header-bezei IS NOT INITIAL.
    DELETE tl_ucomm WHERE ucomm = c_param.
    DELETE tl_ucomm WHERE ucomm = c_campo.
  ENDIF.

  CALL METHOD cl_gui_cfw=>dispatch.
  SET PF-STATUS 'Z001' EXCLUDING tl_ucomm.
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*  CALL METHOD GRID1->CHECK_CHANGED_DATA.
  CASE sy-ucomm.
    WHEN 'CAMPO'.
      CALL SCREEN 0200 STARTING AT 060 2
             ENDING   AT 125 18.
    WHEN c_add.
      IF wg_flag IS INITIAL.
        PERFORM limpa_variavel USING sy-ucomm.
        PERFORM get_next_number  USING  'ZTP_VENDA'
                                        '01'
                               CHANGING wg_header-tp_venda.

        PERFORM get_next_number  USING  'ZESQ_CALC'
                                        '01'
                               CHANGING wg_header-esq_calc.

*        MOVE: sy-datum TO wg_header-erdat,
*              sy-uzeit TO wg_header-erzet.
      ENDIF.
      MOVE c_add TO wg_acao.
    WHEN c_atual.
      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      MOVE: c_atual TO wg_acao.
    WHEN c_search.
      PERFORM busca_dados.

    WHEN c_copy.
**    Valida se existe documento para ser modificado.
*      SELECT SINGLE doc_simulacao
*        FROM zsdt0040
*        INTO wg_header-doc_simulacao
*         WHERE doc_simulacao EQ wg_header-doc_simulacao.

      IF sy-subrc IS INITIAL.
        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.
*        PERFORM get_next_number  USING  'ZSIMULACAO'
*                                       '1'
*                              CHANGING wg_header-doc_simulacao.

*        MOVE: sy-datum TO wg_header-erdat,
*              sy-uzeit TO wg_header-erzet.

        MOVE c_modif TO wg_acao.
      ENDIF.
*    WHEN c_descp.
*      PERFORM inputa_desc.
    WHEN c_modif.
**    Valida se existe documento para ser modificado.
      SELECT SINGLE tp_venda
        FROM zsdt0057
        INTO wg_header-tp_venda
         WHERE tp_venda EQ wg_header-tp_venda.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'ENQUEUE_EZSDT0057'
          EXPORTING
            tp_venda       = wg_header-tp_venda
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.

        MOVE c_modif TO wg_acao.

      ENDIF.

    WHEN c_cancel.
      CALL FUNCTION 'DEQUEUE_EZSDT0057'
        EXPORTING
          tp_venda = wg_header-tp_venda.
      PERFORM limpa_variavel USING sy-ucomm.
    WHEN c_show_msgre.
      PERFORM verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN c_save.
      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        PERFORM grava_dados.

      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN 'BACK'
      OR 'EXIT'.
      CALL FUNCTION 'DEQUEUE_EZSDT0057'
        EXPORTING
          tp_venda = wg_header-tp_venda.
      LEAVE TO SCREEN 0.

    WHEN 'PARAM' OR 'PARAM1'.
      CALL SCREEN 0300.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE.

  wl_repid = sy-repid.
  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
*     I_SHOW      = 'X'
      i_repid     = sy-repid
      i_popup     = 0
*     I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field = 'X_FIELD'
      i_set_cell  = 'WG_CELL'
      i_set_obj   = 'WG_OBJ'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

** Esquema de calculo ( Esquema de calculo)
  IF container1 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC_01'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command_esq_calc FOR grid1.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

*    lt_f4-fieldname = 'MATNR'.
*    lt_f4-register = 'X' .
*    lt_f4-getbefore = 'X' .
*    append lt_f4 .


    wa_layout-stylefname = 'STYLE'.
    PERFORM montar_layout.
    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_esq_calc[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1,
*              lcl_event_handler=>on_hotspot_click for grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.
*              lcl_event_handler=>on_onf4 FOR grid1.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
        1 'ZSDT0058'  'NIVEL'       'TG_ESQ_CALC' 'NIVEL'          ' ' ' ' ' ' ' ' ' ',
        2 'ZSDT0058'  'COD_FP'      'TG_ESQ_CALC' 'COD_FP'         ' ' ' ' 'X' ' ' ' ',
        3 'ZSDT0056'  'BEZEI'       'TG_ESQ_CALC' 'BEZEI'          ' ' ' ' ' ' ' ' ' ',
        4 'ZSDT0058'  'FIELD'       'TG_ESQ_CALC' 'FIELD'          'Campo' ' ' 'X' ' ' ' ',
        5 'ZSDT0056'  'TIPO_CALC'   'TG_ESQ_CALC' 'TIPO_CALC'      ' ' '7' ' ' ' ' ' ',
        6 ''  'FORMULA'     '' 'FORMULA'        'Fórmula' '255' ' ' ' ' ' '.

*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_layout2.
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
        1 ' '  ' '     'TG_ABA' 'ABA'          'ABA'   '30' 'X' ' ' ' ',
        2 ' '  ' '     'TG_ABA' 'CAMPO'        'CAMPO' '30' 'X' ' ' ' '.
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
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.

  IF p_field = 'ABA' OR p_field = 'CAMPO'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'COD_FP'.
*  OR P_FIELD EQ 'LGORT'
*  OR P_FIELD EQ 'ZIEME'.
*  or p_field eq 'TRUNIT'
*  or p_field eq 'COMPR'.
    w_fieldcatalog-f4availabl = 'X'.
  ENDIF.
*
*  if p_field eq 'AUART'.
*    w_fieldcatalog-drdn_hndl  = 1.
*  elseif p_field eq 'INCO1'.
*    w_fieldcatalog-drdn_hndl  = 2.
*  endif.
*
*  if p_field eq 'VBELN'.
*    w_fieldcatalog-hotspot = c_x.
*  endif.
*  if p_field eq 'MATNR'.
*    w_fieldcatalog-f4availabl = c_x.
*    w_fieldcatalog-edit_mask  = '==MATN1'.
*  endif.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_variavel USING p_acao.
  DATA: wl_tp_venda TYPE zsdt0057-tp_venda.

  CLEAR: wl_tp_venda.

  IF p_acao EQ c_cancel
   OR p_acao EQ c_atual.
    wl_tp_venda = wg_header-tp_venda.
    CLEAR: wg_header, tg_esq_calc, wg_acao, wg_flag, x_field, tg_msg_ret, wg_cell, wg_desc_auart, wa_0053.
    REFRESH: tg_msg_ret, tg_esq_calc.

    wg_header-tp_venda = wl_tp_venda.
  ELSEIF p_acao EQ c_add.
    CLEAR: wg_header, tg_esq_calc, wg_acao, x_field, tg_msg_ret, wg_desc_auart, wa_0053.
    REFRESH: tg_msg_ret, tg_esq_calc.
  ENDIF.

ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  DATA: values   TYPE vrm_values WITH HEADER LINE,
        tl_tvlv  TYPE TABLE OF tvlv WITH HEADER LINE,
        tl_tvlvt TYPE TABLE OF tvlvt WITH HEADER LINE,
        wl_t052  TYPE t052.
  FIELD-SYMBOLS: <fs_campo1> TYPE any.

  REFRESH: tl_tvlv, tl_tvlvt, values.

*  IF INIT IS INITIAL.
*    SELECT *
*      FROM TVLV
*      INTO TABLE TL_TVLV.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT *
*        FROM TVLVT
*         INTO TABLE TL_TVLVT
*         FOR ALL ENTRIES IN TL_TVLV
*          WHERE ABRVW EQ TL_TVLV-ABRVW
*            AND SPRAS EQ SY-LANGU.
*
*    ENDIF.
*
*    LOOP AT TL_TVLV.
*      READ TABLE TL_TVLVT
*        WITH KEY ABRVW = TL_TVLV-ABRVW.
*      IF SY-SUBRC IS INITIAL.
*
*        VALUES-TEXT = TL_TVLVT-BEZEI.
*        VALUES-KEY  = TL_TVLV-ABRVW.
*        APPEND VALUES.
*      ENDIF.
*    ENDLOOP.
*
*    CALL FUNCTION 'VRM_SET_VALUES'
*      EXPORTING
*        ID              = 'WG_HEADER-VKAUS'
*        VALUES          = VALUES[]
*      EXCEPTIONS
*        ID_ILLEGAL_NAME = 1
*        OTHERS          = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    REFRESH: VALUES.
*    CLEAR: VALUES.
*    INIT = C_X.
*  ENDIF.



  IF wg_acao EQ c_add
  OR wg_acao EQ c_modif.
    LOOP AT SCREEN.
      IF screen-group2 EQ 'A1'.
*        IF SCREEN-NAME EQ 'WG_COND_PGT-QTE_VENC'.
*          IF WL_T052-ZTAGG IS NOT INITIAL.
*            SCREEN-INPUT = 1.
*          ELSE.
**            CLEAR: WG_COND_PGT-QTE_VENC.
*            SCREEN-INPUT = 0.
*          ENDIF.
*        ELSE.
        screen-input = 1.
*        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-group1 EQ 'BLK'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name EQ 'WA_0053-NUMERO_RUC'.
      IF wg_header-param_espec EQ 'A' OR "Algodoeira
         wg_header-param_espec EQ 'X' OR  "Algodão Vda Fins Exp
         wg_header-param_espec EQ 'P'.    "Performance
        screen-invisible = 0.
      ELSE.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    " 24.09.2024 - 147331 - RAMON -->
    IF screen-name EQ 'WG_HEADER-REDIST_FIX' AND wg_header-param_espec IS NOT INITIAL.

      IF wg_header-param_espec EQ 'M'.
        screen-input = 1.
      ELSE.
        CLEAR wg_header-redist_fix.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.
    " 24.09.2024 - 147331 - RAMON --<


    "27.05.2025 - #180502 - Renato
    IF screen-name EQ 'WG_HEADER-LOTE_OBG'.

      IF wg_header-param_espec EQ 'M'.
        screen-input = 1.
      ELSE.
        CLEAR wg_header-lote_obg.
        screen-input = 0.
      ENDIF.

      MODIFY SCREEN.

    ENDIF.
    "27.05.2025 - #180502 - fim

  ENDLOOP.

*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
*     I_SHOW      = 'X'
      i_repid     = sy-repid
      i_popup     = 0
*     I_PRESSED_TAB = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field = 'X_FIELD'
*     I_SET_CELL  = 'WG_CELL'
*     I_SET_OBJ   = 'WG_OBJ'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].

    APPEND wg_cell TO tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc NE 0.
    CLEAR: p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração,'
                      'não foi encontrado!'.
  ELSE.
    wg_flag = c_x.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_doc.
  DATA: wl_0057 TYPE zsdt0057,
        tl_0058 TYPE TABLE OF zsdt0058 WITH HEADER LINE,
        tl_0056 TYPE TABLE OF zsdt0056 WITH HEADER LINE,
        tl_0070 TYPE TABLE OF zsdt0070 WITH HEADER LINE.

  CLEAR: wl_0057, tl_0058, tl_0056,tg_aba.
  REFRESH: tl_0056, tl_0058, tl_0070.

  SELECT SINGLE *
    FROM zsdt0057
    INTO wl_0057
     WHERE tp_venda EQ wg_header-tp_venda.

  IF sy-subrc IS INITIAL.
    SELECT  aba campo
      FROM zsdt0086
      INTO TABLE tg_aba
     WHERE tp_venda EQ wg_header-tp_venda.

    SELECT *
      FROM zsdt0235
      INTO TABLE it_0235
      WHERE tp_venda EQ wg_header-tp_venda.

    SELECT *
      FROM zsdt0058
      INTO TABLE tl_0058
       WHERE esq_calc EQ wl_0057-esq_calc.

    IF sy-subrc IS INITIAL.
      SELECT *
      FROM zsdt0056
      INTO TABLE tl_0056
       FOR ALL ENTRIES IN tl_0058
       WHERE cod_fp EQ tl_0058-cod_fp.

      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zsdt0070
          INTO TABLE tl_0070
           FOR ALL ENTRIES IN tl_0056
            WHERE cod_fp EQ tl_0056-cod_fp.

      ENDIF.
    ENDIF.


    PERFORM monta_dados_doc TABLES tl_0058
                                   tl_0056
                                   tl_0070
                            USING  wl_0057.



  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O documento de Tipo de Venda'
                                           'não foi encontrado'.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM monta_dados_doc  TABLES tl_0058 STRUCTURE zsdt0058
                             tl_0056 STRUCTURE zsdt0056
                             tl_0070 STRUCTURE zsdt0070
                      USING  wl_0057 TYPE zsdt0057.

  DATA: p_2(13) TYPE p DECIMALS 2,
        p_4(13) TYPE p DECIMALS 4.

  CLEAR: wg_header, tg_esq_calc.
  REFRESH: tg_esq_calc.


* Header
  MOVE-CORRESPONDING: wl_0057 TO wg_header.

* Esquema de calculo
  LOOP AT tl_0058.
    READ TABLE tl_0056
      WITH KEY cod_fp = tl_0058-cod_fp.

    MOVE-CORRESPONDING: tl_0058 TO tg_esq_calc.
    MOVE: tl_0056-bezei     TO tg_esq_calc-bezei,
*          TL_0056-TP_COD_FP TO TG_ESQ_CALC-TP_COD_FP,
          tl_0058-bezei     TO wg_header-desc_esq.

*    IF TL_0056-TP_COD_FP NE 'C'.
*      MOVE  TL_0056-TIPO_CALC TO TG_ESQ_CALC-TIPO_CALC.
*
*      IF TG_ESQ_CALC-TIPO_CALC EQ C_F.
*        CONDENSE TG_ESQ_CALC-FORMULA NO-GAPS.
*        IF TL_0056-C_DECIMAIS EQ 2.
*          P_2 = TG_ESQ_CALC-FORMULA.
*          WRITE P_2 TO TG_ESQ_CALC-FORMULA.
*        ELSEIF TL_0056-C_DECIMAIS EQ 4.
*          P_4 = TG_ESQ_CALC-FORMULA.
*          WRITE P_4 TO TG_ESQ_CALC-FORMULA.
*        ENDIF.
*      ENDIF.
*    ELSE.
    CLEAR: tl_0070.
    READ TABLE tl_0070
      WITH KEY cod_fp = tl_0056-cod_fp
               field  = tl_0058-field.
    IF sy-subrc IS INITIAL.
      MOVE  tl_0070-tipo_calc TO tg_esq_calc-tipo_calc.

      IF tg_esq_calc-tipo_calc EQ c_f.
        CONDENSE tg_esq_calc-formula NO-GAPS.
        IF tl_0070-c_decimais EQ 2.
          p_2 = tg_esq_calc-formula.
          WRITE p_2 TO tg_esq_calc-formula.
        ELSEIF tl_0070-c_decimais EQ 4.
          p_4 = tg_esq_calc-formula.
          WRITE p_4 TO tg_esq_calc-formula.
        ENDIF.
      ENDIF.
    ENDIF.
*    ENDIF.

    APPEND tg_esq_calc.
    CLEAR: tl_0056.
  ENDLOOP.

  it_tabela = it_0235.
  SORT it_tabela BY tabela.
  DELETE ADJACENT DUPLICATES FROM it_tabela COMPARING tabela.

  LOOP AT it_tabela INTO DATA(wa_tabela).

    ASSIGN wa_tabela-tabela TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA str TYPE (<fs_str>).

    LOOP AT it_0235 INTO DATA(w_0235) WHERE tabela EQ wa_tabela-tabela.
      IF w_0235-tabela EQ 'ZSDT0053'.
        ASSIGN wa_0053 TO FIELD-SYMBOL(<f_0053>).
        ASSIGN COMPONENT w_0235-campo  OF STRUCTURE <f_0053> TO <fs_campo>.
        IF <fs_campo> IS ASSIGNED.
          <fs_campo> = abap_true.
          wa_0053 = <f_0053>.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .

  DATA: wl_input_0057 TYPE zsdt0057,
        wl_0057       TYPE zsdt0057,
        tl_input_0086 TYPE TABLE OF zsdt0086 WITH HEADER LINE,
        tl_input_0058 TYPE TABLE OF zsdt0058 WITH HEADER LINE.

  CLEAR: wl_input_0057, tl_input_0058, wl_0057.

  SELECT SINGLE *
    FROM zsdt0057
    INTO wl_0057
     WHERE tp_venda EQ wg_header-tp_venda.

  IF sy-subrc IS INITIAL.
    MOVE: wl_0057-usnam      TO wl_input_0057-usnam,
          wl_0057-data_atual TO wl_input_0057-data_atual,
          wl_0057-hora_atual TO wl_input_0057-hora_atual.
  ELSE.
    MOVE: sy-uname     TO wl_input_0057-usnam,
          sy-datum     TO wl_input_0057-data_atual,
          sy-uzeit     TO wl_input_0057-hora_atual.
  ENDIF.
* Header
  MOVE: wg_header-tp_venda         TO wl_input_0057-tp_venda,
        wg_header-bezei            TO wl_input_0057-bezei,
        wg_header-param_espec      TO wl_input_0057-param_espec,
        wg_header-auart            TO wl_input_0057-auart,
        wg_header-boleto           TO wl_input_0057-boleto,
        wg_header-esq_calc         TO wl_input_0057-esq_calc,
        wg_header-status           TO wl_input_0057-status,

        " 19.02.2024 - RAMON -->
        wg_header-redist           TO wl_input_0057-redist,
        " 19.02.2024 - RAMON --<

        " 20.09.2024 - 147331 - RAMON -->
        wg_header-redist_fix       TO wl_input_0057-redist_fix,
  " 20.09.2024 - 147331 - RAMON --<

        wg_header-lote_obg TO wl_input_0057-lote_obg. "27.05.2025 - #180502 - Renato


  LOOP AT tg_esq_calc.
* Esquema de calculo
    MOVE-CORRESPONDING: tg_esq_calc TO tl_input_0058.
    MOVE: wl_input_0057-esq_calc    TO tl_input_0058-esq_calc,
          wg_header-desc_esq        TO tl_input_0058-bezei,
          wl_input_0057-usnam       TO tl_input_0058-usnam,
          wl_input_0057-data_atual  TO tl_input_0058-data_atual,
          wl_input_0057-hora_atual  TO tl_input_0058-hora_atual.

    IF tg_esq_calc-tipo_calc EQ c_f.
      TRANSLATE tl_input_0058-formula USING '. '.
      TRANSLATE tl_input_0058-formula USING ',.'.
      CONDENSE tl_input_0058-formula  NO-GAPS.
    ENDIF.

    APPEND tl_input_0058.
    CLEAR tl_input_0058.
  ENDLOOP.

  LOOP AT tg_aba.
    MOVE-CORRESPONDING tg_aba TO tl_input_0086.
    MOVE: wg_header-tp_venda TO tl_input_0086-tp_venda,
          sy-uname           TO tl_input_0086-usnam,
          sy-datum           TO tl_input_0086-data_atual,
          sy-uzeit           TO tl_input_0086-hora_atual.
    APPEND tl_input_0086.
  ENDLOOP.

  DELETE FROM zsdt0057 WHERE tp_venda EQ wl_input_0057-tp_venda.
  DELETE FROM zsdt0086 WHERE tp_venda EQ wl_input_0057-tp_venda.
  DELETE FROM zsdt0058 WHERE esq_calc EQ wl_input_0057-esq_calc.

  MODIFY zsdt0057 FROM wl_input_0057.
  MODIFY zsdt0086 FROM TABLE tl_input_0086.
  MODIFY zsdt0058 FROM TABLE tl_input_0058.
  COMMIT WORK.


  MESSAGE s836(sd) WITH 'Tipo de venda e Esquema de Calculo'
                         wl_input_0057-tp_venda
                         wl_input_0057-esq_calc
                         ', criado/modificado com sucesso!'.

*  call function 'DEQUEUE_EZSDT0040'
*    exporting
*      doc_simulacao = wg_header-doc_simulacao.

  PERFORM limpa_variavel USING c_atual.
  wg_acao = c_atual.

  LEAVE TO SCREEN 100.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  CLEAR: wg_desc_auart.
*             WG_DESC_VKORG,
*             WG_DESC_VTWEG,
*             WG_DESC_SPART,
*             WG_DESC_VKGRP,
*             WG_DESC_VKBUR,
*             WG_DESC_ZLSCH,
*             WG_DESC_ZTERM.
*
*  SELECT SINGLE NAME1
*    FROM KNA1
*    INTO WG_DESC_KUNNR
*     WHERE KUNNR EQ WG_HEADER-KUNNR.

  " 03.09.2024 - 150033 - RAMON -->
****  IF ( wg_header-param_espec EQ 'U' ).

  SELECT SINGLE batxt
    FROM t161t
    INTO wg_desc_auart
    WHERE spras EQ sy-langu
      AND bsart EQ wg_header-auart.

****  ELSE.

  IF wg_desc_auart IS INITIAL.

    SELECT SINGLE bezei
      FROM tvakt
      INTO wg_desc_auart
      WHERE spras EQ sy-langu
        AND auart EQ wg_header-auart.


  ENDIF.


****  ENDIF.
  " 03.09.2024 - 150033 - RAMON --<

*
*  SELECT SINGLE VTEXT
*    FROM TVKOT
*    INTO WG_DESC_VKORG
*     WHERE SPRAS EQ SY-LANGU
*       AND VKORG EQ WG_HEADER-VKORG.
*
*  SELECT SINGLE VTEXT
*    FROM TVTWT
*    INTO WG_DESC_VTWEG
*     WHERE SPRAS EQ SY-LANGU
*       AND VTWEG EQ WG_HEADER-VTWEG.
*
*  SELECT SINGLE VTEXT
*    FROM TSPAT
*    INTO WG_DESC_SPART
*     WHERE SPRAS EQ SY-LANGU
*       AND SPART EQ WG_HEADER-SPART.
*
*  SELECT SINGLE VTEXT
*    FROM TSPAT
*    INTO WG_DESC_SPART
*     WHERE SPRAS EQ SY-LANGU
*       AND SPART EQ WG_HEADER-SPART.
*
*  SELECT SINGLE BEZEI
*    FROM TVGRT
*    INTO WG_DESC_VKGRP
*     WHERE SPRAS EQ SY-LANGU
*       AND VKGRP EQ WG_HEADER-VKGRP.
*
*  SELECT SINGLE BEZEI
*    FROM TVKBT
*    INTO WG_DESC_VKBUR
*     WHERE SPRAS EQ SY-LANGU
*       AND VKBUR EQ WG_HEADER-VKBUR.
*
*  SELECT SINGLE TEXT2
*    FROM T042ZT
*    INTO WG_DESC_ZLSCH
*     WHERE SPRAS EQ SY-LANGU
*       AND LAND1 EQ 'BR'
*       AND ZLSCH EQ WG_COND_PGT-ZLSCH.
*
*  SELECT SINGLE VTEXT
*   FROM TVZBT
*   INTO WG_DESC_ZTERM
*    WHERE SPRAS EQ SY-LANGU
*      AND ZTERM EQ WG_COND_PGT-ZTERM.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM valida_layout  TABLES   tl_fieldcatalog STRUCTURE lvc_s_fcat
                     USING   uname.

  DATA: tl_parametros        TYPE ustyp_t_parameters,
        wl_parametros        TYPE ustyp_parameters,
        wl_fieldcatalog      TYPE lvc_s_fcat,
        wl_variante01        TYPE zvariante01,
        tl_variante02_alv    TYPE TABLE OF zvariante02 WITH HEADER LINE,
        tl_variante02_screen TYPE TABLE OF zvariante02 WITH HEADER LINE,
        wl_tabix             TYPE sy-tabix,
        wl_atributo(30).

  REFRESH: tl_parametros, tl_variante02_alv, tl_variante02_screen.
  FIELD-SYMBOLS: <fs_atributos> TYPE any.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = uname
*     WITH_TEXT           =
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  READ TABLE tl_parametros INTO wl_parametros
    WITH KEY parid = 'ZVARIANTE'.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE grpva EQ wl_parametros-parva
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      CONDENSE wl_variante01-grpva NO-GAPS.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_screen
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip NE 'ALV'
           AND dynnr   EQ sy-dynnr.

    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
    AND ( sy-tcode NE 'SE38'
       AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
          AND tl_variante02_screen-acao EQ wg_acao )
            OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE default_var EQ c_x
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
         FROM zvariante02
         INTO TABLE tl_variante02_screen
          WHERE grpva   EQ wl_variante01-grpva
            AND tcode   EQ sy-tcode
            AND atr_tip NE 'ALV'
            AND dynnr   EQ sy-dynnr.
    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
        AND ( sy-tcode NE 'SE38'
           AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_alv-acao IS NOT INITIAL
            AND tl_variante02_alv-acao EQ wg_acao )
              OR tl_variante02_alv-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z002'.
  SET TITLEBAR 'Z002'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0200 OUTPUT.
*  DATA:   WL_REPID     TYPE SY-REPID,
*            TL_FUNCTION  TYPE UI_FUNCTIONS,
*            WL_FUNCTION  LIKE TL_FUNCTION WITH HEADER LINE,
*            LT_F4        TYPE LVC_T_F4 WITH HEADER LINE.

  wl_repid = sy-repid.


  IF container2 IS INITIAL.
    REFRESH: tl_function, lt_f4.
    CLEAR wa_layout.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'A'.
    "WA_LAYOUT-CWIDTH_OPT   = 'X'.
    wa_layout-box_fname    = 'MARK'.


    CREATE OBJECT container2
      EXPORTING
        container_name = 'CC_ABAS'.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container2.

    CREATE OBJECT obg_toolbar2
      EXPORTING
        io_alv_grid = grid2.

*      * Register event handler
    SET HANDLER obg_toolbar2->on_toolbar FOR grid2.
    SET HANDLER obg_toolbar2->handle_user_command FOR grid2.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    lt_f4-fieldname = 'ABA'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    lt_f4-fieldname = 'CAMPO'.
    lt_f4-register = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
    PERFORM montar_layout2.
    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_aba[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid2->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished2 FOR grid2,
              lcl_event_handler=>on_data_changed2 FOR grid2,
              lcl_event_handler=>on_f4 FOR grid2.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN 'GRAVAR'.
      DATA:         tl_input_0086 TYPE TABLE OF zsdt0086 WITH HEADER LINE.
      LOOP AT tg_aba.
        MOVE-CORRESPONDING tg_aba TO tl_input_0086.
        MOVE: wg_header-tp_venda TO tl_input_0086-tp_venda,
              sy-uname           TO tl_input_0086-usnam,
              sy-datum           TO tl_input_0086-data_atual,
              sy-uzeit           TO tl_input_0086-hora_atual.
        APPEND tl_input_0086.
      ENDLOOP.
      DELETE FROM zsdt0086 WHERE tp_venda EQ wg_header-tp_venda.
      MODIFY zsdt0086 FROM TABLE tl_input_0086.
      SET SCREEN 0.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  LISTAR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_ABA_CAMPO  text
*----------------------------------------------------------------------*
FORM listar_grid  TABLES tl_campo
                  USING  p_wl_aba_campo
                         wl_campo TYPE ty_campo.


  IF p_wl_aba_campo = 'PRECO'.
    PERFORM f_preco(zsdr0022) USING wg_header-tp_venda.
  ELSE.
    PERFORM montar_layout(zsdr0022) USING p_wl_aba_campo.
  ENDIF.

  ASSIGN ('(ZSDR0022)T_FIELDCATALOG[]') TO <t_itens>.

  CREATE DATA t_work LIKE LINE OF <t_itens>.
  ASSIGN t_work->* TO  <w_itens>.

  IF <t_itens> IS ASSIGNED.
    LOOP AT <t_itens> INTO <w_itens>.
      IF p_wl_aba_campo NE 'PRECO'.
        ASSIGN COMPONENT 'EDIT'  OF STRUCTURE <w_itens> TO <fs_campo>.
        IF sy-subrc <> 0.
          EXIT.
        ELSEIF <fs_campo> NE 'X'.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR   inttab.
      ASSIGN COMPONENT 'REF_TABLE'  OF STRUCTURE <w_itens> TO <fs_tabname>.
      IF <fs_tabname> IS NOT INITIAL.
        ASSIGN COMPONENT 'REF_FIELD'  OF STRUCTURE <w_itens> TO <fs_campo>.

        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = <fs_tabname>
            fieldname      = <fs_campo>
            langu          = sy-langu
          TABLES
            dfies_tab      = inttab
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          WRITE:/ 'Field name not found'.
        ENDIF.

        READ TABLE inttab INDEX 1.
      ENDIF.

      ASSIGN COMPONENT 'FIELDNAME'  OF STRUCTURE <w_itens> TO <fs_campo>.
      wl_campo-campo = <fs_campo>.
      ASSIGN COMPONENT 'SCRTEXT_S'  OF STRUCTURE <w_itens> TO <fs_campo>.

      IF <fs_campo> IS NOT INITIAL.
        wl_campo-descr =  <fs_campo>.
      ELSE.
        wl_campo-descr =  inttab-scrtext_l.
      ENDIF.
      APPEND wl_campo TO tl_campo.
    ENDLOOP.

    UNASSIGN: <t_itens>, <w_itens>, <fs_campo>, <fs_tabname>.
  ENDIF.
ENDFORM.                    " LISTAR_GRID


*&---------------------------------------------------------------------*
*&      Module  SEARCH_TIPO  INPUT
*&---------------------------------------------------------------------*
MODULE search_tipo INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE.

  IF ( wg_header-param_espec IS NOT INITIAL ).
    CASE wg_header-param_espec.
      WHEN 'U'.

        SELECT bsart, batxt
          FROM t161t
          INTO TABLE @DATA(tl_t161)
          WHERE spras EQ 'P'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield    = 'BSART'
            dynpprog    = sy-repid                            "'ZFINR018'
            dynpnr      = sy-dynnr
            dynprofield = 'WG_HEADER-AUART'
            value_org   = 'S'
          TABLES
            value_tab   = tl_t161
            return_tab  = tl_return_tab.

      WHEN OTHERS.

        SELECT auart, bezei
          FROM tvakt
          INTO TABLE @DATA(tl_tvak)
          WHERE spras EQ 'P'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield    = 'AUART'
            dynpprog    = sy-repid                            "'ZFINR018'
            dynpnr      = sy-dynnr
            dynprofield = 'WG_HEADER-AUART'
            value_org   = 'S'
          TABLES
            value_tab   = tl_tvak
            return_tab  = tl_return_tab.

    ENDCASE.
  ELSE.
    MESSAGE 'Selecionar primeiro -Parâmetro Especial-' TYPE 'I'.
    EXIT.
  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_PARAM  INPUT
*&---------------------------------------------------------------------*
MODULE modify_screen_param INPUT.

  CHECK ( wg_header-param_espec IS NOT INITIAL ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR 'TI0300'.

  PERFORM processa_dados.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM gravar_dados.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_active_tab_set OUTPUT.

  tab_strip-activetab = g_tab_strip-pressed_tab.

  g_tab_strip-subscreen =
  SWITCH #(
  g_tab_strip-pressed_tab
                         WHEN c_tab_strip-tab0 THEN '0310'
                         WHEN c_tab_strip-tab1 THEN '0301'
                         WHEN c_tab_strip-tab2 THEN '0302'
                         WHEN c_tab_strip-tab3 THEN '0303'
                         WHEN c_tab_strip-tab4 THEN '0304'
                         WHEN c_tab_strip-tab5 THEN '0305'
                         WHEN c_tab_strip-tab6 THEN '0306'
                         WHEN c_tab_strip-tab7 THEN '0307'
                         WHEN c_tab_strip-tab8 THEN '0308'
                         WHEN c_tab_strip-tab9 THEN '0309'
                         ELSE g_tab_strip-pressed_tab
).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_active_tab_get INPUT.

  g_tab_strip-pressed_tab =
  SWITCH #(  sy-ucomm
                      WHEN c_tab_strip-tab0 THEN c_tab_strip-tab0
                      WHEN c_tab_strip-tab1 THEN c_tab_strip-tab1
                      WHEN c_tab_strip-tab2 THEN c_tab_strip-tab2
                      WHEN c_tab_strip-tab3 THEN c_tab_strip-tab3
                      WHEN c_tab_strip-tab4 THEN c_tab_strip-tab4
                      WHEN c_tab_strip-tab5 THEN c_tab_strip-tab5
                      WHEN c_tab_strip-tab6 THEN c_tab_strip-tab6
                      WHEN c_tab_strip-tab7 THEN c_tab_strip-tab7
                      WHEN c_tab_strip-tab8 THEN c_tab_strip-tab8
                      WHEN c_tab_strip-tab9 THEN c_tab_strip-tab9
                      ELSE g_tab_strip-pressed_tab
    ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravar_dados .

  IF it_0235_save NE it_0235.
    DELETE FROM zsdt0235 WHERE tp_venda EQ wg_header-tp_venda.
    MODIFY zsdt0235 FROM TABLE it_0235_save.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  IF sy-ucomm CS 'C53'.

    ASSIGN 'ZSDT0053' TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA str TYPE (<fs_str>).

    DATA(it_campos) = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

    ASSIGN wa_0053 TO FIELD-SYMBOL(<f_0053>).

    LOOP AT it_campos INTO DATA(wa_campos).
      ASSIGN COMPONENT wa_campos-fieldname OF STRUCTURE <f_0053> TO <campo>.

      IF ( sy-subrc EQ 0 ) AND ( <campo> EQ abap_true ).

        IF NOT line_exists( it_0235_save[ tabela = <fs_str> campo = wa_campos-fieldname ] ).
          APPEND VALUE #(
                          tp_venda = wg_header-tp_venda
                          aba = 'D'
                          tabela = <fs_str>
                          campo = wa_campos-fieldname
           ) TO it_0235_save.
        ENDIF.

      ELSE.
        DELETE it_0235_save WHERE tabela EQ <fs_str> AND campo EQ wa_campos-fieldname.
      ENDIF.
    ENDLOOP.

  ENDIF.


ENDFORM.
