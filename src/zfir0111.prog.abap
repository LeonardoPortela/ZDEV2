REPORT zfir0111.
TYPE-POOLS: esp1.

TYPES: BEGIN OF ty_saida,
         seqitem       TYPE  num6,
         status        TYPE  char1,
         status_lanc   TYPE  icon_int,
         gjahr         TYPE  gjahr,
         monat         TYPE  monat,
         dt_doc        TYPE  budat,
         dt_lanc       TYPE  bldat,
         saknr         TYPE  saknr,
         txt50         TYPE  txt50,
         kostl         TYPE  kostl,
         aufnr         TYPE  aufnr,
         desc_fornec   TYPE  char30,
         desc_desp_rec TYPE  char30,
         xblnr         TYPE  xblnr,
         nr_doc        TYPE  xblnr,
         dmbtr         TYPE  dmbtr,
         estorno       TYPE  estorno,
         d_c           TYPE  z_dc,
         lote          TYPE  zlote_num,
         doc_cont      TYPE  zlote_num,
         waers         TYPE  waers,
         recno         TYPE  char1,
         obj_key       TYPE  zib_contabil_chv-obj_key,
         celltab       TYPE  lvc_t_styl,
       END OF ty_saida,

       BEGIN OF ty_filtro,
         bukrs     TYPE bukrs,
         butxt     TYPE butxt,
         werks     TYPE werks_d,
         name1     TYPE name1,
         name_text TYPE name_text,
         usnam     TYPE syuname,
         gjahr     TYPE gjahr,
         monat     TYPE monat,
       END OF ty_filtro,

       BEGIN OF ty_celltab,
         fieldname TYPE  lvc_fname,
         style     TYPE  lvc_style,
         style2    TYPE  lvc_style,
         style3    TYPE  lvc_style,
         style4    TYPE  lvc_style,
         maxlen    TYPE  int4,
       END OF ty_celltab.


TYPES: BEGIN OF ty_message,
         status(4)    TYPE c,
         linha(6)     TYPE c,
         saknr        TYPE saknr,
         mensagem(50) TYPE c.
TYPES: END OF ty_message.


DATA: lt_modify_celltab TYPE STANDARD TABLE OF ty_celltab INITIAL SIZE 0.

DATA: it_sintetica    TYPE STANDARD TABLE OF zfie0111_01,
      wa_sintetica    TYPE zfie0111_01,
      it_saida        TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
      wa_saida        TYPE ty_saida,
      wa_filtro       TYPE ty_filtro,
      wa_zfit0218     TYPE zfit0218,
      escape_loop_alv TYPE i,
      p_erro(1)       TYPE c,
      ls_zglt097      TYPE zglt097.

*** BUG - 178483 - Inicio - CBRAND
DATA: it_message   TYPE TABLE OF ty_message.
DATA: wa_message   LIKE LINE OF  it_message.
DATA: it_excl TYPE slis_t_extab,
      wa_excl TYPE slis_extab.

* Exclusão dos botões padrões do ALV.
wa_excl-fcode = '&ETA'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '%SC'.  APPEND wa_excl TO it_excl.
wa_excl-fcode = '%SC+'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&OUP'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&ODN'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&ILT'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&OL0'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&CRB'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&CRL'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&CRR'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&CRE'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&ALL'. APPEND wa_excl TO it_excl.
wa_excl-fcode = '&SAL'. APPEND wa_excl TO it_excl.
*** BUG - 178483 - Fim - CBRAND

DATA: container_main_c1      TYPE REF TO cl_gui_custom_container,
      o_alv_c1               TYPE REF TO cl_salv_table,
      lo_functions_c1        TYPE REF TO cl_salv_functions_list,
      lo_cols_c1             TYPE REF TO cl_salv_columns,
      lo_column_c1           TYPE REF TO cl_salv_column,
      lo_column_tab_c1       TYPE REF TO cl_salv_column_table,
      lo_columns_tab_c1      TYPE REF TO cl_salv_columns_table,
      "lo_columns_tab_c1         TYPE REF TO cl_salv_columns_table,
      lo_col_tab_c1          TYPE REF TO cl_salv_column_table,
      lo_events_c1           TYPE REF TO cl_salv_events_table,
      lr_selections_c1       TYPE REF TO cl_salv_selections,
      lr_display_settings_c1 TYPE REF TO cl_salv_display_settings,
      l_title_c1             TYPE lvc_title,
      ls_api_c1              TYPE REF TO if_salv_gui_om_extend_grid_api,
      ls_edit_c1             TYPE REF TO if_salv_gui_om_edit_restricted,
      ls_edit_handle_c1      TYPE REF TO cl_salv_gui_om_editable_hndlr,
      lt_celltab_c1          TYPE lvc_t_styl,
      ls_celltab_c1          TYPE lvc_s_styl,
      lt_column_nm_c1        TYPE salv_t_column_ref,
      ls_column_nm_c1        TYPE salv_s_column_ref,
      lo_cols_ref_c1         TYPE        salv_t_column_ref,
      lo_cols_list_c1        TYPE REF TO cl_salv_column_list,
      lo_col_list_c1         LIKE LINE OF lo_cols_ref_c1,
      lr_nncontainer         TYPE string.

DATA: container_main_c2      TYPE REF TO cl_gui_custom_container,
      o_alv_c2               TYPE REF TO cl_salv_table,
      lo_functions_c2        TYPE REF TO cl_salv_functions_list,
      lo_cols_c2             TYPE REF TO cl_salv_columns,
      lo_column_c2           TYPE REF TO cl_salv_column,
      lo_column_tab_c2       TYPE REF TO cl_salv_column_table,
      lo_cols_tab_c2         TYPE REF TO cl_salv_columns_table,
      lo_col_tab_c2          TYPE REF TO cl_salv_column_table,
      lo_events_c2           TYPE REF TO cl_salv_events_table,
      lr_selections_c2       TYPE REF TO cl_salv_selections,
      lr_display_settings_c2 TYPE REF TO cl_salv_display_settings,
      l_title_c2             TYPE lvc_title,
      lo_cols_ref_c2         TYPE        salv_t_column_ref,
      lo_cols_list_c2        TYPE REF TO cl_salv_column_list,
      lo_col_list_c2         LIKE LINE OF lo_cols_ref_c2.


DATA: w_bukrs       TYPE bukrs,
      w_butxt       TYPE butxt,
      w_werks       TYPE werks,
      w_name1       TYPE name1,
      w_gjahr       TYPE char4,
      w_monat       TYPE char2,
      w_usr_id_resp TYPE char10,
      w_usr_nm_resp TYPE char30,
      w_usnam       TYPE zfit0217-usnam.

DATA: _vlr_a           TYPE dmbtr,
      _vlr_d           TYPE dmbtr,
      _vlr_e           TYPE dmbtr,
      _vlr_c_d_verde   TYPE dmbtr,
      _vlr_b_c_verde   TYPE dmbtr,
      _vlr_c_d_amarelo TYPE dmbtr,
      _vlr_b_c_amarelo TYPE dmbtr.

DATA: qtd_rows  TYPE int4,
      index     TYPE i,
      lt_rows   TYPE salv_t_row,
      ls_rows   LIKE LINE OF lt_rows,
      vl_number TYPE i,
      l_acao    TYPE char10,
      lv_lines  TYPE sy-tabix.

DATA: _message_error   TYPE string,
      msg_erro_periodo TYPE string VALUE 'Periodo de lançamento não pode ser maior que o informado!'.


CLASS lcl_alv_events DEFINITION DEFERRED.
DATA lo_alv_events TYPE REF TO lcl_alv_events.

CLASS lcl_alv_events DEFINITION.
  PUBLIC SECTION.
    DATA: grid        TYPE REF TO cl_gui_alv_grid.
    CLASS-METHODS:
      on_hotspot FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender.
*    on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells .
ENDCLASS.
CLASS lcl_alv_events IMPLEMENTATION.

*    METHOD on_data_changed_finished.
*      ENDMETHOD.

  METHOD on_hotspot.
    IF column = 'STATUS_LANC'.
      READ TABLE it_saida INTO DATA(_row) INDEX row.
      IF _row-status_lanc = '@S_TL_R@' AND _row-obj_key IS NOT INITIAL.
        SELECT * FROM zib_contabil_err WHERE obj_key = @_row-obj_key INTO TABLE @DATA(it_erro).
        cl_demo_output=>display( it_erro ).
      ENDIF.
    ELSEIF column = 'DOC_CONT'.
      CLEAR: _row.
      READ TABLE it_saida  INTO _row INDEX row.
      SET PARAMETER ID 'BLN' FIELD _row-doc_cont.
      SET PARAMETER ID 'BUK' FIELD wa_filtro-bukrs.
      SET PARAMETER ID 'GJR' FIELD _row-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      PERFORM u_command.
    ENDIF.
  ENDMETHOD.

  METHOD on_user_command.
    sy-ucomm = e_salv_function.
    PERFORM u_command.
  ENDMETHOD.

  METHOD on_toolbar.

    DATA : mt_toolbar TYPE stb_button.
    CLEAR mt_toolbar.

    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    IF  lr_nncontainer = 'CONTAINER_01'.
      LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
        "3 DESABILITA E 0 HABILITA
        IF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
          <fs_tollbar>-function = 'SALVAR'.
          <fs_tollbar>-icon = '@F_SAVE@'.
          <fs_tollbar>-quickinfo = 'Salvar'.
        ELSEIF <fs_tollbar>-function EQ '&REFRESH'.
          <fs_tollbar>-function = 'REFRESH_ROW'.
        ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
          <fs_tollbar>-function = 'DELETE_ROW'.
        ELSEIF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
          <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&&SEP03'."'&&SEP_ADD_RIGHT'."'&CHECK'.
*      ELSEIF <fs_tollbar>-function EQ '&&SEP03'."'&&SEP_ADD_RIGHT'."'&CHECK'.'
*        <fs_tollbar>-function = 'MODIFY_ROW'.
*        <fs_tollbar>-butn_type = '0'.
*        <fs_tollbar>-icon = '@0Z@'.
*        <fs_tollbar>-quickinfo = 'Modificar'.
        ELSE.
          IF <fs_tollbar>-function+0(2) <> 'FB'.
            <fs_tollbar>-butn_type = '3'.
          ENDIF.
        ENDIF.

        <fs_tollbar>-disabled = COND #( WHEN p_erro IS INITIAL THEN abap_false ELSE abap_true ).

      ENDLOOP.

    ELSE.
      LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar2>).
        "3 DESABILITA E 0 HABILITA
        <fs_tollbar2>-butn_type = '3'.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_listener DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_salv_gui_om_edit_strct_lstr.
ENDCLASS.

CLASS lcl_listener IMPLEMENTATION.

  METHOD if_salv_gui_om_edit_strct_lstr~on_f4_request.
  ENDMETHOD.


  METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.

    SET PARAMETER ID 'BUK' FIELD wa_zfit0218-bukrs.
    SET PARAMETER ID 'WRK' FIELD wa_zfit0218-werks.
    SET PARAMETER ID 'TCD' FIELD sy-repid."sy-tcode.

    o_ui_data_modify->get_ui_changes( IMPORTING t_deleted_rows = DATA(lt_deleted_rows) ).
    o_ui_data_modify->get_ui_changes( IMPORTING t_good_cells = DATA(lt_good_cells) ).
    o_ui_data_modify->get_ui_changes( IMPORTING t_inserted_rows = DATA(lt_inserted_rows) ).
    o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified_cells) ).
    o_ui_data_modify->get_ui_changes( IMPORTING rt_modified_data_rows = DATA(lt_modified_data_rows) ).

    LOOP AT lt_modified_cells ASSIGNING FIELD-SYMBOL(<_get_mod>).
      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_it_saida_mod>) INDEX <_get_mod>-row_id.
      <_it_saida_mod>-recno = abap_true.
      CASE <_get_mod>-fieldname.
        WHEN 'SAKNR'.
          DATA _saknr TYPE skat-saknr.
          UNPACK <_get_mod>-value TO _saknr.
          SELECT SINGLE * FROM skat WHERE spras = 'P' AND ktopl = '0050' AND saknr = @_saknr INTO @DATA(ls_skat).
          SELECT SINGLE * FROM zfit0219 WHERE saknr = @_saknr INTO @DATA(ls_zfit0219).
          IF sy-subrc = 0. "BUG - 178483
            o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
                                                 fieldname  = 'TXT50'
                                                 cell_value = ls_skat-txt50 ).

            IF ls_zfit0219-d_c IS NOT INITIAL.

              o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
                                                   fieldname  = 'D_C'
                                                   cell_value = ls_zfit0219-d_c ).
            ENDIF.
** BUG - 178483 - Inicio - CBRAND
          ELSE.
            MESSAGE | Conta | && _saknr && | não cadastrada na ZFI0164 | && sy-msgv2 TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
** BUG - 178483 - Fim - CBRAND
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN.


START-OF-SELECTION.

*  DATA: it_paramters TYPE  ustyp_t_parameters,
*        it_unidades  TYPE TABLE OF string,
*        wa_unidades  type string,
*        p_autorizacao type boolean.
*
*  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
*    EXPORTING
*      user_name       = sy-uname
*    TABLES
*      user_parameters = it_paramters.
*  IF sy-subrc = 0.
*    READ TABLE it_paramters ASSIGNING FIELD-SYMBOL(<_get_param>) WITH KEY parid = 'Z_GESTAO_FFIXO' .
*    IF sy-subrc = 0 and <_get_param>-parva is not INITIAL.
*      SEARCH <_get_param>-parva FOR ','.
*      IF sy-subrc = 0.
*        SPLIT <_get_param>-parva AT ',' INTO TABLE it_unidades.
*        "cl_demo_output=>display( it_string ).
*      ELSE.
*        wa_unidades = <_get_param>-parva.
*        APPEND wa_unidades TO it_unidades.
*      ENDIF.
*    ENDIF.
*
*    IF it_unidades IS NOT INITIAL.
*      P_autorizacao = abap_true.
*    ELSE.
*      P_autorizacao = abap_false.
*    ENDIF.
*  ENDIF.
*
*  IF P_autorizacao = 'X'.
*    CALL SCREEN '0100'.
*  ELSE.
*    MESSAGE 'Usuário não tem autorização para Lançamento!' TYPE 'I'.
*  ENDIF.

  CALL SCREEN '0100'.

END-OF-SELECTION.

MODULE status_0100 OUTPUT.

  DATA: z_cprog TYPE sy-cprog.

  GET PARAMETER ID 'P_CPROG' FIELD z_cprog.

  IF z_cprog = 'ZFIR0115'.
    GET PARAMETER ID 'P_WERKS' FIELD wa_filtro-werks.
    GET PARAMETER ID 'P_GJAHR' FIELD wa_filtro-gjahr.
    GET PARAMETER ID 'P_MONAT' FIELD wa_filtro-monat.

  ENDIF.

  escape_loop_alv = escape_loop_alv + 1.
  IF escape_loop_alv = 1.
    "PERFORM gos.
    PERFORM make_c1.
    PERFORM make_c2.
    SET PF-STATUS 'STATUS_0100'.
    SET TITLEBAR 'TITLE_0100'.
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.
  PERFORM u_command.
ENDMODULE.


MODULE busca_gjahr.

  DATA: lt_ret_zfit0216_gjahr TYPE TABLE OF ddshretval,
        ls_ret_zfit0216_gjahr TYPE ddshretval,
        ls_zfit0216_gjahr     TYPE zfit0218.

  IF wa_filtro-werks IS NOT INITIAL.
    SELECT DISTINCT gjahr
    FROM zfit0216 AS a
    WHERE a~werks = @wa_filtro-werks
    INTO TABLE @DATA(lt_zfit0216_gjahr).
    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'GJAHR'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'GJAHR'
          value_org       = 'S'
        TABLES
          value_tab       = lt_zfit0216_gjahr
          return_tab      = lt_ret_zfit0216_gjahr
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE lt_ret_zfit0216_gjahr INTO ls_ret_zfit0216_gjahr WITH KEY  retfield = 'WA_FILTRO-MONAT'.
        IF sy-subrc = 0.
*        sy-ucomm = 'ENTER_FILTRO'.
*        PERFORM u_command.
        ENDIF.
      ELSE.
        MESSAGE 'Peencha corretamente o campo empresa!' TYPE 'I'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Verifique os campo Filial' TYPE 'I'.
  ENDIF.

ENDMODULE.

MODULE busca_monat.

  DATA: lt_ret_zfit0216_monat TYPE TABLE OF ddshretval,
        ls_ret_zfit0216_monat TYPE ddshretval,
        ls_zfit0216_monat     TYPE zfit0218.

  IF wa_filtro-werks IS NOT INITIAL AND wa_filtro-gjahr IS NOT INITIAL.

    SELECT DISTINCT monat
    FROM zfit0216 AS a
    WHERE gjahr = @wa_filtro-gjahr
    AND a~werks = @wa_filtro-werks
    INTO TABLE @DATA(lt_zfit0216_monat).
    IF sy-subrc = 0.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'MONAT'
          dynpprog        = sy-cprog
          dynpnr          = sy-dynnr
          dynprofield     = 'MONAT'
          value_org       = 'S'
        TABLES
          value_tab       = lt_zfit0216_monat
          return_tab      = lt_ret_zfit0216_monat
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE lt_ret_zfit0216_monat INTO ls_ret_zfit0216_monat WITH KEY  retfield = 'WA_FILTRO-MONAT'.
        IF sy-subrc = 0.
*        sy-ucomm = 'ENTER_FILTRO'.
*        PERFORM u_command.
        ENDIF.
      ELSE.
        MESSAGE 'Peencha corretamente o campo empresa!' TYPE 'I'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Verifique os campos Filial e Ano' TYPE 'I'.
  ENDIF.

ENDMODULE.

MODULE busca_filial.

  DATA: lt_ret_zfit0218 TYPE TABLE OF ddshretval,
        ls_ret_zfit0218 TYPE ddshretval,
        ls_zfit0218     TYPE zfit0218.

  SELECT bukrs,werks,usnam
  FROM zfit0218 AS a
  INTO TABLE @DATA(lt_zfit0218).
  IF sy-subrc = 0.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'WERKS'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = 'WA_FILTRO-WERKS'
        value_org       = 'S'
      TABLES
        value_tab       = lt_zfit0218
        return_tab      = lt_ret_zfit0218
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE lt_ret_zfit0218 INTO ls_ret_zfit0218 WITH KEY  retfield = 'WA_FILTRO-WERKS'.
      IF sy-subrc = 0.

        SELECT SINGLE a~*
        FROM zfit0218 AS a
        WHERE a~werks = @ls_ret_zfit0218-fieldval
        INTO @wa_zfit0218.

        sy-ucomm = 'ENTER_FILTRO'.
        PERFORM u_command.
      ENDIF.
    ELSE.
      MESSAGE 'Peencha corretamente o campo empresa!' TYPE 'I'.
    ENDIF.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
FORM u_command.

  DATA: lo_selections TYPE REF TO cl_salv_selections.
  DATA: linha TYPE i.
  FREE: lt_rows.
  CLEAR: qtd_rows.

  lo_selections = o_alv_c1->get_selections( ).
  lt_rows = lo_selections->get_selected_rows( ).
  qtd_rows = lines( lt_rows ).

  CASE sy-ucomm.
    WHEN ''. " Enter key.
      CLEAR:wa_zfit0218.
      SELECT SINGLE a~*
      FROM zfit0218 AS a
      WHERE a~werks = @wa_filtro-werks
      INTO @wa_zfit0218.

      MOVE-CORRESPONDING wa_zfit0218 TO wa_filtro.

      SELECT SINGLE name1 FROM t001w WHERE werks = @wa_zfit0218-werks INTO @wa_filtro-name1.
      SELECT SINGLE butxt FROM t001 WHERE bukrs = @wa_zfit0218-bukrs INTO @wa_filtro-butxt.
      SELECT SINGLE name_text FROM v_usr_name WHERE bname = @wa_zfit0218-usnam INTO @wa_filtro-name_text.

      IF wa_filtro-bukrs IS NOT INITIAL
        AND wa_filtro-werks IS NOT INITIAL
        AND wa_filtro-gjahr IS NOT INITIAL
        AND wa_filtro-monat IS NOT INITIAL.
**********************************************************************

        DATA: lr_unidades TYPE RANGE OF bukrs INITIAL SIZE 0,
              lr_werks    TYPE RANGE OF werks.

        SELECT DISTINCT 'I' AS sign,'EQ' AS option, werks AS low, werks AS high FROM t001w WHERE werks = @wa_filtro-werks INTO TABLE @lr_werks.

        MOVE-CORRESPONDING lr_werks TO lr_unidades.

        SORT lr_unidades.

        DELETE ADJACENT DUPLICATES FROM lr_unidades.

        IF lr_unidades IS NOT INITIAL.

          TYPES: BEGIN OF ty_unidades_cad,
                   unidade TYPE bukrs,
                 END OF ty_unidades_cad.

          DATA: it_paramters    TYPE  ustyp_t_parameters,
                it_unidades_cad TYPE STANDARD TABLE OF ty_unidades_cad INITIAL SIZE 0,
                wa_unidades_cad TYPE ty_unidades_cad,
                p_autorizacao   TYPE boolean.

          CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
            EXPORTING
              user_name       = sy-uname
            TABLES
              user_parameters = it_paramters.
          IF sy-subrc = 0.
            READ TABLE it_paramters ASSIGNING FIELD-SYMBOL(<_get_param>) WITH KEY parid = 'Z_GESTAO_FFIXO' .
            IF sy-subrc = 0 AND <_get_param>-parva IS NOT INITIAL.
              SEARCH <_get_param>-parva FOR ','.
              IF sy-subrc = 0.
                SPLIT <_get_param>-parva AT ',' INTO TABLE it_unidades_cad.
                "cl_demo_output=>display( it_string ).
              ELSE.
                wa_unidades_cad = <_get_param>-parva.
                APPEND wa_unidades_cad TO it_unidades_cad.
              ENDIF.

              FIND <_get_param>-parva IN '*'.
              IF sy-subrc = 0.
                SELECT DISTINCT werks FROM t001w INTO TABLE @it_unidades_cad.
              ENDIF.
            ENDIF.

            IF it_unidades_cad IS NOT INITIAL.

              LOOP AT it_unidades_cad INTO wa_unidades_cad WHERE unidade IN lr_unidades.
                p_autorizacao = 'X'.
                EXIT.
              ENDLOOP.
            ENDIF.

            IF p_autorizacao <> 'X'.
              MESSAGE 'Usuário não tem autorização para essa transação!' TYPE 'I'.
            ENDIF.

          ENDIF.

          IF wa_filtro-werks IS NOT INITIAL AND wa_filtro-gjahr IS NOT INITIAL AND wa_filtro-monat IS NOT INITIAL.
            PERFORM gos.
          ENDIF.

          lr_nncontainer = 'CONTAINER_01'.

        ENDIF.

**********************************************************************

        p_erro = abap_false.

        PERFORM get_dados.
        PERFORM get_sintetico.
        CLEAR: ls_zglt097. "Verifica ao entrar na tela!
        SELECT SINGLE * FROM zglt097 WHERE bukrs = @wa_filtro-bukrs AND livro_caixa = @wa_filtro-werks AND gjahr = @wa_filtro-gjahr AND monat = @wa_filtro-monat INTO @ls_zglt097.
        IF sy-subrc = 0.
          IF sy-datum > ls_zglt097-data_lim.
            CLEAR: p_erro,_message_error.
            p_erro = 'X'.
            _message_error = |A data Limite para Lançamento é { ls_zglt097-data_lim+6(2) }/{ ls_zglt097-data_lim+4(2) }/{ ls_zglt097-data_lim+0(4) } !|.
            MESSAGE _message_error  TYPE 'I'.
*            EXIT.  "//wbarbosa 05/02/2025
          ENDIF.
        ELSE.
          CLEAR: p_erro,_message_error.
          p_erro = 'X'.
          _message_error = 'Este período não esta liberado para lançamentos, procurar o CSC-Administrativo!'.
          MESSAGE _message_error  TYPE 'I'.
        ENDIF.
        o_alv_c1->refresh( ).
        o_alv_c2->refresh( ).
        cl_gui_cfw=>flush( ).
      ENDIF.

    WHEN 'BACK'.
      CLEAR: wa_filtro.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      CLEAR: wa_filtro.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      CLEAR: wa_filtro.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ENTER_FILTRO'.
      "CLEAR: wa_filtro.
      PERFORM enter_filtro.
      "PERFORM refresh_all.
    WHEN 'BT_01'.
      CALL TRANSACTION 'ZFI0163'.
    WHEN 'BT_02'.
      CALL TRANSACTION 'ZFI0164'.
    WHEN 'BT_03'.
      CALL TRANSACTION 'ZFI0166'.
    WHEN 'CANCELAR_101'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'SALVAR'.
      IF it_saida IS NOT INITIAL.
        DATA: wa_zfit0217 TYPE zfit0217.
        CLEAR:wa_zfit0217.
        "verifica erros
        CLEAR: wa_saida,p_erro.
        LOOP AT it_saida INTO wa_saida WHERE recno = abap_true.
          IF wa_saida-dt_doc IS NOT INITIAL.
            DATA(dt) = wa_saida-dt_doc.
            DATA(_ano) =  dt+0(4)."6(4).
            DATA(_mes) =  dt+4(2)."3(2).
            DATA(periodo_doc) = |{ _mes }{ _ano }|.
            DATA(periodo_filtro) = |{ wa_filtro-monat  }{ wa_filtro-gjahr }|.
            IF periodo_doc >  periodo_filtro.
              CLEAR: p_erro,_message_error.
              p_erro = 'X'.
              _message_error = msg_erro_periodo.
            ENDIF.
          ENDIF.

          IF wa_saida-dmbtr IS NOT INITIAL.
            IF wa_saida-dmbtr = 0.
              CLEAR: p_erro,_message_error.
              p_erro = 'X'.
              _message_error = 'O valor não pode ser 0!'.
            ENDIF.
          ELSE.
            CLEAR: p_erro,_message_error.
            p_erro = 'X'.
            _message_error = 'O valor não pode ser Vazio ou Nulo!'.
          ENDIF.

          IF wa_saida-d_c IS INITIAL.
            CLEAR: p_erro,_message_error.
            p_erro = 'X'.
            _message_error = 'O campo D\C não pode ser vazio!'.
          ENDIF.
*** BUG - 178483 - Inicio - CBRAND
          SELECT SINGLE * FROM zfit0219 WHERE saknr = @wa_saida-saknr INTO @DATA(ls_zfit0219).
          IF sy-subrc <> 0.
            CONCATENATE 'Conta'  wa_saida-saknr  'não cadastrada na ZFI0164' INTO _message_error SEPARATED BY space.
            p_erro = 'X'.
          ENDIF.
*** BUG - 178483 - Fim- CBRAND
        ENDLOOP.


*        IF ls_ZGLT097 IS NOT INITIAL.
*          IF ls_ZGLT097-data_lim > sy-datum.
*            CLEAR: p_erro,_message_error.
*            p_erro = 'X'.
*            _message_error = msg_dt_lim.
*          ENDIF.
*        ELSE.
*          CLEAR: p_erro,_message_error.
*          p_erro = 'X'.
*          _message_error = 'Não existe aprovador para a Filial e Período informados'.
*        ENDIF.


        IF p_erro = 'X'.
          MESSAGE _message_error TYPE 'I'.
          EXIT.
        ENDIF.
        CLEAR: wa_saida.
        LOOP AT it_saida INTO wa_saida WHERE recno = abap_true.
          MOVE-CORRESPONDING wa_filtro TO wa_zfit0217.
          MOVE-CORRESPONDING wa_saida TO wa_zfit0217.
          wa_zfit0217-usnam = sy-uname.
          wa_zfit0217-dt_entrada = sy-datum.
          wa_zfit0217-hr_entrada = sy-uzeit.
          wa_zfit0217-xblnr = ''.
          MODIFY zfit0217 FROM wa_zfit0217.
          COMMIT WORK.
        ENDLOOP.
        PERFORM refresh_all.

      ELSE.
        IF wa_zfit0218-bukrs IS INITIAL
          AND wa_zfit0218-werks IS INITIAL
          .
          MESSAGE 'Favor preencher todos os Campos de Selecção!' TYPE 'I'.
        ELSE.
          MESSAGE 'Não existe lançamentos!' TYPE 'I'.
        ENDIF.
      ENDIF.

    WHEN 'FB_REPROCESSAR'.
      IF qtd_rows > 0.
        CLEAR: ls_rows.
        LOOP AT lt_rows INTO ls_rows.
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_get_rep>) INDEX ls_rows.
          IF <_get_rep>-status_lanc = '@S_TL_R@' AND <_get_rep>-obj_key IS NOT INITIAL.
            UPDATE zib_contabil SET rg_atualizado = 'N' WHERE obj_key = <_get_rep>-obj_key.
            DELETE FROM zib_contabil_err WHERE obj_key = <_get_rep>-obj_key.
            COMMIT WORK.
          ELSE.
            MESSAGE 'Para reprocessar o Status precisar ser o de erro!' TYPE 'I'.
          ENDIF.
        ENDLOOP.
        PERFORM refresh_all.
      ELSE.
        MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
    WHEN 'FB_LIBERAR'.
      IF qtd_rows > 0.
        DATA: vl_number TYPE i.
        CLEAR: ls_rows,vl_number,  it_message.
*** BUG - 178483 - CBRAND - Inicio
        LOOP AT lt_rows INTO ls_rows.
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_get_ger_val>) INDEX ls_rows.
          IF <_get_ger_val>-status_lanc NE '@S_TL_G@'. "BUG - 185890 - CBRAND
            SELECT SINGLE * FROM zfit0217
              WHERE bukrs   = @wa_filtro-bukrs
                AND werks   = @wa_filtro-werks
                AND monat   = @<_get_ger_val>-monat
                AND gjahr   = @<_get_ger_val>-gjahr
                AND seqitem = @<_get_ger_val>-seqitem
               INTO @DATA(ls_zfit0217).

            IF sy-subrc <> 0.
              wa_message-status = icon_led_red.
              wa_message-linha    = <_get_ger_val>-seqitem.
              wa_message-saknr    = <_get_ger_val>-saknr.
              wa_message-mensagem = 'Salvar documento antes de Liberar para aprovação'.

              APPEND wa_message TO it_message.
              CLEAR: wa_message.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF it_message IS NOT INITIAL.
          DATA(lit_fieldcat) = VALUE slis_t_fieldcat_alv(

         ( fieldname = 'STATUS'     seltext_m = 'Status'    outputlen = '07'   just = 'C' icon = 'X' )
         ( fieldname = 'LINHA'      seltext_m = 'Linha'     outputlen = '07'   )
         ( fieldname = 'SAKNR'      seltext_m = 'Conta'     outputlen = '10'   )
         ( fieldname = 'MENSAGEM'   seltext_m = 'Mensagem'  outputlen = '50' ) ).

          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
            EXPORTING
              i_title              = 'Erros Encontrados'
              i_allow_no_selection = 'X'
              i_tabname            = 'IT_MESSAGE'
              i_zebra              = 'X'
              it_fieldcat          = lit_fieldcat
              it_excluding         = it_excl
            TABLES
              t_outtab             = it_message
            EXCEPTIONS
              program_error        = 1
              OTHERS               = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
        ELSE.
*** BUG - 178483 - CBRAND - Fim
          LOOP AT lt_rows INTO ls_rows.
            READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_get_ger>) INDEX ls_rows.
            IF <_get_ger>-status_lanc NE '@S_TL_G@'. "BUG - 185890 - CBRAND

              IF vl_number IS INITIAL.

                CALL FUNCTION 'NUMBER_GET_NEXT'
                  EXPORTING
                    nr_range_nr             = '01'
                    object                  = 'Z_GFFIXO'
                  IMPORTING
                    number                  = vl_number
                  EXCEPTIONS
                    interval_not_found      = 1
                    number_range_not_intern = 2
                    object_not_found        = 3
                    quantity_is_0           = 4
                    quantity_is_not_1       = 5
                    interval_overflow       = 6
                    buffer_overflow         = 7
                    OTHERS                  = 8.
                IF sy-subrc <> 0.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                ELSE.
                  <_get_ger>-lote = vl_number.
                ENDIF.
              ELSE.
                <_get_ger>-lote = vl_number.
              ENDIF.
              CLEAR:wa_zfit0217.
              MOVE-CORRESPONDING wa_filtro TO wa_zfit0217.
              MOVE-CORRESPONDING <_get_ger> TO wa_zfit0217.
              wa_zfit0217-usnam = sy-uname.
              wa_zfit0217-dt_entrada = sy-datum.
              wa_zfit0217-hr_entrada = sy-uzeit.
              wa_zfit0217-status = 'L'.
              wa_zfit0217-xblnr = ''.
              MODIFY zfit0217 FROM wa_zfit0217.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
          CLEAR:vl_number.
          PERFORM refresh_all.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
    WHEN 'FB_REVERTER'.
      IF qtd_rows > 0.
        CLEAR: ls_rows,vl_number.
        LOOP AT lt_rows INTO ls_rows.
          READ TABLE it_saida INTO DATA(wa_saida_reversao) INDEX ls_rows.
          IF wa_saida_reversao-status_lanc = '@S_TL_G@' AND wa_saida_reversao-obj_key IS NOT INITIAL.

            "Marca Lançamento como Reversão
            UPDATE zfit0217 SET estorno = 'X' WHERE obj_key = wa_saida_reversao-obj_key AND seqitem = wa_saida_reversao-seqitem.
            COMMIT WORK.

            CLEAR:vl_number.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'Z_GFFIXO'
              IMPORTING
                number                  = vl_number
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ELSE.
              wa_saida_reversao-lote = vl_number.
              wa_saida_reversao-status = 'L'.

              IF wa_saida_reversao-d_c = 'D'.
                wa_saida_reversao-d_c = 'C'.
              ELSE.
                wa_saida_reversao-d_c = 'D'.
              ENDIF.

              wa_saida_reversao-xblnr = wa_saida_reversao-obj_key.

              CLEAR: wa_zfit0217, wa_saida_reversao-obj_key, wa_saida_reversao-seqitem.
              SELECT SINGLE MAX( seqitem ) AS seqitem FROM zfit0217
                WHERE bukrs = @wa_filtro-bukrs
                  AND werks = @wa_filtro-werks
                  AND gjahr = @wa_filtro-gjahr
                  AND monat = @wa_filtro-monat
                    INTO @DATA(l_seqitem).

              CONDENSE l_seqitem NO-GAPS.

              wa_saida_reversao-seqitem = l_seqitem + 1.
              MOVE-CORRESPONDING wa_saida_reversao TO wa_zfit0217.
              wa_zfit0217-mandt = sy-mandt.
              wa_zfit0217-bukrs = wa_filtro-bukrs.
              wa_zfit0217-werks = wa_filtro-werks.
              wa_zfit0217-usnam = sy-uname.
              wa_zfit0217-dt_entrada = sy-datum.
              wa_zfit0217-hr_entrada = sy-uzeit.
              MODIFY zfit0217 FROM wa_zfit0217.
              COMMIT WORK.
            ENDIF.
          ENDIF.
          CLEAR: wa_saida_reversao.
        ENDLOOP.
        PERFORM refresh_all.
      ELSE.
        MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
    WHEN 'FB_MODIFY_ROW'.
      IF qtd_rows = 1.
        CLEAR: ls_rows.
        LOOP AT lt_rows INTO ls_rows.
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_get_modify>) INDEX ls_rows.
          IF <_get_modify>-status_lanc = '@OUTLIG@' OR <_get_modify>-status_lanc = '@S_TL_R@'. "FUNDO FIXO - Reprocessamento de documentos com erro BUG #181652 - BG
            <_get_modify>-celltab = lt_modify_celltab.
            o_alv_c1->refresh( ).
          ELSE.
            MESSAGE 'Este Status não permite modificação!' TYPE 'I' DISPLAY LIKE 'I'.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE 'Selecione uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
    WHEN 'INSERT_ROW'.
      IF wa_zfit0218-bukrs IS NOT INITIAL
  AND wa_zfit0218-werks IS NOT INITIAL AND p_erro IS INITIAL.
        PERFORM f_nova_linha.
      ELSE.
        IF p_erro = 'X'.
          MESSAGE _message_error TYPE 'I'.
          EXIT.
        ELSE.
          MESSAGE 'Favor preencher todos os Campos de Selecção!' TYPE 'I'.
          EXIT.
        ENDIF.
      ENDIF.
    WHEN 'DELETE_ROW'.
      TYPES: BEGIN OF ty_seq,
               sign   TYPE sign,
               option TYPE c LENGTH 2,
               low    LIKE zfit0217-seqitem,
               high   LIKE zfit0217-seqitem,
             END OF ty_seq.
      DATA: lr_seq TYPE RANGE OF zfit0217-seqitem,
            l_seq  TYPE ty_seq.
      IF qtd_rows > 0.
        CLEAR: ls_rows,l_seq,lr_seq.
        LOOP AT lt_rows INTO ls_rows.
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_get_del>) INDEX ls_rows.
          IF <_get_del>-status_lanc = '@S_TL_Y@' OR <_get_del>-status_lanc = '@EB@' OR <_get_del>-status_lanc = '@OUTLIG@'.
            l_seq-sign = 'I'.
            l_seq-option = 'EQ'.
            l_seq-low = <_get_del>-seqitem.
            APPEND l_seq TO lr_seq.
          ELSE.
            DATA(erro) = 'X'.
          ENDIF.
        ENDLOOP.
        IF erro = 'X'.
          MESSAGE 'Selecione apenas os status "Liberado" ou "Novo" !' TYPE 'I' DISPLAY LIKE 'I'.
          CLEAR: erro.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.
      IF lr_seq IS NOT INITIAL.
        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_del>) WHERE seqitem IN lr_seq AND ( status_lanc = '@S_TL_Y@' OR status_lanc = '@OUTLIG@' ).
          DELETE FROM zfit0217
          WHERE bukrs = wa_filtro-bukrs
          AND werks = wa_filtro-werks
          AND gjahr = wa_filtro-gjahr
          AND monat = wa_filtro-monat
          AND seqitem = <_del>-seqitem.
          COMMIT WORK.
          DELETE it_saida WHERE seqitem = <_del>-seqitem .
        ENDLOOP.

        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_del2>) WHERE seqitem IN lr_seq AND status_lanc = '@EB@' OR status_lanc ='@OUTLIG@'.
          DELETE it_saida WHERE seqitem = <_del2>-seqitem .
        ENDLOOP.

      ENDIF.
      PERFORM refresh_all.
    WHEN 'REFRESH_ROW'.
      PERFORM refresh_all.
  ENDCASE.
ENDFORM.

FORM get_dados.
  FREE: it_saida.
  SELECT * FROM zfit0217
    WHERE bukrs = @wa_filtro-bukrs
  AND werks = @wa_filtro-werks
  AND gjahr = @wa_filtro-gjahr
  AND monat = @wa_filtro-monat
  INTO TABLE @DATA(it_c1).
  SORT it_c1 BY bukrs ASCENDING werks ASCENDING seqitem ASCENDING.
  MOVE-CORRESPONDING it_c1 TO it_saida.
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_get>).
    CASE <_get>-status.
      WHEN 'A'.
        <_get>-status_lanc = '@S_TL_G@'.
        IF <_get>-obj_key IS NOT INITIAL.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<_get>-obj_key INTO @DATA(l_belnr).
          IF sy-subrc = 0.
            <_get>-doc_cont = l_belnr.
          ELSE.
            SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<_get>-obj_key INTO @DATA(l_err).
            IF sy-subrc = 0.
              <_get>-status_lanc = '@S_TL_R@'.
            ELSE.
              <_get>-status_lanc = '@AH@'.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'L'.
        <_get>-status_lanc = '@S_TL_Y@'.
      WHEN 'R'.
        <_get>-status_lanc = '@S_NONO@'.
      WHEN OTHERS.
        <_get>-status_lanc = '@OUTLIG@'.
    ENDCASE.
  ENDLOOP.
ENDFORM.

FORM make_c1.
  CLEAR:lr_nncontainer.
  lr_nncontainer = 'CONTAINER_01'.

  container_main_c1 = NEW cl_gui_custom_container(
    "parent         = cl_gui_container=>default_screen
    lifetime       = cl_gui_container=>lifetime_dynpro
    container_name = 'CONTAINER_01'
  ).

  DATA: lx_msg TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = container_main_c1
          container_name = 'CONTAINER_01'
        IMPORTING
          r_salv_table   = o_alv_c1
        CHANGING
          t_table        = it_saida ).
    CATCH cx_salv_msg INTO lx_msg.
  ENDTRY.

  lo_functions_c1 = o_alv_c1->get_functions( ).

  TRY.

      lo_functions_c1->add_function( name     = 'FB_MODIFY_ROW'
                                     icon     = '@0Z@'
                                     text     = ''
                                     tooltip  = 'Modificar'
                                     position = if_salv_c_function_position=>right_of_salv_functions ).

      lo_functions_c1->add_function( name     = 'FB_LIBERAR'
                                     icon     = '@39@'
                                     text     = 'Liberar para Aprovação'
                                     tooltip  = 'Liberar para Aprovação'
                                     position = if_salv_c_function_position=>right_of_salv_functions ).

      lo_functions_c1->add_function( name     = 'FB_REVERTER'
                                     icon     = '@B_CANC@' "'@9X@'
                                     text     = 'Estornar' "'Reverter Lançamento'
                                     tooltip  = 'Estornar' "'Reverter Lançamento'
                                     position = if_salv_c_function_position=>right_of_salv_functions ).

      lo_functions_c1->add_function( name     = 'FB_REPROCESSAR'
                                     icon     = '@F9@'
                                     text     = 'Reprocessar Contabilização'
                                     tooltip  = 'Reprocessar Contabilização'
                                     position = if_salv_c_function_position=>right_of_salv_functions ).


    CATCH cx_root.

  ENDTRY.

**********************************************************************


  lo_columns_tab_c1 = o_alv_c1->get_columns( ).
  lo_events_c1 = o_alv_c1->get_event( ).

  SET HANDLER lo_alv_events->on_user_command FOR ALL INSTANCES ACTIVATION 'X'."FOR lo_events_c1.
  SET HANDLER lo_alv_events->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.
  SET HANDLER lo_alv_events->on_hotspot FOR ALL INSTANCES ACTIVATION 'X'.

*...Get all the Columns

  lo_cols_c1 = o_alv_c1->get_columns( ).
  "lo_cols_c1->set_optimize( abap_false ).
  DATA: ls_ddic_f4_ref TYPE salv_s_ddic_reference.

  lo_cols_ref_c1    = lo_cols_c1->get( ).


  TRY.

      LOOP AT lo_cols_ref_c1 INTO lo_col_list_c1.
        lo_cols_list_c1 ?= lo_col_list_c1-r_column.    "Narrow casting
        CLEAR: ls_ddic_f4_ref.
        CASE lo_col_list_c1-columnname.
          WHEN 'GJAHR' .
            lo_cols_list_c1->set_short_text( 'Ano' ).
            lo_cols_list_c1->set_medium_text( 'Ano' ).
            lo_cols_list_c1->set_long_text( 'Ano' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c1->set_output_length( '04' ).
          WHEN 'MONAT' .
            lo_cols_list_c1->set_short_text( 'Mês' ).
            lo_cols_list_c1->set_medium_text( 'Mês' ).
            lo_cols_list_c1->set_long_text( 'Mês' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c1->set_output_length( '03' ).
          WHEN 'SEQITEM' .
            lo_cols_list_c1->set_short_text( 'Seq.' ).
            lo_cols_list_c1->set_medium_text( 'Seq.' ).
            lo_cols_list_c1->set_long_text( 'Seq.' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '04' ).
          WHEN 'STATUS_LANC'.
            lo_cols_list_c1->set_short_text( 'Status' ).
            lo_cols_list_c1->set_medium_text( 'Status' ).
            lo_cols_list_c1->set_long_text( 'Status' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c1->set_output_length( '06' ).
            lo_cols_list_c1->set_cell_type( if_salv_c_cell_type=>hotspot ).
          WHEN 'LOTE'.
            lo_cols_list_c1->set_short_text( 'Lote' ).
            lo_cols_list_c1->set_medium_text( 'Lote' ).
            lo_cols_list_c1->set_long_text( 'Lote' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
          WHEN 'DT_DOC'.
            lo_cols_list_c1->set_short_text( 'Dt.Doc.' ).
            lo_cols_list_c1->set_medium_text( 'Data Doc.' ).
            lo_cols_list_c1->set_long_text( 'Data Documento' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
          WHEN 'DT_LANC'.
            lo_cols_list_c1->set_short_text( 'Dt.Lanc.' ).
            lo_cols_list_c1->set_medium_text( 'Data Lanc.' ).
            lo_cols_list_c1->set_long_text( 'Data Lançamento' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
          WHEN 'SAKNR'.
            lo_cols_list_c1->set_short_text( 'Ct.Razão' ).
            lo_cols_list_c1->set_medium_text( 'Conta Razão' ).
            lo_cols_list_c1->set_long_text( 'Conta Razão' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
            ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0219'  field = 'SAKNR').
            lo_cols_list_c1->set_ddic_reference( value = ls_ddic_f4_ref ).
            lo_cols_list_c1->set_f4( if_salv_c_bool_sap=>true ).
          WHEN 'TXT50'.
            lo_cols_list_c1->set_short_text( 'DescRazão ' ).
            lo_cols_list_c1->set_medium_text( 'Descrição Razão' ).
            lo_cols_list_c1->set_long_text( 'Descrição Razão' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
          WHEN 'KOSTL'.
            lo_cols_list_c1->set_short_text( 'Cent.Cust.' ).
            lo_cols_list_c1->set_medium_text( 'Centro Custo' ).
            lo_cols_list_c1->set_long_text( 'Centro de Custo' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
            ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0218'  field = 'KOSTL').
            lo_cols_list_c1->set_ddic_reference( value = ls_ddic_f4_ref ).
            lo_cols_list_c1->set_f4( if_salv_c_bool_sap=>true ).
          WHEN 'AUFNR'.
            lo_cols_list_c1->set_short_text( 'Ordem' ).
            lo_cols_list_c1->set_medium_text( 'Ordem' ).
            lo_cols_list_c1->set_long_text( 'Ordem' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
          WHEN'DESC_FORNEC' .
            lo_cols_list_c1->set_short_text( 'For.Desc.' ).
            lo_cols_list_c1->set_medium_text( 'Forne Descrição' ).
            lo_cols_list_c1->set_long_text( 'Fornecedor (Descrição)' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '25' ).

          WHEN'DESC_DESP_REC' .
            lo_cols_list_c1->set_short_text( 'Desc.D\C' ).
            lo_cols_list_c1->set_medium_text( 'Desc.Despesa\Receita' ).
            lo_cols_list_c1->set_long_text( 'Descrição Despesa\Receita' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '30' ).

          WHEN'NR_DOC' .
            lo_cols_list_c1->set_short_text( 'Nro.Doc.' ).
            lo_cols_list_c1->set_medium_text( 'Nro.Doc.' ).
            lo_cols_list_c1->set_long_text( 'Nro.Doc.' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).

          WHEN'DMBTR'.
            lo_cols_list_c1->set_short_text( 'Valor BRL' ).
            lo_cols_list_c1->set_medium_text( 'Valor BRL' ).
            lo_cols_list_c1->set_long_text( 'Valor BRL' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).

          WHEN'ESTORNO'.
            lo_cols_list_c1->set_short_text( 'Estornado' ).
            lo_cols_list_c1->set_medium_text( 'Estornado' ).
            lo_cols_list_c1->set_long_text( 'Estornado' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c1->set_output_length( '9' ).

          WHEN'D_C' .
            lo_cols_list_c1->set_short_text( 'D\C' ).
            lo_cols_list_c1->set_medium_text( 'D\C' ).
            lo_cols_list_c1->set_long_text( 'D\C' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            lo_cols_list_c1->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c1->set_output_length( '3' ).
            ls_ddic_f4_ref = VALUE #( table  = 'ZFIT0217'  field = 'D_C').
            lo_cols_list_c1->set_ddic_reference( value = ls_ddic_f4_ref ).
            lo_cols_list_c1->set_f4( if_salv_c_bool_sap=>true ).

          WHEN 'DOC_CONT' .
            lo_cols_list_c1->set_short_text( 'Doc.Cont.').
            lo_cols_list_c1->set_medium_text( 'Doc.Contabil' ).
            lo_cols_list_c1->set_long_text( 'Doc.Contabil' ).
            lo_cols_list_c1->set_optimized( abap_false ).
            "lo_cols_list_c1->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c1->set_output_length( '10' ).
            lo_cols_list_c1->set_cell_type( if_salv_c_cell_type=>hotspot ).
          WHEN OTHERS.
            lo_cols_list_c1->set_visible( if_salv_c_bool_sap=>false ).
        ENDCASE.
      ENDLOOP.
    CATCH cx_salv_not_found.
  ENDTRY.

*        lo_cols_c1->set_column_position( columnname = 'CHAVE_NFE_SAP'      position  =  1   ).
*        lo_cols_c1->set_column_position( columnname = 'EMPRESA'            position  = 2   ).
*        lo_cols_c1->set_column_position( columnname = 'FILIAL'             position  = 3   ).

*  lo_col_tab_c1 ?= lo_columns_tab_c1->get_column( columnname = 'DT_DOC' ).
*  lo_col_tab_c1->set_ddic_reference( value = 'ZFIT0217-BLDAT' ).

*    l_title = |Nome Relatório|.
*    lr_display_settings = o_alv_c1->get_display_settings( ).
*    lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*    lr_display_settings->set_list_header( l_title ).
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*    lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*
*    "Enable Zebra Layout
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lr_selections_c1 = o_alv_c1->get_selections( ).
  lr_selections_c1->set_selection_mode( if_salv_c_selection_mode=>row_column ). "if_salv_c_selection_mode=>c. if_salv_c_selection_mode=>multiple

  lo_functions_c1->set_all( abap_false ).
  lo_functions_c1->set_default( abap_false ).

  o_alv_c1->display( ).

  PERFORM edit_make_c1.
  CLEAR:lr_nncontainer.
ENDFORM.

FORM make_c2.
  CLEAR:lr_nncontainer.
  lr_nncontainer = 'CONTAINER_02'.

  container_main_c2 = NEW cl_gui_custom_container(
    parent         = cl_gui_container=>default_screen
    "lifetime       =  cl_gui_container=>lifetime_dynpro
    container_name = 'CONTAINER_02'
  ).

  DATA: lx_msg TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = container_main_c2
          container_name = 'CONTAINER_02'
        IMPORTING
          r_salv_table   = o_alv_c2
        CHANGING
          t_table        = it_sintetica ).
    CATCH cx_salv_msg INTO lx_msg.
  ENDTRY.

  lo_functions_c2 = o_alv_c2->get_functions( ).
  lo_functions_c2->set_all( abap_false ).
  lo_functions_c2->set_default( abap_false ).

*...Get all the Columns

  lo_cols_c2 = o_alv_c2->get_columns( ).
  "lo_cols_c1->set_optimize( abap_false ).
  DATA: ls_ddic_f4_ref TYPE salv_s_ddic_reference.

  lo_cols_ref_c2    = lo_cols_c2->get( ).

  TRY.

      LOOP AT lo_cols_ref_c2 INTO lo_col_list_c2.
        lo_cols_list_c2 ?= lo_col_list_c2-r_column.    "Narrow casting
        CLEAR: ls_ddic_f4_ref.
        CASE lo_col_list_c2-columnname.
          WHEN 'SEQ' .
            lo_cols_list_c2->set_short_text( 'Seq.' ).
            lo_cols_list_c2->set_medium_text( 'Seq.' ).
            lo_cols_list_c2->set_long_text( 'Seq.' ).
            lo_cols_list_c2->set_optimized( abap_false ).
            lo_cols_list_c2->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c2->set_output_length( '04' ).
          WHEN 'DESCRICAO' .
            lo_cols_list_c2->set_short_text( 'Descrição' ).
            lo_cols_list_c2->set_medium_text( 'Descrição' ).
            lo_cols_list_c2->set_long_text( 'Descrição' ).
            lo_cols_list_c2->set_optimized( abap_false ).
            "lo_cols_list_c2->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c2->set_output_length( '20' ).
          WHEN 'PERIODO' .
            lo_cols_list_c2->set_short_text( 'Período' ).
            lo_cols_list_c2->set_medium_text( 'Período' ).
            lo_cols_list_c2->set_long_text( 'Período' ).
            lo_cols_list_c2->set_optimized( abap_false ).
            lo_cols_list_c2->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c2->set_output_length( '8' ).
          WHEN  'STATUS'.
            lo_cols_list_c2->set_short_text( 'Status' ).
            lo_cols_list_c2->set_medium_text( 'Status' ).
            lo_cols_list_c2->set_long_text( 'Status' ).
            lo_cols_list_c2->set_optimized( abap_false ).
            lo_cols_list_c2->set_alignment( if_salv_c_alignment=>centered ).
            lo_cols_list_c2->set_output_length( '6' ).
          WHEN  'VLR'.
            lo_cols_list_c2->set_short_text( 'Valor BRL' ).
            lo_cols_list_c2->set_medium_text( 'Valor BRL' ).
            lo_cols_list_c2->set_long_text( 'Valor BRL' ).
            lo_cols_list_c2->set_optimized( abap_false ).
            "lo_cols_list_c2->set_alignment( if_salv_c_alignment=>left ).
            lo_cols_list_c2->set_output_length( '15' ).

        ENDCASE.

      ENDLOOP.
    CATCH cx_salv_not_found.
  ENDTRY.
*        lo_cols_c2->set_column_position( columnname = 'CHAVE_NFE_SAP'      position  =  1   ).
*        lo_cols_c2->set_column_position( columnname = 'EMPRESA'            position  = 2   ).
*        lo_cols_c2->set_column_position( columnname = 'FILIAL'             position  = 3   ).


  o_alv_c2->display( ).

  CLEAR: lr_nncontainer.
ENDFORM.

FORM f_nova_linha .

  DATA: lo_selections TYPE REF TO cl_salv_selections,
        qtd           TYPE i.
  FREE: lt_rows.
  CLEAR: qtd_rows,index.

  lo_selections = o_alv_c1->get_selections( ).
  lt_rows = lo_selections->get_selected_rows( ).
  qtd_rows = lines( lt_rows ).


  DATA linha TYPE i.
  CLEAR: ls_column_nm_c1,linha,ls_celltab_c1.
  FREE: lt_column_nm_c1,lt_celltab_c1.

  "AQUI SE PEGA DINAMICAMENTE AS COLUNAS OPTEI POR FIXAR OS VALORES NA lt_modify_celltab DEVIDO PRECISAR MODIFICAR TAMBEM A LINHA
*  lt_column_nm_c1 = o_alv_c1->get_columns( )->get( ).
*  SORT  lt_column_nm_c1 BY columnname ASCENDING.
*  LOOP AT lt_column_nm_c1 ASSIGNING FIELD-SYMBOL(<_celltab>).
*    IF <_celltab>-columnname = 'DT_DOC'
*      OR <_celltab>-columnname = 'SAKNR'
*      OR <_celltab>-columnname = 'KOSTL'
*      OR <_celltab>-columnname = 'AUFNR'
*      OR <_celltab>-columnname = 'DESC_FORNEC'
*      OR <_celltab>-columnname = 'DESC_DESP_REC'
*      OR <_celltab>-columnname = 'DMBTR'
*      OR <_celltab>-columnname = 'NR_DOC'
*      "OR <_celltab>-columnname = 'D_C'
*      .
*      linha = linha + 1.
*      ls_celltab_c1-fieldname = <_celltab>-columnname.
*      ls_celltab_c1-style = '00080000'.
*      INSERT ls_celltab_c1 INTO lt_celltab_c1 INDEX linha.
*    ENDIF.
*  ENDLOOP.

  CLEAR: qtd,linha,wa_saida.

  qtd = lines( it_saida ).

  IF qtd = 0.
    linha = 1.
  ELSE.
    SORT it_saida BY seqitem DESCENDING.
    READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_new>) INDEX 1.
    IF  sy-subrc = 0.
      IF <_new>-seqitem >= 1.
        linha = <_new>-seqitem + 1.
      ELSE.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM zfit0218 WHERE bukrs = @wa_filtro-bukrs AND werks = @wa_filtro-werks INTO @DATA(ls_zfit0218).
  IF sy-subrc = 0.
    wa_saida-kostl = ls_zfit0218-kostl.
  ENDIF.

  wa_saida-seqitem = linha.
  wa_saida-status_lanc = icon_light_out.
  wa_saida-gjahr = wa_filtro-gjahr.
  wa_saida-monat = wa_filtro-monat.

  SELECT SINGLE waers FROM t001 WHERE bukrs = @wa_filtro-bukrs INTO @wa_saida-waers.

  APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
  CLEAR: qtd,linha,wa_saida.
  <_put_celltab>-celltab = lt_modify_celltab."lt_celltab_c1. "aQUI VC EDITA UMA LINHA
  SORT it_saida ASCENDING.
  o_alv_c1->refresh( ).
ENDFORM.

FORM edit_make_c1 .
  ls_api_c1 = o_alv_c1->extended_grid_api( ).
  ls_edit_c1 = ls_api_c1->editable_restricted( ).

  ls_edit_c1->set_t_celltab_columnname( t_celltab_columnname = 'CELLTAB' ).

  DATA(mo_listener) = NEW lcl_listener( ).
  ls_edit_c1->set_listener( mo_listener ).
  ls_edit_c1->validate_changed_data( ).
  o_alv_c1->refresh( ).

  "AQUI INCLUI AS COLUNAS QUE PODEM SER EDITADAS
  lt_modify_celltab = VALUE #(
( fieldname = 'AUFNR' style = '00080000' )
( fieldname = 'DESC_DESP_REC' style = '00080000' )
( fieldname = 'DESC_FORNEC' style = '00080000' )
( fieldname = 'DMBTR' style = '00080000' )
( fieldname = 'DT_DOC' style = '00080000' )
( fieldname = 'KOSTL' style = '00080000' )
( fieldname = 'NR_DOC' style = '00080000' )
( fieldname = 'SAKNR' style = '00080000' )
).

ENDFORM.

FORM enter_filtro .
  DATA: dynpfields LIKE dynpread OCCURS 6 WITH HEADER LINE.
  DATA: l_stepl LIKE sy-stepl.
  REFRESH dynpfields.

  CLEAR dynpfields.
  dynpfields-fieldname = 'WA_FILTRO-BUKRS'. "Screen field name
  dynpfields-fieldvalue = wa_zfit0218-bukrs. "New value
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.
  dynpfields-fieldname = 'WA_FILTRO-WERKS'. "Screen field name
  dynpfields-fieldvalue = wa_zfit0218-werks. "New value
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.
  SELECT SINGLE usnam FROM zfit0218 WHERE bukrs = @wa_zfit0218-bukrs AND werks = @wa_zfit0218-werks INTO @wa_zfit0218-usnam.
  dynpfields-fieldname = 'WA_FILTRO-USNAM'. "Screen field name
  dynpfields-fieldvalue = wa_zfit0218-usnam. "New value
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.
  dynpfields-fieldname = 'WA_FILTRO-NAME1'. "Screen field name
  SELECT SINGLE name1 FROM t001w WHERE werks = @wa_zfit0218-werks INTO @dynpfields-fieldvalue.
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.
  dynpfields-fieldname = 'WA_FILTRO-BUTXT'. "Screen field name
  SELECT SINGLE butxt FROM t001 WHERE bukrs = @wa_zfit0218-bukrs INTO @dynpfields-fieldvalue.
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.
  dynpfields-fieldname = 'WA_FILTRO-NAME_TEXT'. "Screen field name
  SELECT SINGLE name_text FROM v_usr_name WHERE bname = @wa_zfit0218-usnam INTO @dynpfields-fieldvalue.
  dynpfields-stepl = l_stepl. "Step loop for table controls
  APPEND dynpfields.

  CLEAR dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid "Program name
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 0.
  CLEAR sy-ucomm.
ENDFORM.

FORM get_sintetico .
  CLEAR: _vlr_a,_vlr_b_c_amarelo,_vlr_b_c_verde,_vlr_c_d_amarelo,_vlr_c_d_verde,_vlr_d,_vlr_e.
  FREE: it_sintetica.
  DO 6 TIMES.
    CLEAR:wa_sintetica.
    DATA linha TYPE i.
    linha = linha + 1.
    CASE linha.
      WHEN 1.
        wa_sintetica-seq = 'A'.
        wa_sintetica-descricao = 'Saldo Anterior'.

        IF wa_filtro-monat = '01'.
          DATA(v_monat) = '12'.
          DATA(v_gjahr) = wa_filtro-gjahr - 1.
        ELSE.
          v_monat = wa_filtro-monat - 1.

          IF v_monat < 9.
            v_monat = |0{ v_monat }|.
          ENDIF.

          v_gjahr = wa_filtro-gjahr.
        ENDIF.

        SELECT SINGLE saldo_fixo
FROM zfit0216
WHERE 1 = 1
AND bukrs = @wa_filtro-bukrs
        AND werks = @wa_filtro-werks
        AND monat = @v_monat
        AND gjahr = @v_gjahr
        INTO @wa_sintetica-vlr.
        wa_sintetica-periodo = |{ v_monat }/{ v_gjahr }|.
        _vlr_a = wa_sintetica-vlr.
      WHEN 2.
        wa_sintetica-seq = 'B'.
        wa_sintetica-descricao = 'Entrada'.

        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_entrada>) WHERE d_c = 'C'.
          IF <_entrada>-status_lanc = '@S_TL_G@'. "Verde
            _vlr_b_c_verde = _vlr_b_c_verde + <_entrada>-dmbtr.
          ENDIF.
          IF <_entrada>-status_lanc <> '@S_TL_G@' AND <_entrada>-status_lanc <> '@S_NONO@'. "'@S_TL_Y@'. "Amarelo
            _vlr_b_c_amarelo = _vlr_b_c_amarelo + <_entrada>-dmbtr.
          ENDIF.
        ENDLOOP.
        wa_sintetica-vlr = _vlr_b_c_verde.
      WHEN 3.
        wa_sintetica-seq = 'C'.
        wa_sintetica-descricao = 'Saida'.

        LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_saida>) WHERE d_c = 'D'.
          IF  <_saida>-status_lanc = '@S_TL_G@'. "verde
            _vlr_c_d_verde = _vlr_c_d_verde + <_saida>-dmbtr.
          ENDIF.
          IF  <_saida>-status_lanc <> '@S_TL_G@' AND <_saida>-status_lanc <> '@S_NONO@'.
            _vlr_c_d_amarelo = _vlr_c_d_amarelo + <_saida>-dmbtr.
          ENDIF.
        ENDLOOP.
        _vlr_c_d_amarelo = _vlr_c_d_amarelo * -1.
        _vlr_c_d_verde = _vlr_c_d_verde * -1.
        wa_sintetica-vlr = _vlr_c_d_verde.
      WHEN 4.
        wa_sintetica-seq = 'D'.
        wa_sintetica-descricao = 'Saldo Contabilizado'.
        _vlr_d = _vlr_a + _vlr_b_c_verde + _vlr_c_d_verde.
        wa_sintetica-vlr = _vlr_d.
      WHEN 5.
        wa_sintetica-seq = 'E'.
        wa_sintetica-descricao = 'Total Não Contabilizado'.
        _vlr_e = _vlr_b_c_amarelo + _vlr_c_d_amarelo.
        wa_sintetica-vlr = _vlr_e.

        IF wa_sintetica-vlr <> 0.
          wa_sintetica-status = '@S_LEDY@'.
        ELSE.
          wa_sintetica-status = ''.
        ENDIF.

      WHEN 6.
        wa_sintetica-seq = 'F'.
        wa_sintetica-descricao = 'Saldo Fundo'.
        wa_sintetica-vlr = _vlr_d + _vlr_e.
    ENDCASE.
    APPEND wa_sintetica TO it_sintetica.
  ENDDO.
  CLEAR: _vlr_a,_vlr_b_c_amarelo,_vlr_b_c_verde,_vlr_c_d_amarelo,_vlr_c_d_verde,_vlr_d,_vlr_e.
ENDFORM.

FORM refresh_all .
  PERFORM get_dados.
  PERFORM get_sintetico.
  o_alv_c1->refresh( ).
  o_alv_c2->refresh( ).
  cl_gui_cfw=>flush( ).
ENDFORM.

FORM gos.

**********************************************************************GOS

*Name         Description                    Icon
*
*ARL_LINK     Save Business Document         Save business document
*BARCODE      Enter Bar Code                 Enter Bar Code
*CREATE_ATTA  Create...                      Create...
*INFO_SERVICE Help for object services       Help for the object services
*MYO_ADD      Add to My Objects              Add to My Objects
*MYOBJECTS    My Objects                     My Objects
*NOTE_CREA    Create note                    Create note
*PCATTA_CREA  Create Attachment              Insert Document as Attachment
*PERS_NOTE    Private note                   Private note
*POC_MONITOR  Process Monitor                Process Monitor
*PPFACTION    Actions in PPF                 Actions
*SO_SENDHIST  Object Outbox                  Object Outbox
*SO_SENDOBJ   Send object with note          Send object with note
*SO_SENDSERV  Send                           Send object with note
*SOFTPHONE    Telephony                      Telephony
*SRELATIONS   Relationships                  Display relationships
*SUBSCRIBE    Subscribe/cancel object        Subscribe/cancel object
*URL_CREA     Create external document (URL) Insert Hyperlink as Attachment
*VIEW_ATTA    Attachment list                Attachment list
*WF_ARCHIVE   Archived workflows             Archived workflows
*WF_OVERVIEW  Workflow overview              Workflow overview
*WF_SERVICES  Workflow                       Workflow
*WF_START     Start Workflow                 Start Workflow
*WF_TOOLBOX   Current workflow task          Current workflow task


  DATA: go_myobject TYPE REF TO cl_gos_manager,
        lt_services TYPE tgos_sels,
        ls_service  TYPE sgos_sels,
        ls_object   TYPE borident.


*  IF wa_filtro-gjahr IS NOT INITIAL AND wa_filtro-monat IS NOT INITIAL.
  ls_object-objkey = |{ sy-tcode }{ wa_filtro-werks }{ wa_filtro-gjahr }{ wa_filtro-monat }|.
  ls_object-objtype = |{ sy-tcode }|.
*  else.
*    CLEAR: ls_object-objkey ,ls_object-objtype.
*  ENDIF.

  ls_service-sign = 'I'.
  ls_service-option = 'EQ'.
  ls_service-low = 'VIEW_ATTA'.
  APPEND ls_service TO lt_services.

  ls_service-sign = 'I'.
  ls_service-option = 'EQ'.
  ls_service-low = 'PCATTA_CREA'.
  APPEND ls_service TO lt_services.


  CREATE OBJECT go_myobject
    EXPORTING
      is_object            = ls_object
      it_service_selection = lt_services
      ip_no_commit         = abap_false
      ip_mode              = 'E' " Edit mode
    EXCEPTIONS
      OTHERS               = 1.
**********************************************************************

ENDFORM.
