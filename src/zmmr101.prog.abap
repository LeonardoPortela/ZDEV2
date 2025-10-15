*&--------------------------------------------------------------------&*
*&                        MM                                         &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 26/11/2014                                              &*
*& Descrição: Relatório – Estratégia de liberação                     &*
*& Transação: ZMM                                                     &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zmmr101.

TYPE-POOLS vrm.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES: csks, t001w, ausp, zprovcoupa01, zmmt0003, t16ft.
*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES: BEGIN OF ty_saida,
         bukrs           TYPE csks-bukrs,
         butxt           TYPE t001-butxt,
         gsber           TYPE csks-gsber,
         werks           TYPE t001w-werks,
         name1           TYPE t001w-name1,
         uname           TYPE zmmt0003-uname,
         kostl           TYPE csks-kostl,
         ktext           TYPE cskt-ktext,
         bloq(1),
         objek           TYPE ausp-objek,
         atwrt           TYPE ausp-atwrt,
         bname           TYPE user_addr-bname,
         name_textc      TYPE user_addr-name_textc,
         requisicao_nome TYPE frgxt,
         pedido_nome_01  TYPE t16fd-frgct,
         pedido_valor_01 TYPE t16ft-frgxt,
         pedido_nome_02  TYPE t16fd-frgct,
         pedido_valor_02 TYPE t16ft-frgxt,
         pedido_nome_03  TYPE t16fd-frgct,
         pedido_valor_03 TYPE t16ft-frgxt,
         pedido_nome_04  TYPE t16fd-frgct,
         pedido_valor_04 TYPE t16ft-frgxt,
         pedido_nome_05  TYPE t16fd-frgct,
         pedido_valor_05 TYPE t16ft-frgxt,
         pedido_nome_06  TYPE t16fd-frgct,
         pedido_valor_06 TYPE t16ft-frgxt,
         pedido_nome_07  TYPE t16fd-frgct,
         pedido_valor_07 TYPE t16ft-frgxt,
         pedido_nome_08  TYPE t16fd-frgct,
         pedido_valor_08 TYPE t16ft-frgxt,
       END OF ty_saida.

TYPES: BEGIN OF ty_saidac,
         bukrs     TYPE csks-bukrs,
         butxt     TYPE t001-butxt,
         werks     TYPE zprovcoupa01-werks,
         name1     TYPE t001w-name1,
         tp_estrat TYPE zprovcoupa01-tp_estrat,
         uname     TYPE zmmt0003-uname,
         kostl     TYPE csks-kostl,
         ltext     TYPE zprovcoupa01-ltext,
         tp_oper   TYPE zprovcoupa01-tp_oper,
         nivel     TYPE zprovcoupa01-nivel,
         aprovador TYPE zprovcoupa01-aprovador,
         dt_atual  TYPE zprovcoupa01-dt_atual,
         hr_atual  TYPE zprovcoupa01-hr_atual,
         usnam     TYPE zprovcoupa01-usnam,
       END OF ty_saidac.

TYPES: BEGIN OF ty_pedido.
TYPES: pedido_nome_01  TYPE t16fd-frgct,
       pedido_valor_01 TYPE t16ft-frgxt,
       pedido_nome_02  TYPE t16fd-frgct,
       pedido_valor_02 TYPE t16ft-frgxt,
       pedido_nome_03  TYPE t16fd-frgct,
       pedido_valor_03 TYPE t16ft-frgxt,
       pedido_nome_04  TYPE t16fd-frgct,
       pedido_valor_04 TYPE t16ft-frgxt,
       pedido_nome_05  TYPE t16fd-frgct,
       pedido_valor_05 TYPE t16ft-frgxt,
       pedido_nome_06  TYPE t16fd-frgct,
       pedido_valor_06 TYPE t16ft-frgxt,
       pedido_nome_07  TYPE t16fd-frgct,
       pedido_valor_07 TYPE t16ft-frgxt,
       pedido_nome_08  TYPE t16fd-frgct,
       pedido_valor_08 TYPE t16ft-frgxt.
TYPES: END OF ty_pedido.

TYPES: BEGIN OF ty_aux,
         objek TYPE ausp-objek,
         frggr TYPE frggr,
         frgsx TYPE frgsx,
       END OF ty_aux.

TYPES: BEGIN OF ty_aux1,
         objek TYPE ausp-objek,
         frggr TYPE frggr,
         frgsx TYPE frgsx,
         frgxt TYPE frgxt,
         frgc1 TYPE t16fs-frgc1,
         frgc2 TYPE t16fs-frgc2,
         frgc3 TYPE t16fs-frgc3,
         frgc4 TYPE t16fs-frgc4,
         frgc5 TYPE t16fs-frgc5,
         frgc6 TYPE t16fs-frgc6,
         frgc7 TYPE t16fs-frgc7,
         frgc8 TYPE t16fs-frgc8,
       END OF ty_aux1.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA:
  it_zmmt0003   TYPE TABLE OF zmmt0003,
***  it_ausp_aux   TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp       TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp_2     TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp_2_aux TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp_3     TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp_4     TYPE TABLE OF ausp WITH HEADER LINE,
  it_ausp_4_aux TYPE TABLE OF ausp WITH HEADER LINE,
  it_t16fs      TYPE TABLE OF t16fs,
  it_t16fc      TYPE TABLE OF t16fc,
  it_t16fd      TYPE TABLE OF t16fd,
  it_csks       TYPE TABLE OF csks,
  it_t001w      TYPE TABLE OF t001w,
  it_t001       TYPE TABLE OF t001,
  it_cskt       TYPE TABLE OF cskt,
  it_tka01      TYPE TABLE OF tka01,
***  it_aux        TYPE TABLE OF ty_aux WITH HEADER LINE,
  it_t16ft      TYPE TABLE OF t16ft,
  it_t16ft_aux  TYPE TABLE OF t16ft,
  it_aux1       TYPE TABLE OF ty_aux1 WITH HEADER LINE,
  it_user_addr  TYPE TABLE OF user_addr,
  it_saidac     TYPE TABLE OF ty_saidac, " RJF
  it_saida      TYPE TABLE OF ty_saida.
*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: wa_zmmt0003       TYPE zmmt0003,
      wa_ausp           TYPE ausp,
      wa_t16fs          TYPE t16fs,
      wa_t16fs_ant      TYPE t16fs,
      wa_t16fc          TYPE t16fc,
      wa_t16ft          TYPE t16ft,
      wa_t16fd          TYPE t16fd,
      wa_tka01          TYPE tka01,
      wa_csks           TYPE csks,
      wa_t001w          TYPE t001w,
      wa_t001           TYPE t001,
      wa_ped            TYPE ty_pedido,
      wa_cskt           TYPE cskt,
      wa_aux            TYPE ty_aux,
      wa_ausp_aux       TYPE ausp,
      wa_ausp_2         TYPE ausp,
      wa_ausp_3         TYPE ausp,
      wa_saida          TYPE ty_saida,
      wa_saidac         TYPE ty_saidac,
      wa_saida_aux      TYPE ty_saida,
      wa_aux1           TYPE ty_aux1,
      wa_t16ft_aux      TYPE t16ft,
      wa_user_addr      TYPE user_addr,
      wa_pedido_nome_01 TYPE t16fs.
*=============================================================================*
*WORK AREA  TELA                                                              *
*=============================================================================*
DATA: wa_cont   TYPE REF TO cl_gui_custom_container,
      wa_alv    TYPE REF TO  cl_gui_alv_grid,
      lv_exit,
      lv_estrat TYPE ztp_estrat,
      wa_layout TYPE lvc_s_layo.
*----------------------------------------------------------------------*
***INCLUDE Zmmr101_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.
DATA: vg_atinn LIKE ausp-atinn.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
*    PERFORM HANDLE_HOTSPOT_CLICK
*       USING ES_ROW_NO-ROW_ID
*             E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click
*  METHOD TOP_OF_PAGE.
*    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.
*  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.
*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat    TYPE TABLE OF lvc_s_fcat.
DATA:it_list    TYPE vrm_values,
     list_value TYPE vrm_values.
*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

* Ini - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    SELECTION-SCREEN SKIP.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: s_sap RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND u2.
      SELECTION-SCREEN COMMENT 4(15) TEXT-004 FOR FIELD s_sap.

      PARAMETERS: s_coupa RADIOBUTTON GROUP g2.
      SELECTION-SCREEN COMMENT 25(12) TEXT-005 FOR FIELD s_coupa.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b3.
* Fim - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP

  SELECT-OPTIONS : s_kokrs FOR csks-kokrs  NO INTERVALS NO-EXTENSION MODIF ID m1 ,   "Área contab. custos
                   s_bukrs FOR csks-bukrs  MODIF ID m1,
                   s_tipo  FOR ausp-atwrt  MODIF ID m2,                               "Empresa
                   s_kostl FOR csks-kostl  MODIF ID m1,                              "Centro de custo
                   s_aprov FOR t16ft-frgxt MODIF ID m1 MATCHCODE OBJECT zsh_aprovador_sap,
                   s_werks FOR t001w-werks MODIF ID m2.                              "Centro


** Ini - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-006.
*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: s_custoc RADIOBUTTON GROUP g3 DEFAULT 'X' USER-COMMAND u3 MODIF ID m3.
*SELECTION-SCREEN COMMENT 4(15) text-007 FOR FIELD s_custoc MODIF ID m3.
*
*PARAMETERS: s_estoqu RADIOBUTTON GROUP g3 MODIF ID m3.
*SELECTION-SCREEN COMMENT 25(7) text-008 FOR FIELD s_estoqu MODIF ID m3.
*
*PARAMETERS: s_cusest RADIOBUTTON GROUP g3 MODIF ID m3.
*SELECTION-SCREEN COMMENT 40(18) text-009 FOR FIELD s_cusest MODIF ID m3.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b4.

*...
  SELECT-OPTIONS :
                   s_bukrs3 FOR csks-bukrs  MODIF ID m3,                              " Empresa
                   s_kostl3 FOR csks-kostl  MODIF ID m3,                              " Centro de custo
                   s_werks3 FOR t001w-werks MODIF ID m3,                              " Centro
                   s_aprova FOR zprovcoupa01-aprovador NO INTERVALS NO-EXTENSION MODIF ID m3 MATCHCODE OBJECT zsh_aprovador_coupa.                   " Aprovador

* Fim - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP - 23.01.2023
  SELECTION-SCREEN : COMMENT 55(79) TEXT-013 FOR FIELD s_aprov MODIF ID m1.


SELECTION-SCREEN:END OF BLOCK b1.
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN SKIP.
  PARAMETERS: s_ccusto RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1 MODIF ID m1,
              s_centro RADIOBUTTON GROUP g1 MODIF ID m1.
  PARAMETERS: s_tela TYPE c AS CHECKBOX DEFAULT abap_true MODIF ID m1.
  SELECTION-SCREEN SKIP.
SELECTION-SCREEN:END OF BLOCK b2.
*=============================================================================*
*AT SELECTION-SCREEN                                                          *
*=============================================================================*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tipo-low.
  PERFORM f4_tipo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tipo-high.
  PERFORM f4_tipo.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF s_ccusto = 'X'  .
      IF screen-group1 = 'M1'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'M2'.
        REFRESH: s_tipo, s_werks.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'M1' .
        REFRESH: s_kostl, s_bukrs, s_kokrs.
        screen-input = 0.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'M2' .
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

* Ini - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP - 23.01.2023
*s_sap
    IF s_sap = 'X'.
      IF screen-group1 = 'M3'.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
*      ELSEIF screen-group1 = 'M2'.
*        REFRESH: s_tipo, s_werks.
*        screen-visible = 0.
*        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 = 'M3' .
*        REFRESH: s_kostl, s_bukrs, s_kokrs.
        screen-invisible = 0.
        MODIFY SCREEN.
*      ELSEIF screen-group1 = 'M2' .
*        screen-input = 1.
*        MODIFY SCREEN.
      ENDIF.

      IF screen-group1 EQ 'M1' OR screen-group1 EQ 'M2'.
*        REFRESH: s_kostl, s_bukrs, s_kokrs.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
*      ELSEIF screen-group1 = 'M2' .
*        screen-input = 1.
*        MODIFY SCREEN.
      ENDIF.
    ENDIF.
* Fim - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP - 23.01.2023
  ENDLOOP.
*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  LOOP AT s_tipo.
    TRANSLATE s_tipo-low TO UPPER CASE.
    TRANSLATE s_tipo-high TO UPPER CASE.
    MODIFY s_tipo INDEX sy-tabix.
  ENDLOOP.

* Ini - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP - 23.01.2023
  PERFORM f_trata_screen.
* Fim - RJF - 94149 - CS2022000824 - Melhoria na transação ZMM0071 no SAP - 23.01.2023

  IF lv_exit IS INITIAL.
    gf_first_display = 'X'.

    PERFORM: f_selecionar_dados,               " Form selecionar dado
             f_organizar_dados,                " ORGANIZAR DADOS
             f_alv.                            "Saida ALV
  ENDIF.

END-OF-SELECTION.
  IF lv_exit IS INITIAL.
    CALL SCREEN 0100.
    CALL METHOD ctl_alv_resumo->refresh_table_display.
    CALL METHOD cl_gui_cfw=>dispatch.
  ENDIF.
*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_selecionar_dados.

  REFRESH: it_ausp, it_ausp_2, it_ausp_3, it_ausp_4.

  DATA lr_uname TYPE RANGE OF uname.

* RJF - Ini
  IF s_coupa IS NOT INITIAL.

*    IF s_aprova IS INITIAL.
*      IF s_cusest IS INITIAL.
*        SELECT *
*          FROM zprovcoupa01
*          INTO TABLE @DATA(it_zprovcoupa01)
*          WHERE tp_estrat = @lv_estrat
*            AND kostl     IN @s_kostl3
*            AND werks     IN @s_werks3
*            AND aprovador IN @s_aprova.
*      ELSE.
*        SELECT *
*          FROM zprovcoupa01
*          INTO TABLE @DATA(it_zprovcoupa01)
*          WHERE ( tp_estrat EQ @lv_estrat
*              OR tp_estrat EQ 'E' )
*            AND kostl     IN @s_kostl3
*            AND werks     IN @s_werks3
*            AND aprovador IN @s_aprova.
*      ENDIF.
*    ELSE.
    SELECT *
      FROM zprovcoupa01
      INTO TABLE @DATA(it_zprovcoupa01)
      WHERE kostl     IN @s_kostl3
        AND werks     IN @s_werks3
        AND aprovador IN @s_aprova.
*    ENDIF.

    SELECT *
      FROM t001
      INTO TABLE it_t001
    WHERE bukrs IN s_bukrs3.


    SELECT *
      FROM t001w
      INTO TABLE it_t001w
    WHERE werks IN s_werks3.


    LOOP AT it_zprovcoupa01 ASSIGNING FIELD-SYMBOL(<fs_zprovcoupa>).

      MOVE-CORRESPONDING <fs_zprovcoupa> TO wa_saidac.

      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = <fs_zprovcoupa>-werks.

      IF sy-subrc IS INITIAL.
        MOVE: wa_t001w-werks TO wa_saidac-werks,
              wa_t001w-name1 TO wa_saidac-name1.
        READ TABLE it_t001 INTO wa_t001 INDEX 1. "WITH KEY werks = wa_t001-werks.
        IF sy-subrc IS INITIAL.
          MOVE: wa_t001-bukrs TO wa_saidac-bukrs,
                wa_t001-butxt TO wa_saidac-butxt.
        ENDIF.
      ENDIF.

      APPEND wa_saidac TO it_saidac.
      CLEAR wa_saidac.
    ENDLOOP.

* RJF - Fim
  ELSE.

    IF s_centro EQ 'X'.

      SELECT *
        FROM t001w
        INTO TABLE it_t001w
      WHERE werks IN s_werks.

      "Pedidos
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMMCCUSTOPO'
        IMPORTING
          output = vg_atinn.

      CALL FUNCTION 'CLSE_SELECT_AUSP'
        EXPORTING
          klart                     = '032'
          atinn                     = vg_atinn
        TABLES
          t_ausp                    = it_ausp_2
        EXCEPTIONS
          no_entry_found            = 1
          parameters_not_sufficient = 2
          OTHERS                    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Centro
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMMCENTRO'
        IMPORTING
          output = vg_atinn.

      CALL FUNCTION 'CLSE_SELECT_AUSP'
        EXPORTING
          klart                     = '032'
          atinn                     = vg_atinn
        TABLES
          t_ausp                    = it_ausp_3
        EXCEPTIONS
          no_entry_found            = 1
          parameters_not_sufficient = 2
          OTHERS                    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Tipos
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMMTIPODOC'
        IMPORTING
          output = vg_atinn.

      CALL FUNCTION 'CLSE_SELECT_AUSP'
        EXPORTING
          klart                     = '032'
          atinn                     = vg_atinn
        TABLES
          t_ausp                    = it_ausp_4
        EXCEPTIONS
          no_entry_found            = 1
          parameters_not_sufficient = 2
          OTHERS                    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.

      SELECT *
             FROM csks
             INTO TABLE it_csks
             WHERE kokrs IN s_kokrs
               AND bukrs IN s_bukrs
               AND kostl IN s_kostl
               AND datbi GE sy-datum
               AND gsber IN s_werks
               AND bukrs IN s_bukrs
          ORDER BY kostl.

      SELECT *
        FROM t001
        INTO TABLE it_t001
        FOR ALL ENTRIES IN it_csks
      WHERE bukrs EQ it_csks-bukrs.


      SELECT *
        FROM t001w
        INTO TABLE it_t001w
        FOR ALL ENTRIES IN it_csks
      WHERE werks EQ it_csks-gsber.


      SELECT *
        INTO TABLE it_cskt
        FROM cskt
         FOR ALL ENTRIES IN it_csks
       WHERE kokrs EQ it_csks-kokrs
         AND kostl EQ it_csks-kostl
      AND datbi EQ it_csks-datbi.

      "Requisiçoes
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMMCCUSTO'
        IMPORTING
          output = vg_atinn.

      CALL FUNCTION 'CLSE_SELECT_AUSP'
        EXPORTING
          klart                     = '032'
          atinn                     = vg_atinn
        TABLES
          t_ausp                    = it_ausp
        EXCEPTIONS
          no_entry_found            = 1
          parameters_not_sufficient = 2
          OTHERS                    = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      "Pedidos
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'ZMMCCUSTOPO'
        IMPORTING
          output = vg_atinn.

      CALL FUNCTION 'CLSE_SELECT_AUSP'
        EXPORTING
          klart                     = '032'
          atinn                     = vg_atinn
        TABLES
          t_ausp                    = it_ausp_2
        EXCEPTIONS
          no_entry_found            = 1
          parameters_not_sufficient = 2
          OTHERS                    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

*******  CLEAR it_ausp_aux[].
*******  MOVE it_ausp[]   TO it_ausp_aux[].
*******  DELETE ADJACENT DUPLICATES FROM it_ausp_aux COMPARING objek.
*******
*******  LOOP AT it_ausp_aux.
*******    it_aux-objek = it_ausp_aux-objek.
*******    it_aux-frggr = it_ausp_aux-objek(2).
*******    it_aux-frgsx = it_ausp_aux-objek+2(2).
*******    APPEND it_aux.
*******  ENDLOOP.
*******
*******  CLEAR it_ausp_aux[].
*******  MOVE it_ausp_2[] TO it_ausp_aux[].
*******  DELETE ADJACENT DUPLICATES FROM it_ausp_aux COMPARING objek.
*******
*******  LOOP AT it_ausp_aux.
*******    it_aux-objek = it_ausp_aux-objek.
*******    it_aux-frggr = it_ausp_aux-objek(2).
*******    it_aux-frgsx = it_ausp_aux-objek+2(2).
*******    APPEND it_aux.
*******  ENDLOOP.
*******
*******  DELETE ADJACENT DUPLICATES FROM it_aux COMPARING ALL FIELDS.

    SELECT *
       FROM t16fs
    INTO TABLE it_t16fs.

    SORT it_t16fs BY frggr frgsx.

    SELECT *
      FROM t16ft
      INTO TABLE it_t16ft
    WHERE spras EQ 'P'.
    SORT it_t16ft BY frggr frgsx.

    SELECT *
      FROM t16fc
    INTO TABLE it_t16fc.
    SORT it_t16fc BY frggr frgco.

    SELECT *
      FROM t16fd
    INTO TABLE it_t16fd.
    SORT it_t16fd BY frggr frgco.

    IF lines( s_aprov ) GT 0.
      SELECT * FROM user_addr
        INTO TABLE @DATA(lt_user_aprov)
        WHERE name_textc IN @s_aprov.
      IF sy-subrc EQ 0.
        LOOP AT lt_user_aprov INTO DATA(ls_user_aprov).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_user_aprov-bname ) TO lr_uname.
        ENDLOOP.
      ENDIF.
    ENDIF.

*    SELECT *
*      FROM zmmt0003
*      INTO TABLE it_zmmt0003
*      FOR ALL ENTRIES IN it_csks
*      WHERE kostl EQ it_csks-kostl
*        AND   dt_val_de  LE sy-datum
*        AND   uname IN s_aprov
*        AND   dt_val_ate GE sy-datum.

    SELECT *
      FROM zmmt0003
      INTO TABLE it_zmmt0003
      FOR ALL ENTRIES IN it_csks
      WHERE kostl EQ it_csks-kostl
        AND   dt_val_de  LE sy-datum
        AND   uname IN lr_uname
        AND   dt_val_ate GE sy-datum.

    IF ( it_zmmt0003[] IS NOT INITIAL ).

      SELECT *
        FROM user_addr
        INTO TABLE it_user_addr
        FOR ALL ENTRIES IN it_zmmt0003[]
        WHERE bname = it_zmmt0003-uname.
      SORT it_user_addr BY name_textc bname.

    ENDIF.
  ENDIF.


ENDFORM.                    "F_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
FORM f_organizar_dados.

  DATA: aux_atwrt        TYPE atwrt,
        vg_subrc_ausp_01 TYPE sysubrc,
        vg_subrc_ausp_02 TYPE sysubrc,
        vg_sy_tabix      TYPE sytabix,
        var_valor        TYPE t16ft-frgxt,
        vg_maior         TYPE i,
        i                TYPE i.

  DATA rl_aprov TYPE RANGE OF t16fd-frgct.

  CLEAR: it_t16ft_aux.
  MOVE it_t16ft TO it_t16ft_aux.


  IF s_centro EQ 'X'.

    it_ausp_2_aux[] = it_ausp_2[].
    it_ausp_4_aux[] = it_ausp_4[].

    "Exclui Centros com Centros de Custo
    LOOP AT it_ausp_3.
      vg_sy_tabix = sy-tabix.
      READ TABLE it_ausp_2_aux WITH KEY objek = it_ausp_3-objek BINARY SEARCH.
      IF sy-subrc = 0.
        it_ausp_3-lkenz = 'X'.
        MODIFY it_ausp_3 INDEX vg_sy_tabix TRANSPORTING lkenz.
        CONTINUE.
      ENDIF.

      wa_t16ft-frggr = it_ausp_3-objek(2).
      wa_t16ft-frgsx = it_ausp_3-objek+2(2).

      READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16ft-frggr
                                                 frgsx = wa_t16ft-frgsx BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        PERFORM retorna_posicao USING wa_t16fs CHANGING vg_maior.
        it_ausp_3-adzhl = vg_maior.
        MODIFY it_ausp_3 INDEX vg_sy_tabix TRANSPORTING adzhl.
      ENDIF.
    ENDLOOP.

    DELETE it_ausp_3 WHERE lkenz = 'X'.

    "Exclui Centro que não tem o Tipo selecionado
    IF s_tipo IS NOT INITIAL.
      DELETE it_ausp_4_aux WHERE atwrt NOT IN  s_tipo.
      LOOP AT it_ausp_3.
        vg_sy_tabix = sy-tabix.
        READ TABLE it_ausp_4_aux WITH KEY objek = it_ausp_3-objek BINARY SEARCH.
        IF sy-subrc = 0.
          it_ausp_3-lkenz = 'Y'.
          MODIFY it_ausp_3 INDEX vg_sy_tabix TRANSPORTING lkenz.
          CONTINUE.
        ENDIF.
      ENDLOOP.
      DELETE it_ausp_3 WHERE lkenz NE 'Y'.
    ENDIF.

    "Pedidos
    LOOP AT it_t001w INTO wa_t001w.
      aux_atwrt = wa_t001w-werks.
      CLEAR: wa_saida.
      READ TABLE it_ausp_3 WITH KEY atwrt = aux_atwrt.
      vg_subrc_ausp_02 = sy-subrc.
      IF vg_subrc_ausp_02 IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      wa_saida-werks = wa_t001w-werks.           "Centro
      wa_saida-name1 = wa_t001w-name1.           "Descrição
      APPEND wa_saida TO it_saida.
    ENDLOOP.

    IF s_tela EQ abap_true.

      LOOP AT it_t001w INTO wa_t001w.
        aux_atwrt = wa_t001w-werks.
        CLEAR: wa_t16fs_ant.

        LOOP AT it_ausp_3 WHERE atwrt = aux_atwrt.
          wa_t16fs-frggr = it_ausp_3-objek(2).
          wa_t16fs-frgsx = it_ausp_3-objek+2(2).

          READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16fs-frggr
                                                     frgsx = wa_t16fs-frgsx BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF wa_t16fs_ant IS INITIAL.
              MOVE wa_t16fs TO wa_t16fs_ant.
            ELSE.
              PERFORM verifica_mais_preenchido_2 USING wa_t16fs_ant wa_t16fs CHANGING vg_maior.
              CASE vg_maior.
                WHEN 2.
                  MOVE wa_t16fs TO wa_t16fs_ant.
              ENDCASE.
            ENDIF.
          ENDIF.
        ENDLOOP.
        "(2)
        CLEAR: wa_ped.
        IF wa_t16fs_ant IS NOT INITIAL.
          PERFORM busca_valor_nome_pedido_centro USING wa_t16fs_ant-frggr wa_t16fs_ant-frgsx aux_atwrt CHANGING wa_ped.
        ENDIF.
        IF wa_ped IS NOT INITIAL.
          "PEDIDO
          READ TABLE it_saida INTO wa_saida WITH KEY werks = wa_t001w-werks.
          IF sy-subrc IS INITIAL.
            vg_sy_tabix = sy-tabix.
            IF wa_saida-pedido_nome_01 IS INITIAL.
              MOVE-CORRESPONDING wa_ped TO wa_saida.
              MODIFY it_saida INDEX sy-tabix FROM wa_saida.

              IF wa_saida-pedido_valor_01 IS INITIAL.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                MODIFY it_saida INDEX sy-tabix FROM wa_saida.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.

      LOOP AT it_t001w INTO wa_t001w.
        aux_atwrt = wa_t001w-werks.
        i = 1.
        WHILE i LE 8.
          LOOP AT it_ausp_3 WHERE atwrt = aux_atwrt AND adzhl EQ i.
            wa_t16fs-frggr = it_ausp_3-objek(2).
            wa_t16fs-frgsx = it_ausp_3-objek+2(2).
            "(2)
            PERFORM busca_valor_nome_pedido_centro USING wa_t16fs-frggr wa_t16fs-frgsx aux_atwrt CHANGING wa_ped.
            IF wa_ped IS NOT INITIAL.
              "(1)PEDIDO
              READ TABLE it_saida INTO wa_saida WITH KEY werks = wa_t001w-werks.
              IF sy-subrc IS INITIAL.
                vg_sy_tabix = sy-tabix.
                IF wa_saida-pedido_nome_01 IS INITIAL.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  MODIFY it_saida INDEX sy-tabix FROM wa_saida.

                  IF wa_saida-pedido_valor_01 IS INITIAL.
                    MOVE-CORRESPONDING wa_ped TO wa_saida.
                    MODIFY it_saida INDEX sy-tabix FROM wa_saida.
                  ENDIF.
                ELSE.
                  MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
                  CLEAR: wa_saida.
                  wa_saida-werks           = wa_saida_aux-werks.
                  wa_saida-name1           = wa_saida_aux-name1.
                  wa_saida-objek           = wa_saida_aux-objek.
                  wa_saida-atwrt           = wa_saida_aux-atwrt.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  INSERT wa_saida INTO it_saida INDEX sy-tabix.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
          ADD 1 TO i.
        ENDWHILE.
      ENDLOOP.

    ENDIF.

  ELSE.

    LOOP AT it_ausp_2.
      wa_t16ft-frggr = it_ausp_2-objek(2).
      wa_t16ft-frgsx = it_ausp_2-objek+2(2).
      vg_sy_tabix = sy-tabix.
      READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16ft-frggr
                                                 frgsx = wa_t16ft-frgsx BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        PERFORM retorna_posicao USING wa_t16fs CHANGING vg_maior.
        it_ausp_2-adzhl = vg_maior.
        MODIFY it_ausp_2 INDEX vg_sy_tabix TRANSPORTING adzhl.
      ENDIF.
    ENDLOOP.

    LOOP AT it_csks INTO wa_csks.

      aux_atwrt = wa_csks-kostl.

      CLEAR: wa_saida.

      READ TABLE it_ausp WITH KEY atwrt = aux_atwrt.
      vg_subrc_ausp_01 = sy-subrc.

      READ TABLE it_ausp_2 WITH KEY atwrt = aux_atwrt.
      vg_subrc_ausp_02 = sy-subrc.

      IF vg_subrc_ausp_02 IS NOT INITIAL AND vg_subrc_ausp_01 IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      "CENTRO DE CUSTO E DESCRIÇÃO
      READ TABLE it_cskt INTO wa_cskt WITH KEY kokrs = wa_csks-kokrs
                                               kostl = wa_csks-kostl
                                               datbi = wa_csks-datbi.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      wa_saida-kostl = wa_csks-kostl.           "Centro de custo
      wa_saida-ktext = wa_cskt-ktext.           "Descrição
      wa_saida-bukrs = wa_csks-bukrs.           "Empresa
      wa_saida-gsber = wa_csks-gsber.           "Centro
      IF wa_csks-bkzkp = 'X'.
        wa_saida-bloq  = 'X'.
      ELSE.
        CLEAR  wa_saida-bloq.
      ENDIF.
      "COD. CLIENTE.
      LOOP AT it_zmmt0003 INTO wa_zmmt0003 WHERE kostl = wa_csks-kostl AND uname NE ''.
        wa_saida-uname = wa_zmmt0003-uname.

        "Reservas
        READ TABLE it_user_addr INTO wa_user_addr WITH KEY bname = wa_zmmt0003-uname.
        IF sy-subrc IS INITIAL.
          wa_saida-name_textc = wa_user_addr-name_textc.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ENDLOOP.


      LOOP AT it_t001 INTO wa_t001 WHERE bukrs = wa_csks-bukrs.
        wa_saida-butxt =  wa_t001-butxt.
      ENDLOOP.

      LOOP AT it_t001w INTO wa_t001w WHERE werks = wa_csks-gsber.
        wa_saida-name1 =  wa_t001w-name1.
      ENDLOOP.


      IF wa_saida-name_textc IS INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ENDLOOP.


    "Requisição
    LOOP AT it_csks INTO wa_csks.
      aux_atwrt = wa_csks-kostl.
      LOOP AT it_ausp WHERE atwrt = aux_atwrt.
        wa_t16ft-frggr = it_ausp-objek(2).
        wa_t16ft-frgsx = it_ausp-objek+2(2).
        READ TABLE it_t16ft INTO wa_t16ft
                            WITH KEY frggr = wa_t16ft-frggr
                                     frgsx = wa_t16ft-frgsx BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_saida INTO wa_saida WITH KEY kostl = wa_csks-kostl.
          vg_sy_tabix = sy-tabix.
          IF ( wa_saida-requisicao_nome IS INITIAL ) OR ( wa_saida-requisicao_nome = wa_t16ft-frgxt ).
            IF wa_saida-requisicao_nome NE wa_t16ft-frgxt.
              wa_saida-requisicao_nome = wa_t16ft-frgxt.
              MODIFY it_saida INDEX sy-tabix FROM wa_saida TRANSPORTING requisicao_nome.
            ENDIF.
          ELSE.
            wa_saida-requisicao_nome = wa_t16ft-frgxt.
            ADD 1 TO vg_sy_tabix.
            INSERT wa_saida INTO it_saida INDEX vg_sy_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.


    IF s_tela EQ abap_true.

      LOOP AT it_csks INTO wa_csks.
        aux_atwrt = wa_csks-kostl.
        CLEAR: wa_t16fs_ant.

        LOOP AT it_ausp_2 WHERE atwrt = aux_atwrt.
          wa_t16fs-frggr = it_ausp_2-objek(2).
          wa_t16fs-frgsx = it_ausp_2-objek+2(2).

          READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = wa_t16fs-frggr
                                                     frgsx = wa_t16fs-frgsx BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF wa_t16fs_ant IS INITIAL.
              MOVE wa_t16fs TO wa_t16fs_ant.
            ELSE.
              PERFORM verifica_mais_preenchido_2 USING wa_t16fs_ant wa_t16fs CHANGING vg_maior.
              CASE vg_maior.
                WHEN 2.
                  MOVE wa_t16fs TO wa_t16fs_ant.
              ENDCASE.
            ENDIF.
          ENDIF.
        ENDLOOP.
        "(2)
        CLEAR: wa_ped.
        IF wa_t16fs_ant IS NOT INITIAL.
          PERFORM busca_valor_nome_pedido USING wa_t16fs_ant-frggr wa_t16fs_ant-frgsx aux_atwrt CHANGING wa_ped.
        ENDIF.
        IF wa_ped IS NOT INITIAL.

          "PEDIDO
          READ TABLE it_saida INTO wa_saida WITH KEY kostl = wa_csks-kostl.
          IF sy-subrc IS INITIAL.
            vg_sy_tabix = sy-tabix.
            IF wa_saida-pedido_nome_01 IS INITIAL.
              MOVE-CORRESPONDING wa_ped TO wa_saida.
              MODIFY it_saida INDEX sy-tabix FROM wa_saida.

              IF wa_saida-pedido_valor_01 IS INITIAL.
                MOVE-CORRESPONDING wa_ped TO wa_saida.
                MODIFY it_saida INDEX sy-tabix FROM wa_saida.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
      LOOP AT it_csks INTO wa_csks.
        aux_atwrt = wa_csks-kostl.
        i = 1.
        WHILE i LE 8.
          LOOP AT it_ausp_2 WHERE atwrt = aux_atwrt AND adzhl EQ i.

            wa_t16fs-frggr = it_ausp_2-objek(2).
            wa_t16fs-frgsx = it_ausp_2-objek+2(2).
            "(2)
            PERFORM busca_valor_nome_pedido USING wa_t16fs-frggr wa_t16fs-frgsx aux_atwrt CHANGING wa_ped.
            IF wa_ped IS NOT INITIAL.
              "(1)PEDIDO
              READ TABLE it_saida INTO wa_saida WITH KEY kostl = wa_csks-kostl.
              IF sy-subrc IS INITIAL.
                vg_sy_tabix = sy-tabix.
                IF wa_saida-pedido_nome_01 IS INITIAL.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  MODIFY it_saida INDEX sy-tabix FROM wa_saida.

                  IF wa_saida-pedido_valor_01 IS INITIAL.
                    MOVE-CORRESPONDING wa_ped TO wa_saida.
                    MODIFY it_saida INDEX sy-tabix FROM wa_saida.
                  ENDIF.
                ELSE.
                  MOVE-CORRESPONDING wa_saida TO wa_saida_aux.
                  CLEAR: wa_saida.
                  wa_saida-uname           = wa_saida_aux-uname.
                  wa_saida-kostl           = wa_saida_aux-kostl.
                  wa_saida-ktext           = wa_saida_aux-ktext.
                  wa_saida-objek           = wa_saida_aux-objek.
                  wa_saida-atwrt           = wa_saida_aux-atwrt.
                  wa_saida-requisicao_nome = wa_saida_aux-requisicao_nome.
                  MOVE-CORRESPONDING wa_ped TO wa_saida.
                  INSERT wa_saida INTO it_saida INDEX sy-tabix.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
          ADD 1 TO i.
        ENDWHILE.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF s_aprov IS NOT INITIAL.
    SORT it_saida BY pedido_nome_02 pedido_nome_03 pedido_nome_03 pedido_nome_04 pedido_nome_05 pedido_nome_06 pedido_nome_07 pedido_nome_08.
    DELETE it_saida
    WHERE   pedido_nome_01 NOT IN s_aprov AND
    pedido_nome_02 NOT IN s_aprov AND
    pedido_nome_03 NOT IN s_aprov AND
    pedido_nome_04 NOT IN s_aprov AND
    pedido_nome_05 NOT IN s_aprov AND
    pedido_nome_06 NOT IN s_aprov AND
    pedido_nome_07 NOT IN s_aprov AND
    pedido_nome_08 NOT IN s_aprov.

*    DELETE it_saida
*    WHERE   pedido_nome_01 NOT IN rl_aprov AND
*    pedido_nome_02 NOT IN rl_aprov AND
*    pedido_nome_03 NOT IN rl_aprov AND
*    pedido_nome_04 NOT IN rl_aprov AND
*    pedido_nome_05 NOT IN rl_aprov AND
*    pedido_nome_06 NOT IN rl_aprov AND
*    pedido_nome_07 NOT IN rl_aprov AND
*    pedido_nome_08 NOT IN rl_aprov.

  ENDIF.

  "PSA
  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<it_saida>).

    IF s_sap IS NOT INITIAL.

      IF s_ccusto IS NOT INITIAL.
        IF <it_saida>-name1 IS INITIAL.
          SELECT SINGLE name1 FROM t001w WHERE werks = @<it_saida>-gsber INTO @<it_saida>-name1.
        ENDIF.

        IF <it_saida>-butxt IS INITIAL AND <it_saida>-bukrs IS NOT INITIAL.
          SELECT SINGLE butxt FROM t001 WHERE bukrs = @<it_saida>-bukrs INTO @<it_saida>-butxt.
        ENDIF.
      ENDIF.

      IF s_centro IS NOT INITIAL.
        IF <it_saida>-bukrs IS INITIAL.
          SELECT SINGLE b~bukrs,butxt FROM t001w AS a
            INNER JOIN t001 AS b ON b~bukrs = a~vkorg
            WHERE a~werks = @<it_saida>-werks
            INTO @DATA(aux_emp).
          <it_saida>-bukrs = aux_emp-bukrs.
          <it_saida>-butxt = aux_emp-butxt.
          IF <it_saida>-butxt IS INITIAL.
            SELECT SINGLE butxt FROM t001 WHERE bukrs = @<it_saida>-bukrs INTO @<it_saida>-butxt.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    IF <it_saida>-werks IS NOT INITIAL AND <it_saida>-gsber IS INITIAL.
      <it_saida>-gsber = <it_saida>-werks.
    ENDIF.

    IF <it_saida>-gsber IS NOT INITIAL AND <it_saida>-werks IS INITIAL.
      <it_saida>-werks = <it_saida>-gsber.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "F_ORGANIZAR_DADOS


*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv.

  "IF s_coupa IS INITIAL.
  IF s_sap IS NOT INITIAL.
*    IF s_ccusto EQ 'X'.

    IF s_ccusto IS NOT INITIAL OR s_centro IS NOT INITIAL.
      PERFORM alv_preenche_cat USING:
      'BUKRS'            'Empresa'                    '20'  '' ''   '' ,
      'BUTXT'            'Descrição Empresa'          '20'  '' ''   '' ,
      'WERKS'            'Centro'                     '20'  '' ''   '' , "PSA 27122023 - GSBER
      'NAME1'            'Descrição Centro'           '20'  '' ''   '' ,
      'KOSTL'            'Centro de custos'           '11'  '' ''   '' ,
      'KTEXT'            'Descrição'                  '20'  '' ''   '' ,
      'BLOQ'             'Bloqueado'                  '10'  '' ''   '' ,
      'NAME_TEXTC'       'Reservas'                   '20'  '' ''   '' ,
      'REQUISICAO_NOME'  'Requisição'                 '20'  '' ''   '' ,
      'PEDIDO_NOME_01'   'Pedidos1'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_01'  'Valor1'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_02'   'Pedidos2'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_02'  'Valor2'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_03'   'Pedidos3'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_03'  'Valor3'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_04'   'Pedidos4'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_04'  'Valor4'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_05'   'Pedidos5'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_05'  'Valor5'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_06'   'Pedidos6'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_06'  'Valor6'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_07'   'Pedidos7'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_07'  'Valor7'                     '20'  '' ''   '' ,
      'PEDIDO_NOME_08'   'Pedidos8'                   '20'  '' ''   '' ,
      'PEDIDO_VALOR_08'  'Valor8'                     '20'  '' ''   '' .
*    ELSE.
*      PERFORM alv_preenche_cat USING:
*        'WERKS'            'Centro'                     '11'  '' ''   '' ,
*        'NAME1'            'Descrição'                  '10'  '' ''   '' ,
*        'PEDIDO_VALOR_01'  'Valor1'                     '20'  '' ''   '' ,
*        'PEDIDO_NOME_01'   'Pedidos1'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_02'   'Pedidos2'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_03'   'Pedidos3'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_04'   'Pedidos4'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_05'   'Pedidos5'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_06'   'Pedidos6'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_07'   'Pedidos7'                   '20'  '' ''   '' ,
*        'PEDIDO_NOME_08'   'Pedidos8'                   '20'  '' ''   '' .

    ENDIF.

* RJF - Ini
  ENDIF.

  IF s_coupa IS NOT INITIAL.
    PERFORM alv_preenche_cat USING:
          'BUKRS'            'Empresa'                    '07'  '' ''   '' ,
          'BUTXT'            'Descrição Empresa'          '17'  '' ''   '' ,
          'WERKS'            'Centro'                     '06'  '' ''   '' ,
          'NAME1'            'Descrição Centro'           '17'  '' ''   '' ,
          'TP_ESTRAT'        'Estratégia'                 '10'  '' ''   '' ,
          'KOSTL'            'Centro de custos'           '17'  '' ''   '' ,
          'LTEXT'            'Descrição'                  '10'  '' ''   '' ,
          'TP_OPER'          'Tipo Oper.'                 '09'  '' ''   '' ,
          'NIVEL'            'Nível Aprov.'               '12'  '' ''   '' ,
          'APROVADOR'        'Aprovador'                  '09'  '' ''   '' ,
          'DT_ATUAL'         'Dt. Atualização'            '15'  '' ''   '' ,
          'HR_ATUAL'         'HR. Atualização'            '15'  '' ''   '' ,
          'USNAM'            'Usuário'                    '08'  '' ''   '' .
  ENDIF.
* RJF - Fim

ENDFORM.                    "F_ALV


"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo  TYPE c
                                p_desc   TYPE c
                                p_tam    TYPE c
                                p_hot    TYPE c
                                p_zero   TYPE c
                                p_sum    TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-outputlen = p_tam.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text  TYPE sdydo_text_element
                    p_style TYPE sdydo_attribute
                    p_size  TYPE sdydo_attribute
                    p_color TYPE sdydo_attribute.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      sap_fontstyle = cl_dd_area=>sans_serif.

  "SAP_STYLE    = CL_DD_AREA=>HEADING
  "SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
  "SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
ENDFORM.                    " ADD_TEXT


*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF gf_first_display = 'X'.

*   Create object for container
    CREATE OBJECT ctl_cccontainer
      EXPORTING
        container_name = 'TELA_0100'.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = ctl_cccontainer
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

*    CALL METHOD dg_splitter_2->set_column_width
*      EXPORTING
*        id    = 1
*        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_grid.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 15.


*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_resumo
      EXPORTING
        i_parent = dg_parent_grid.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

    "GS_LAYOUT-SEL_MODE = 'A'.
    gs_layout-zebra      = 'X'.
    "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*   Create Object for Event Handler
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_resumo.
*    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE          FOR CTL_ALV_RESUMO.
    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = dg_parent_grid
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    APPEND ctl_alv_resumo->mc_mb_export TO it_exclude_fcode.

    IF s_coupa IS INITIAL.
**   Send data to ALV grid
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida.
    ELSE.
**   Send data to ALV grid
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saidac.
    ENDIF.

    PERFORM cria_html_cab.

    CALL METHOD ctl_alv_resumo->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = dg_dyndoc_id.

    CLEAR: gf_first_display.

  ENDIF.

*  CALL METHOD ctl_alv_resumo->refresh_table_display.

*  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
*    EXPORTING
*      is_col_info = gs_scroll_col
*      is_row_no   = gs_scroll_row.

  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD ctl_alv_resumo->refresh_table_display.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        qtd                     TYPE i.

  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  IF s_coupa IS INITIAL.
    p_text = 'Relatório Estratégia de liberação'.
  ELSE.
    p_text = 'Relatório de Estratégias – COUPA'.
  ENDIF.

  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.
*
*  sdydo_text_element = 'Empresa:'.
*  APPEND sdydo_text_element TO p_text_table.
*
*  IF s_coupa IS INITIAL.
*    sdydo_text_element = 'Área contab. custos : '.
*    APPEND sdydo_text_element TO p_text_table.
*  ENDIF.
*
*  sdydo_text_element = 'Centro de custo: '.
*  APPEND sdydo_text_element TO p_text_table.
*
*  sdydo_text_element = 'Centro: '.
*  APPEND sdydo_text_element TO p_text_table.
*
*  IF s_coupa IS NOT INITIAL.
*    sdydo_text_element = 'Aprovador: '.
*    APPEND sdydo_text_element TO p_text_table.
*  ENDIF.
*
*  CALL METHOD column_1->add_text
*    EXPORTING
*      text_table = p_text_table
*      fix_lines  = 'X'.
*
*  CLEAR: p_text_table, sdydo_text_element.
*
*  "Empresa*********
*  DESCRIBE TABLE s_bukrs LINES qtd.
*
*  IF qtd = 1.
*    READ TABLE s_bukrs INDEX 1.
*    SELECT SINGLE * INTO wa_csks FROM csks WHERE bukrs EQ s_bukrs-low.
*    sdydo_text_element = wa_csks-bukrs.
*  ELSE.
*    LOOP AT s_bukrs.
*      IF sdydo_text_element IS INITIAL.
*        SELECT SINGLE * INTO wa_csks FROM csks WHERE bukrs EQ s_bukrs-low.
*        sdydo_text_element = s_bukrs-low.
*      ELSE.
*        CONCATENATE sdydo_text_element 'Empresa' INTO sdydo_text_element SEPARATED BY space.
*        CONCATENATE sdydo_text_element s_bukrs-low INTO sdydo_text_element.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*  APPEND sdydo_text_element TO p_text_table.
*
*  IF s_coupa IS NOT INITIAL AND qtd GE 1.
*    sdydo_text_element = s_bukrs3-low.
*    APPEND sdydo_text_element TO p_text_table.
*  ENDIF.
*
*  IF s_coupa IS INITIAL.
*    "Área de contab. custos *****
*    sdydo_text_element = s_kokrs-low.
*    APPEND sdydo_text_element TO p_text_table.
*  ENDIF.
*
*  "Centro de custos *********
*  sdydo_text_element = s_kostl-low.
*  APPEND sdydo_text_element TO p_text_table.
*
*  "Centro *********
*  sdydo_text_element = s_werks-low.
*  APPEND sdydo_text_element TO p_text_table.
*
*  IF s_coupa IS NOT INITIAL.
*    sdydo_text_element = s_aprova-low.
*    APPEND sdydo_text_element TO p_text_table.
*  ENDIF.

  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM



*&---------------------------------------------------------------------*
*&      Form  VERIFICA_MAIS_PREENCHIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VG_MAIOR  text
*      -->P_IT_AUX1  text
*      -->P_WA_T16FS  text
*----------------------------------------------------------------------*
FORM verifica_mais_preenchido_2  USING p_t16fs1 TYPE t16fs
                                       p_t16fs2 TYPE t16fs
                              CHANGING p_maior TYPE i.

  DATA: vg_1 TYPE i,
        vg_2 TYPE i.

  IF p_t16fs1-frgc8 IS NOT INITIAL.
    vg_1 = 8.
  ELSEIF p_t16fs1-frgc7 IS NOT INITIAL.
    vg_1 = 7.
  ELSEIF p_t16fs1-frgc6 IS NOT INITIAL.
    vg_1 = 6.
  ELSEIF p_t16fs1-frgc5 IS NOT INITIAL.
    vg_1 = 5.
  ELSEIF p_t16fs1-frgc4 IS NOT INITIAL.
    vg_1 = 4.
  ELSEIF p_t16fs1-frgc3 IS NOT INITIAL.
    vg_1 = 3.
  ELSEIF p_t16fs1-frgc2 IS NOT INITIAL.
    vg_1 = 2.
  ELSEIF p_t16fs1-frgc1 IS NOT INITIAL.
    vg_1 = 1.
  ENDIF.

  IF p_t16fs2-frgc8 IS NOT INITIAL.
    vg_2 = 8.
  ELSEIF p_t16fs2-frgc7 IS NOT INITIAL.
    vg_2 = 7.
  ELSEIF p_t16fs2-frgc6 IS NOT INITIAL.
    vg_2 = 6.
  ELSEIF p_t16fs2-frgc5 IS NOT INITIAL.
    vg_2 = 5.
  ELSEIF p_t16fs2-frgc4 IS NOT INITIAL.
    vg_2 = 4.
  ELSEIF p_t16fs2-frgc3 IS NOT INITIAL.
    vg_2 = 3.
  ELSEIF p_t16fs2-frgc2 IS NOT INITIAL.
    vg_2 = 2.
  ELSEIF p_t16fs2-frgc1 IS NOT INITIAL.
    vg_2 = 1.
  ENDIF.

  IF vg_1 GE vg_2.
    p_maior = 1.
  ELSE.
    p_maior = 2.
  ENDIF.

ENDFORM.                    " VERIFICA_MAIS_PREENCHIDO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_NOME_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_valor_nome_pedido  USING    p_frggr TYPE frggr
                                       p_frgsx TYPE frgsx
                                       p_csks  TYPE atwrt
                              CHANGING out_ped TYPE ty_pedido.

  CLEAR: out_ped.

  READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    "Valores/Nomes da Estratégia
    "Valor/nome 1

    IF wa_t16fs-frgc1 IS NOT INITIAL.
      wa_ausp_2-adzhl = 1.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc1
                                 CHANGING out_ped-pedido_valor_01 out_ped-pedido_nome_01.
    ENDIF.

    "Valor/nome 2
    IF wa_t16fs-frgc2 IS NOT INITIAL.
      wa_ausp_2-adzhl = 2.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc2
                                 CHANGING out_ped-pedido_valor_02 out_ped-pedido_nome_02.
    ENDIF.

    "Valor/nome 3
    IF wa_t16fs-frgc3 IS NOT INITIAL.
      wa_ausp_2-adzhl = 3.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc3
                                 CHANGING out_ped-pedido_valor_03 out_ped-pedido_nome_03.
    ENDIF.

    "Valor/nome 4
    IF wa_t16fs-frgc4 IS NOT INITIAL.
      wa_ausp_2-adzhl = 4.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc4
                                 CHANGING out_ped-pedido_valor_04 out_ped-pedido_nome_04.
    ENDIF.

    "Valor/nome 5
    IF wa_t16fs-frgc5 IS NOT INITIAL.
      wa_ausp_2-adzhl = 5.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc5
                                 CHANGING out_ped-pedido_valor_05 out_ped-pedido_nome_05.
    ENDIF.

    "Valor/nome 6
    IF wa_t16fs-frgc6 IS NOT INITIAL.
      wa_ausp_2-adzhl = 6.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc6
                                 CHANGING out_ped-pedido_valor_06 out_ped-pedido_nome_06.
    ENDIF.

    "Valor/nome 7
    IF wa_t16fs-frgc7 IS NOT INITIAL.
      wa_ausp_2-adzhl = 7.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc7
                                 CHANGING out_ped-pedido_valor_07 out_ped-pedido_nome_07.
    ENDIF.

    "Valor/nome 8
    IF wa_t16fs-frgc8 IS NOT INITIAL.
      wa_ausp_2-adzhl = 8.
      PERFORM busca_valor_sequencia USING p_csks wa_ausp_2-adzhl wa_t16fs-frggr wa_t16fs-frgc8
                                 CHANGING out_ped-pedido_valor_08 out_ped-pedido_nome_08.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_VALOR_NOME_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  RETORNA_POSICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM retorna_posicao  USING    p_t16fs   TYPE t16fs
                      CHANGING p_posicao TYPE i.

  IF p_t16fs-frgc1 IS NOT INITIAL.
    p_posicao = 1.
  ENDIF.

  IF p_t16fs-frgc2 IS NOT INITIAL.
    p_posicao = 2.
  ENDIF.

  IF p_t16fs-frgc3 IS NOT INITIAL.
    p_posicao = 3.
  ENDIF.

  IF p_t16fs-frgc4 IS NOT INITIAL.
    p_posicao = 4.
  ENDIF.

  IF p_t16fs-frgc5 IS NOT INITIAL.
    p_posicao = 5.
  ENDIF.

  IF p_t16fs-frgc6 IS NOT INITIAL.
    p_posicao = 6.
  ENDIF.

  IF p_t16fs-frgc7 IS NOT INITIAL.
    p_posicao = 7.
  ENDIF.

  IF p_t16fs-frgc8 IS NOT INITIAL.
    p_posicao = 8.
  ENDIF.

ENDFORM.                    " RETORNA_POSICAO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_SEQUENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_valor_sequencia  USING    p_csks    TYPE atwrt
                                     p_adzhl   TYPE adzhl
                                     p_frggr   TYPE frggr
                                     p_frgcx   TYPE frgsx
                            CHANGING p_valor_x TYPE t16ft-frgxt
                                     p_nome_x  TYPE t16fd-frgct.

  READ TABLE it_ausp_2 INTO wa_ausp_2 WITH KEY atwrt = p_csks
                                               adzhl = p_adzhl.
  IF sy-subrc IS INITIAL.
    READ TABLE it_t16ft INTO wa_t16ft WITH KEY frggr = wa_ausp_2-objek(2)
                                               frgsx = wa_ausp_2-objek+2(2) BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      p_valor_x = wa_t16ft-frgxt.
    ENDIF.

    "NOMES PEDIDOS.
    READ TABLE it_t16fd INTO wa_t16fd WITH KEY frggr = p_frggr
                                               frgco = p_frgcx BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      p_nome_x = wa_t16fd-frgct.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_VALOR_SEQUENCIA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_SEQUENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_valor_sequencia_centro  USING    "p_csks    TYPE atwrt
                                            "p_adzhl   TYPE adzhl
                                            p_frgsx   TYPE frgsx
                                            p_frggr   TYPE frggr
                                            p_frgcx   TYPE frgsx
                                   CHANGING p_valor_x TYPE t16ft-frgxt
                                            p_nome_x  TYPE t16fd-frgct.

  "READ TABLE it_ausp_3 INTO wa_ausp_3 WITH KEY atwrt = p_csks
  "                                             adzhl = p_adzhl.

  "IF sy-subrc IS INITIAL.
  READ TABLE it_t16ft INTO wa_t16ft WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH .
  IF sy-subrc IS INITIAL.
    p_valor_x = wa_t16ft-frgxt.
  ENDIF.

  "NOMES PEDIDOS.
  READ TABLE it_t16fd INTO wa_t16fd WITH KEY frggr = p_frggr
                                             frgco = p_frgcx BINARY SEARCH .
  IF sy-subrc IS INITIAL.
    p_nome_x = wa_t16fd-frgct.
  ENDIF.
  "ENDIF.

ENDFORM.                    " BUSCA_VALOR_SEQUENCIA

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_NOME_PEDIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_valor_nome_pedido_centro  USING    p_frggr TYPE frggr
                                       p_frgsx TYPE frgsx
                                       p_csks  TYPE atwrt
                              CHANGING out_ped TYPE ty_pedido.

  CLEAR: out_ped.

  READ TABLE it_t16fs INTO wa_t16fs WITH KEY frggr = p_frggr
                                             frgsx = p_frgsx BINARY SEARCH.

  IF sy-subrc IS INITIAL.
    "Valores/Nomes da Estratégia
    "Valor/nome 1

    IF wa_t16fs-frgc1 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 1.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc1 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_01 out_ped-pedido_nome_01.
    ENDIF.

    "Valor/nome 2
    IF wa_t16fs-frgc2 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 2.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc2 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_02 out_ped-pedido_nome_02.
    ENDIF.

    "Valor/nome 3
    IF wa_t16fs-frgc3 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 3.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc3 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_03 out_ped-pedido_nome_03.
    ENDIF.

    "Valor/nome 4
    IF wa_t16fs-frgc4 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 4.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc4 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_04 out_ped-pedido_nome_04.
    ENDIF.

    "Valor/nome 5
    IF wa_t16fs-frgc5 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 5.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc5 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_05 out_ped-pedido_nome_05.
    ENDIF.

    "Valor/nome 6
    IF wa_t16fs-frgc6 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 6.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc6 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_06 out_ped-pedido_nome_06.
    ENDIF.

    "Valor/nome 7
    IF wa_t16fs-frgc7 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 7.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc7 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_07 out_ped-pedido_nome_07.
    ENDIF.

    "Valor/nome 8
    IF wa_t16fs-frgc8 IS NOT INITIAL.
      "WA_AUSP_2-ADZHL = 8.
      PERFORM busca_valor_sequencia_centro USING wa_t16fs-frgsx wa_t16fs-frggr wa_t16fs-frgc8 "P_CSKS WA_AUSP_2-ADZHL
                                 CHANGING out_ped-pedido_valor_08 out_ped-pedido_nome_08.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_VALOR_NOME_PEDIDO
*&---------------------------------------------------------------------*
*&      Form  F4_TIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_tipo .

  TYPES: BEGIN OF ty_f4_tipo,
           tipo TYPE ausp-atwrt.
  TYPES: END OF ty_f4_tipo.

  DATA: it_tipo  TYPE STANDARD TABLE OF ty_f4_tipo,
        wa_tipo  TYPE ty_f4_tipo,
        vl_atinn LIKE ausp-atinn.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'ZMMTIPODOC'
    IMPORTING
      output = vl_atinn.

  SELECT atwrt
    FROM ausp
    INTO TABLE it_tipo
  WHERE atinn EQ vl_atinn
    AND klart EQ '032'.

  DELETE ADJACENT DUPLICATES FROM it_tipo COMPARING tipo.
  SORT it_tipo BY tipo.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ATWRT'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'S_TIPO'    " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = it_tipo     " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_trata_screen .

  IF s_aprova IS INITIAL.
    IF s_coupa IS NOT INITIAL.
*      IF s_custoc IS NOT INITIAL.
*        IF s_kostl3 IS INITIAL.
*          MESSAGE 'Centro de Custo é campo obrigatório!'(010) TYPE 'I'.
*          lv_exit = abap_true.
*
*        ELSE.
*          PERFORM f_pass_param.
*          lv_estrat = 'K'.
*        ENDIF.
*
*        IF s_bukrs3 IS NOT INITIAL.
**          MESSAGE 'Empresa é campo obrigatório!'(012) TYPE 'I'.
**          lv_exit = abap_true.
**
**        ELSE.
*          PERFORM f_pass_param.
*          lv_estrat = 'K'.
*        ENDIF.
*
*      ENDIF.

*      IF s_estoqu IS NOT INITIAL.
*        IF s_werks3 IS INITIAL.
*          MESSAGE 'Centro é campo obrigatório!'(011) TYPE 'I'.
*          lv_exit = abap_true.
*
*        ELSE.
*          PERFORM f_pass_param.
*          lv_estrat = 'E'.
*        ENDIF.
*
*        IF s_bukrs3 IS NOT INITIAL.
**          MESSAGE 'Empresa é campo obrigatório!'(012) TYPE 'I'.
**          lv_exit = abap_true.
**
**        ELSE.
*          PERFORM f_pass_param.
*          lv_estrat = 'E'.
*        ENDIF.
*
*      ENDIF.

*      IF s_cusest IS NOT INITIAL.
*        IF s_bukrs3 IS NOT INITIAL.
**          MESSAGE 'Empresa é campo obrigatório!'(012) TYPE 'I'.
**          lv_exit = abap_true.
**        ELSE.
*          PERFORM f_pass_param.
*          lv_estrat = 'K'.
*        ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

* BEG - 3000005345 - 29/04/2024 - ABAP
  DATA(lv_nome)      = CONV string( abap_false ).
  DATA(lv_sobrenome) = CONV string( abap_false ).

  IF s_aprova IS NOT INITIAL AND s_coupa IS NOT INITIAL.
    SELECT SINGLE * FROM zprovcoupa01
    INTO @DATA(ls_aprovador)
    WHERE aprovador IN @s_aprova.
    IF sy-subrc NE 0.
      IF s_aprova CS '.'.
        MESSAGE TEXT-015 TYPE 'I'.
        lv_exit = abap_true.
      ELSE.
        LOOP AT s_aprova ASSIGNING FIELD-SYMBOL(<fs_aprova>).
          SPLIT <fs_aprova>-low AT abap_false INTO lv_nome lv_sobrenome.
          TRANSLATE lv_nome TO LOWER CASE.
          TRANSLATE lv_sobrenome TO LOWER CASE.
          CLEAR <fs_aprova>-low.
          CONCATENATE lv_nome lv_sobrenome INTO <fs_aprova>-low SEPARATED BY '.'.
        ENDLOOP.
        SELECT SINGLE * FROM zprovcoupa01
          INTO ls_aprovador
          WHERE aprovador IN s_aprova.
        IF sy-subrc NE 0.
          MESSAGE TEXT-015 TYPE 'I'.
          lv_exit = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* END - 3000005345 - 29/04/2024 - ABAP

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PASS_PARAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_pass_param .
  MOVE: s_kostl3 TO s_kostl,
        s_werks3 TO s_werks,
        s_bukrs3 TO s_bukrs.
ENDFORM.
