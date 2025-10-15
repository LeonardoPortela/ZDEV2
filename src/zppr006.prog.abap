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
**|    + Enio Jesus ( enio.jesus@amaggi.com.br )                              |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatorio de: Etiquetadas, Retiradas, Não Retiradas, À Etiquetar,         |*
**| Etiquetadas + À etiquetar, Lote à Identificar                             |*
**/===========================================================================\*

REPORT zppr006.

TABLES: ekko, ekpo, mchb, mcha, zppt0011, iseg.

TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zppt0011.
TYPES:
         maktx TYPE makt-maktx,
         lifnr TYPE lfa1-lifnr,
         dtval TYPE vfdat,
         name1 TYPE char50,
         color TYPE lvc_t_scol,
       END OF ty_saida,

       ty_t_saida         TYPE TABLE OF ty_saida WITH DEFAULT KEY,
       ty_t_dismemberment TYPE TABLE OF zppt0011 WITH DEFAULT KEY.

DATA gt_outtab TYPE TABLE OF ty_saida.
DATA document TYPE REF TO cl_dd_document.

DATA: t_iseg     TYPE TABLE OF iseg,
      t_zppt0011 TYPE TABLE OF zppt0011.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-i02.
  PARAMETERS:
    p_all  TYPE char1 RADIOBUTTON GROUP gr1 DEFAULT 'X' MODIF ID 01 USER-COMMAND chk,  "Etiquetadas
    p_entr TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 02,              "Retiradas
    p_open TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 03,              "Em deposito
    p_desm TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 04,              "À Etiquetar
    p_gera TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 05,              "Etiquetadas + À etiquetar
    p_lote TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 06,              "Lote á etiquetar
    p_iven TYPE char1 RADIOBUTTON GROUP gr1 MODIF ID 07.              "Iventariado
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-i01.
  SELECT-OPTIONS:
  s_matnr FOR mchb-matnr MODIF ID zg1, "EKPO-MATNR,
  s_werks FOR ekpo-werks MODIF ID zg1,
  s_fabri FOR zppt0011-lfabr MODIF ID zg2,
  s_charg FOR mcha-charg MODIF ID zg1,
  s_lgort FOR ekpo-lgort MODIF ID zg2,
  s_ebeln FOR ekpo-ebeln MODIF ID zg2,
  s_lifnr FOR ekko-lifnr MODIF ID zg2,
  s_vfdat FOR mcha-vfdat MODIF ID zg2.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-i01.
  SELECT-OPTIONS:
  p_iblnr FOR iseg-iblnr MODIF ID zg3 NO-EXTENSION NO INTERVALS, "Documento de inventário
  p_matnr FOR iseg-matnr MODIF ID zg3,"Nº do material
  p_gjahr FOR iseg-gjahr MODIF ID zg3 NO-EXTENSION NO INTERVALS. "Exercício
SELECTION-SCREEN END OF BLOCK b3.

*=============================================================================*
*AT SELECTION-SCREEN                                                          *
*=============================================================================*
AT SELECTION-SCREEN OUTPUT.

  IF p_iven IS NOT INITIAL.
    CLEAR: s_matnr[], s_werks[], s_fabri[], s_charg[], s_lgort[], s_ebeln[], s_lifnr[], s_vfdat[].
  ENDIF.

  IF p_iven IS INITIAL.
    CLEAR: p_iblnr[], p_matnr[], p_gjahr[].
  ENDIF.

  IF p_gera IS INITIAL.
    CLEAR:  s_vfdat[], s_fabri[].
  ENDIF.


  LOOP AT SCREEN.

****Esconder a opção de entrada de pesquisa por lote fabricante e validade.
*    CASE ABAP_TRUE.
*      WHEN P_LOTE.
*        IF SCREEN-NAME CS 'S_VFDAT' OR
*           SCREEN-NAME CS 'S_FABRI'.
*          SCREEN-ACTIVE = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN P_IVEN.
*        IF SCREEN-NAME CS 'S_VFDAT' OR
*           SCREEN-NAME CS 'S_FABRI' OR
*           SCREEN-NAME CS 'S_EBELN' OR
*           SCREEN-NAME CS 'S_LIFNR' OR
*           SCREEN-NAME CS 'S_VFDAT'.
*          SCREEN-ACTIVE = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*    ENDCASE.

*
***DEMOSTRAR OS CAMPOS DO GRUPO 1
    IF screen-group1 EQ 'ZG3'.
      IF p_iven IS INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 EQ 'ZG2'.
      IF p_gera IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
*
    IF screen-group1 EQ 'ZG3'.
      IF p_gera IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.



*DEMOSTRAR OS CAMPOS DO GRUPO 1
    IF screen-group1 EQ 'ZG1'.
      IF p_iven IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

*DEMOSTRAR OS CAMPOS DO GRUPO 1
    IF screen-group1 EQ 'ZG2'.
      IF p_iven IS NOT INITIAL.
        screen-active = '0'.
      ELSE.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS run.

    METHODS set_title_and_status.

    METHODS select_data
      EXCEPTIONS
        data_not_found.

    METHODS check_dados
      EXCEPTIONS
        data_not_found.

    METHODS get_dismemberments
      RETURNING VALUE(table) TYPE ty_t_dismemberment.

    METHODS get_pedidos
      RETURNING VALUE(table) TYPE ekko_tty.

    METHODS get_ebeln
      IMPORTING
                matnr         TYPE matnr
                werks         TYPE werks_d
                lgort         TYPE lgort_d
                charg         TYPE charg_d
      RETURNING VALUE(return) TYPE vbeln.

    METHODS get_saldo
      IMPORTING lfabr         TYPE zde_lote_forn
      RETURNING VALUE(return) TYPE labst.

    METHODS get_charg_available_balance
      IMPORTING
        material                 TYPE zppt0011-matnr
        centro                   TYPE zppt0011-werks
        deposito                 TYPE zppt0011-lgort
        lote                     TYPE zppt0011-charg
      RETURNING
        VALUE(available_balance) TYPE mchb-clabs.

    METHODS set_header.
    METHODS create_docking.

    METHODS get_material_description
      IMPORTING
        material    TYPE makt-matnr
      RETURNING
        VALUE(text) TYPE makt-maktx.

    METHODS get_werks_description
      IMPORTING
        werks       TYPE t001w-werks
      RETURNING
        VALUE(text) TYPE t001w-name1.

    METHODS get_lifnr_description
      IMPORTING
        lifnr       TYPE lfa1-lifnr
      RETURNING
        VALUE(text) TYPE lfa1-name1.

    METHODS get_fieldcatalog
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.

    METHODS get_sort
      RETURNING VALUE(sort) TYPE lvc_t_sort.

    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        es_row_no
        e_column_id
        e_row_id.

    METHODS process_before_output.
    METHODS set_outtab_data.

    METHODS set_outtab_hierarchy
      CHANGING alv_tree TYPE REF TO cl_gui_alv_tree.

    METHODS get_fab
      IMPORTING charg        TYPE charg_d
      RETURNING VALUE(lfabr) TYPE zde_lote_forn.

    METHODS get_vencimento
      IMPORTING charg        TYPE charg_d
                matnr        TYPE matnr
      RETURNING VALUE(vfdat) TYPE vfdat.

    METHODS get_range
      RETURNING VALUE(range) TYPE zrsdsselopts.

    METHODS display.

  PRIVATE SECTION.

    "//tables
    DATA risks_of_validity_date TYPE TABLE OF rgsb4.
    DATA dismemberments         TYPE TABLE OF zppt0011.
    DATA pedidos                TYPE TABLE OF ekko.
    DATA _zppt0016              TYPE TABLE OF zppt0016.

    "//Objects
    DATA docking        TYPE REF TO cl_gui_docking_container.
    DATA splitter       TYPE REF TO cl_gui_splitter_container.
    DATA custom_header  TYPE REF TO cl_gui_container.
    DATA custom_grid    TYPE REF TO cl_gui_container.
    DATA grid           TYPE REF TO cl_gui_alv_grid.
    DATA alv_tree       TYPE REF TO cl_gui_alv_tree.

ENDCLASS.

DATA r_main TYPE REF TO cl_main.

CLASS zcl_sel_dados DEFINITION.
  PUBLIC SECTION.

    TYPES: gt_zppt0011 TYPE TABLE OF zppt0011 WITH EMPTY KEY.

    CLASS-METHODS: check_invent EXPORTING t_zpmt0011 TYPE gt_zppt0011.


ENDCLASS.


CLASS zcl_sel_dados IMPLEMENTATION.

  METHOD check_invent.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = p_matnr
      IMPORTING
        output       = p_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    SELECT *
    FROM zppt0011
    INTO TABLE t_zpmt0011
      WHERE iblnr IN p_iblnr
        AND matnr IN p_matnr
        AND gjahr IN p_gjahr
        AND mark EQ abap_true.



  ENDMETHOD.

ENDCLASS.

CLASS cl_main IMPLEMENTATION.
  METHOD run.
    CREATE OBJECT r_main.

    r_main->check_dados( EXCEPTIONS data_not_found = 4 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF p_iven IS INITIAL.
      r_main->select_data( EXCEPTIONS data_not_found = 4 ).

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      ELSE.
        CALL SCREEN 0001.
      ENDIF.
    ELSE.

      zcl_sel_dados=>check_invent(
        IMPORTING
          t_zpmt0011 = DATA(t_zpmt0011)
      ).


      IF t_zpmt0011 IS NOT INITIAL.
        APPEND LINES OF t_zpmt0011 TO r_main->dismemberments.
        CALL SCREEN 0001.

      ELSE.


        MESSAGE TEXT-m05 TYPE 'I' DISPLAY LIKE 'I'.
*        LEAVE SCREEN.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD process_before_output.
    "//set title
    me->set_title_and_status( ).

    "//screen components
    me->create_docking( ).

    "//set data
    me->set_header( ).
    me->set_outtab_data( ).

    "//display data
    me->display( ).
  ENDMETHOD.

  METHOD set_title_and_status.
    SET TITLEBAR 'MAIN_TITLE'.
    SET PF-STATUS 'MAIN_STATUS'.
  ENDMETHOD.

  METHOD check_dados.

    IF p_iven IS INITIAL.
      IF s_matnr IS INITIAL.
        MESSAGE TEXT-m01 TYPE 'S' RAISING data_not_found.
      ELSE.
        IF s_matnr-low EQ '*' OR s_matnr-high EQ '*'.
          MESSAGE TEXT-m03 TYPE 'S' RAISING data_not_found.
        ENDIF.
      ENDIF.

      IF s_werks IS INITIAL.
        MESSAGE TEXT-m02 TYPE 'S' RAISING data_not_found.
      ELSE.
        IF s_werks-low EQ '*' OR s_werks-high EQ '*'.
          MESSAGE TEXT-m03 TYPE 'S' RAISING data_not_found.
        ENDIF.
      ENDIF.
    ELSE.
      IF p_iblnr IS INITIAL.
        MESSAGE TEXT-m04 TYPE 'S' RAISING data_not_found.

      ENDIF.

      IF p_gjahr IS INITIAL.
        MESSAGE TEXT-m04 TYPE 'S' RAISING data_not_found.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD select_data.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-low
      IMPORTING
        output       = s_matnr-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-high
      IMPORTING
        output       = s_matnr-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    CASE abap_true.
      WHEN p_entr. "//Entregue

        FREE: me->dismemberments.
        SELECT *
          FROM zppt0011  AS a
          INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~ebeln IN s_ebeln
           AND a~vfdat IN s_vfdat
           AND a~lfabr IN s_fabri
*           AND A~MARK  EQ ABAP_FALSE
           AND a~umcha NE space
           AND ( a~charg IN s_charg OR a~umcha IN s_charg )
           AND ( a~umlgo IN s_lgort OR a~umlgo IN s_lgort ).

        IF me->dismemberments IS NOT INITIAL.
          SELECT *
          FROM mchb
          INTO TABLE @DATA(gt_mchb)
            FOR ALL ENTRIES IN @me->dismemberments
            WHERE matnr EQ @me->dismemberments-matnr
              AND werks EQ @me->dismemberments-werks
              AND lgort EQ @me->dismemberments-lgort
              AND charg EQ @me->dismemberments-charg.

          LOOP AT me->dismemberments ASSIGNING FIELD-SYMBOL(<ls_dism>).
            LOOP AT gt_mchb ASSIGNING FIELD-SYMBOL(<lw_mchb>) WHERE matnr EQ <ls_dism>-matnr
                                                                AND werks EQ <ls_dism>-werks
                                                                AND lgort EQ <ls_dism>-lgort
                                                                AND charg EQ <ls_dism>-charg.


              IF <ls_dism>-clabs > 0.
                <ls_dism>-clabs =  <ls_dism>-clabs - <lw_mchb>-clabs.

              ENDIF.
            ENDLOOP.
          ENDLOOP.

        ENDIF.


      WHEN p_open. "//Não Entregue / Em deposito.

        FREE: me->dismemberments.
        SELECT *
          FROM zppt0011  AS a
          INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~ebeln IN s_ebeln
           AND a~vfdat IN s_vfdat
           AND a~lfabr IN s_fabri
*           AND A~UMCHA EQ SPACE
*           AND A~MARK  EQ ABAP_FALSE
*           AND A~UMCHA EQ SPACE
           AND charg IN s_charg
           AND ( EXISTS ( SELECT *
          FROM mchb
            WHERE matnr EQ a~matnr
              AND werks EQ a~werks
              AND lgort EQ a~lgort
              AND charg EQ a~charg
              AND clabs > 0 ) ).

*           AND ( A~UMLGO IN S_LGORT OR A~LGORT IN S_LGORT ).

*        IF ME->DISMEMBERMENTS IS NOT INITIAL.
*          FREE: GT_MCHB.
*          SELECT *
*          FROM MCHB
*          INTO TABLE GT_MCHB
*            FOR ALL ENTRIES IN ME->DISMEMBERMENTS
*            WHERE MATNR EQ ME->DISMEMBERMENTS-MATNR
*              AND WERKS EQ ME->DISMEMBERMENTS-WERKS
*              AND LGORT EQ ME->DISMEMBERMENTS-LGORT
*              AND CHARG EQ ME->DISMEMBERMENTS-CHARG.
*
*          LOOP AT ME->DISMEMBERMENTS ASSIGNING <LS_DISM>.
*            LOOP AT GT_MCHB ASSIGNING <LW_MCHB> WHERE MATNR EQ <LS_DISM>-MATNR
*                                                                AND WERKS EQ <LS_DISM>-WERKS
*                                                                AND LGORT EQ <LS_DISM>-LGORT
*                                                                AND CHARG EQ <LS_DISM>-CHARG.
*
*
**              <LS_DISM>-CLABS = <LW_MCHB>-CLABS.
*            ENDLOOP.
*          ENDLOOP.
*        ENDIF.

      WHEN p_desm. "//À Etiquetar

        FREE: me->dismemberments.

        SELECT * FROM zppt0016
          INTO TABLE @DATA(tl_zppt0016)
          WHERE matnr   IN @s_matnr
            AND werks   IN @s_werks
            AND vfdat   IN @s_vfdat
            AND zlicha  IN @s_fabri
            AND lgort   IN @s_lgort
            AND charg   IN @s_charg.

        IF sy-subrc IS INITIAL.

          me->dismemberments =
          VALUE #( FOR ls IN tl_zppt0016
                       (
                           matnr = ls-matnr
                           werks = ls-werks
                           lgort = ls-lgort
                           charg = ls-chargd
                           clabs = get_charg_available_balance(
                                      material          = ls-matnr
                                      centro            = ls-werks
                                      deposito          = ls-lgort
                                      lote              = ls-chargd ) "LS-CLABS - GET_SALDO( LS-ZLICHA )
                           vfdat = ls-vfdat
                           lfabr = ls-zlicha
                           ebeln = get_ebeln( EXPORTING
                                                     matnr = ls-matnr
                                                     werks = ls-werks
                                                     lgort = ls-lgort
                                                     charg = ls-chargd
                                             )
                        )
                  ).
          DELETE me->dismemberments WHERE clabs EQ 0.
        ENDIF.

      WHEN p_all. "//Etiquetadas

        FREE: me->dismemberments.
        SELECT *
            FROM zppt0011
            INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
           WHERE matnr IN s_matnr
             AND werks IN s_werks
             AND ebeln IN s_ebeln
             AND vfdat IN s_vfdat
             AND lfabr IN s_fabri
             AND lgort IN s_lgort
             AND charg IN s_charg.
*             AND A~MARK  EQ ABAP_FALSE

*             AND ( A~LGORT IN S_LGORT OR A~UMLGO IN S_LGORT )
*             AND ( A~CHARG IN S_CHARG OR A~UMCHA IN S_CHARG ).

      WHEN p_gera. "//Etiquetadas + À etiquetar

        DATA: range TYPE RANGE OF zppt0011-charg.
        DATA: t_mchb TYPE TABLE OF mchb.
        DATA: t_zppt0016 TYPE TABLE OF zppt0016.
        FREE: me->dismemberments.
*        DATA(R_CHARG) = GET_RANGE( ).

*        SELECT * FROM MCHB
*          INTO TABLE @DATA(TL_MCHB)
*          WHERE MATNR   IN @S_MATNR
*            AND WERKS   IN @S_WERKS
*            AND VFDAT   IN S_VFDAT
*            AND ZLICHA  IN S_FABRI
*            AND LGORT   IN @S_LGORT
*            AND CHARG    IN @S_CHARG.

*        SORT TL_MCHB BY MATNR CHARG.
*        IF R_CHARG IS NOT INITIAL.
*          DELETE TL_MCHB WHERE CHARG IN R_CHARG.
*        ENDIF.

        SELECT * FROM zppt0016
          INTO TABLE t_zppt0016
          WHERE matnr   IN s_matnr
            AND werks   IN s_werks
            AND vfdat   IN s_vfdat
            AND zlicha  IN s_fabri
            AND lgort   IN s_lgort
            AND charg  IN s_charg.
*
*        SELECT * FROM ZPPT0016
*        INTO CORRESPONDING FIELDS OF TABLE T_ZPPT0016
*          FOR ALL ENTRIES IN TL_MCHB
*        WHERE MATNR   EQ TL_MCHB-MATNR
*          AND WERKS   EQ TL_MCHB-WERKS
*          AND LGORT   EQ TL_MCHB-LGORT
*          AND CHARG   EQ TL_MCHB-CHARG.
*
*        IF T_ZPPT0016 IS NOT INITIAL.
*          SELECT * FROM MCHB
*        INTO TABLE T_MCHB
*        FOR ALL ENTRIES IN T_ZPPT0016
*        WHERE MATNR   EQ T_ZPPT0016-MATNR
*          AND WERKS   EQ T_ZPPT0016-WERKS
*          AND LGORT   EQ T_ZPPT0016-LGORT
*          AND CHARG    EQ T_ZPPT0016-CHARG.
*
*        ENDIF.

        FREE: t_zppt0016.
        SELECT * FROM zppt0016
        INTO TABLE t_zppt0016
        WHERE matnr   IN s_matnr
          AND werks   IN s_werks
          AND vfdat   IN s_vfdat
          AND zlicha  IN s_fabri
          AND lgort   IN s_lgort
          AND charg   IN s_charg.

        IF sy-subrc IS INITIAL.

          me->dismemberments =
          VALUE #( FOR ls IN t_zppt0016
                       (
                           matnr = ls-matnr
                           werks = ls-werks
                           lgort = ls-lgort
                           charg = ls-chargd
                           clabs = get_charg_available_balance(
                                      material          = ls-matnr
                                      centro            = ls-werks
                                      deposito          = ls-lgort
                                      lote              = ls-chargd ) "LS-CLABS - GET_SALDO( LS-ZLICHA )
                           vfdat = ls-vfdat
                           lfabr = ls-zlicha
                           ebeln = get_ebeln( EXPORTING
                                                     matnr = ls-matnr
                                                     werks = ls-werks
                                                     lgort = ls-lgort
                                                     charg = ls-chargd
                                             )
                        )
                  ).
          DELETE me->dismemberments WHERE clabs EQ 0.
        ENDIF.


*        IF SY-SUBRC IS INITIAL.
*
*          ME->DISMEMBERMENTS =
*          VALUE #( FOR LT IN T_ZPPT0016
*                       (
*                           MATNR = LT-MATNR
*                           WERKS = LT-WERKS
*                           LGORT = LT-LGORT
*                           CHARG = LT-CHARG
*                           CLABS = GET_CHARG_AVAILABLE_BALANCE(
*                                      MATERIAL          = LT-MATNR
*                                      CENTRO            = LT-WERKS
*                                      DEPOSITO          = LT-LGORT
*                                      LOTE              = LT-CHARG )"LS-CLABS - GET_SALDO( LS-ZLICHA )
**                           VFDAT = LS-VFDAT
**                           LFABR = LS-ZLICHA
*                           EBELN = GET_EBELN( EXPORTING
*                                                     MATNR = LT-MATNR
*                                                     WERKS = LT-WERKS
*                                                     LGORT = LT-LGORT
*                                                     CHARG = LT-CHARG
*                                             )
*                        )
*                  ).
*
*          DELETE ME->DISMEMBERMENTS WHERE CLABS EQ 0.
*
*        ENDIF.

        SELECT *
          FROM zppt0011  AS a
          APPENDING CORRESPONDING FIELDS OF TABLE me->dismemberments
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~ebeln IN s_ebeln
           AND a~vfdat IN s_vfdat
           AND a~lfabr IN s_fabri
*           AND A~MARK  EQ ABAP_FALSE
           AND ( a~lgort IN s_lgort OR a~umlgo IN s_lgort )
           AND ( a~charg IN s_charg OR a~umcha IN s_charg ).


      WHEN p_lote. "//Lote à Identificar
        FREE: me->dismemberments.
        FREE: range.
        range = get_range( ).

        SELECT *
          FROM mchb
          INTO TABLE @DATA(it_mchb)
         WHERE matnr IN @s_matnr
           AND werks IN @s_werks
           AND lgort IN @s_lgort
           AND charg IN @s_charg
           AND clabs NE 0.

*        CHECK it_mchb IS NOT INITIAL.
        SORT it_mchb BY matnr charg.
        DELETE it_mchb WHERE charg IN range.

**      Verificar se o lote é uma CHARG principal na ZPPT0016.
        CHECK it_mchb IS NOT INITIAL.
        SELECT *
        FROM zppt0016
        INTO TABLE @DATA(gt_zppt0016)
          FOR ALL ENTRIES IN @it_mchb
          WHERE matnr EQ @it_mchb-matnr
            AND werks EQ @it_mchb-werks
            AND lgort EQ @it_mchb-lgort
            AND charg EQ @it_mchb-charg.

        FREE: it_mchb.
        SELECT *
        FROM mchb
        INTO TABLE it_mchb
       FOR ALL ENTRIES IN gt_zppt0016
       WHERE matnr EQ gt_zppt0016-matnr
         AND werks EQ gt_zppt0016-werks
         AND lgort EQ gt_zppt0016-lgort
         AND charg EQ gt_zppt0016-charg.


        me->dismemberments =
        VALUE #( FOR ls2 IN it_mchb
                     (
                         matnr  = ls2-matnr
                         werks  = ls2-werks
                         clabs  = ls2-clabs
                         lgort  = ls2-lgort
                         charg  = ls2-charg
                      )
               ).
        FREE: gt_zppt0016, it_mchb.

      WHEN p_iven.

*****  Usando o Mark para iventariar as Embalagens porem temos que rever o processo
        SELECT *
          FROM zppt0011  AS a
          INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
         WHERE a~matnr IN s_matnr
           AND a~werks IN s_werks
           AND a~lgort IN s_lgort
           AND a~charg IN s_charg
           AND a~mark  EQ abap_true.

    ENDCASE.

    IF me->dismemberments IS INITIAL.
      MESSAGE TEXT-e01 TYPE 'S' RAISING data_not_found.
    ENDIF.

    SELECT *
       FROM ekko
       INTO TABLE me->pedidos
    FOR ALL ENTRIES IN me->dismemberments
      WHERE ebeln = me->dismemberments-ebeln
        AND lifnr IN s_lifnr.

    SELECT *
       FROM zppt0016
       INTO TABLE _zppt0016
    FOR ALL ENTRIES IN me->dismemberments
       WHERE zlicha EQ me->dismemberments-lfabr
         AND werks  EQ me->dismemberments-werks
         AND matnr  EQ me->dismemberments-matnr. "Inclusão na codição seleção o campo material ( MATNR ) - 2000002339/IR160227 / AOENNING
  ENDMETHOD.

  METHOD get_dismemberments.
    MOVE me->dismemberments TO table.
  ENDMETHOD.

  METHOD get_pedidos.
    MOVE me->pedidos TO table.
  ENDMETHOD.

  METHOD get_ebeln.

    DATA: lv_matnr   TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE mblnr
      FROM mseg
      INTO @DATA(_mblnr)
      WHERE charg EQ @charg
      AND matnr EQ @lv_matnr
      AND werks EQ @werks
      AND lgort EQ @lgort
      AND bwart EQ '101'.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE ebeln
        FROM ekbe
        INTO return
        WHERE belnr EQ _mblnr
        AND bewtp EQ 'E'
        AND bwart EQ '101'.

    ELSE.
      SELECT SINGLE umcha
        FROM mseg
        INTO @DATA(_umcha)
        WHERE charg EQ @charg
          AND matnr EQ @lv_matnr
          AND werks EQ @werks
          AND lgort EQ @lgort
          AND bwart EQ '311' .

      IF ( _umcha IS NOT INITIAL ).
        SELECT SINGLE mblnr
          FROM mseg
          INTO _mblnr
          WHERE charg EQ _umcha
            AND matnr EQ lv_matnr
            AND werks EQ werks
            AND lgort EQ lgort
            AND bwart EQ '101'.

        IF sy-subrc IS INITIAL.
          SELECT SINGLE ebeln
            FROM ekbe
            INTO return
            WHERE belnr EQ _mblnr
            AND bewtp EQ 'E'
            AND bwart EQ '101'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_saldo.
    SELECT SUM( clabs )
      FROM zppt0011
      INTO return
      WHERE lfabr EQ lfabr.
  ENDMETHOD.

  METHOD get_charg_available_balance.

    DATA: lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE clabs
      FROM mchb
      INTO available_balance
     WHERE matnr = lv_matnr
       AND werks = centro
       AND lgort = deposito
       AND charg = lote.
  ENDMETHOD.

  METHOD set_header.
    DATA table_element  TYPE REF TO cl_dd_table_element.
    DATA table_texts    TYPE sdydo_text_table.
    DATA table_text     LIKE LINE OF table_texts.
    DATA column         TYPE REF TO cl_dd_area.
    DATA texto          TYPE char255.

    DATA(_risk) = zcl_risk_of_pesticide=>get_validity_risk( with_icon = abap_false ).

    IF ( document IS NOT BOUND ).
      CREATE OBJECT document.
    ELSE.
      document->initialize_document( ).
    ENDIF.

    "//Build title text
    document->add_text( text = 'Parâmetros de seleção:' sap_style = cl_dd_area=>heading ).
    document->new_line(  ). document->underline( ).

    texto = COND #(
        WHEN p_all  EQ abap_true THEN 'Etiquetadas'
        WHEN p_entr EQ abap_true THEN 'Retiradas'
        WHEN p_open EQ abap_true THEN 'Em depósitos'
        WHEN p_desm EQ abap_true THEN 'A Etiquetar'
        WHEN p_gera EQ abap_true THEN 'Etiquetadas + A Etiquetar'
        WHEN p_lote EQ abap_true THEN 'Lote a Identificar'
        WHEN p_iven EQ abap_true THEN 'Inventario'
        ).
    document->add_text( text = |{ texto CASE = UPPER }| ).
    document->new_line(  ). document->underline( ).

    document->add_table(
      EXPORTING
        no_of_columns               = 2     " Number of Table Columns
        border                      = '0'     " Width of Table Frame; '0' = No Frame
      IMPORTING
        table                       = DATA(_doctable1) ).

    _doctable1->add_column( EXPORTING width = '35%' IMPORTING column = DATA(_column_key) ).
    _doctable1->add_column( IMPORTING column = DATA(_column_value) ).

    _column_key->add_text( text = 'MATERIAL:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_matnr-high IS INITIAL AND s_matnr-low IS NOT INITIAL
                                       THEN |{ CONV i( s_matnr-low ) } - { me->get_material_description( s_matnr-low ) }|
                                       WHEN s_matnr-high IS NOT INITIAL
                                       THEN |{ s_matnr-low } até { s_matnr-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'CENTRO:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_werks-high IS INITIAL AND s_werks-low IS NOT INITIAL
                                       THEN |{ s_werks-low } - { me->get_werks_description( s_werks-low ) }|
                                       WHEN s_werks-high IS NOT INITIAL
                                       THEN |{ s_werks-low } até { s_matnr-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'PEDIDO:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_ebeln-high IS INITIAL AND s_ebeln-low IS NOT INITIAL
                                       THEN |{ s_ebeln-low }|
                                       WHEN s_ebeln-high IS NOT INITIAL
                                       THEN |{ s_ebeln-low } até { s_ebeln-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'LOTE FABRICANTE:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_fabri-high IS INITIAL AND s_fabri-low IS NOT INITIAL
                                       THEN |{ s_fabri-low }|
                                       WHEN s_fabri-high IS NOT INITIAL
                                       THEN |{ s_fabri-low } até { s_fabri-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'LOTE:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_charg-high IS INITIAL AND s_charg-low IS NOT INITIAL
                                       THEN |{ s_charg-low }|
                                       WHEN s_charg-high IS NOT INITIAL
                                       THEN |{ s_charg-low } até { s_charg-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'DEPOSITO:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_lgort-high IS INITIAL AND s_lgort-low IS NOT INITIAL
                                       THEN |{ s_lgort-low }|
                                       WHEN s_lgort-high IS NOT INITIAL
                                       THEN |{ s_lgort-low } até { s_lgort-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'FORNECEDOR:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN s_lifnr-high IS INITIAL AND s_lifnr-low IS NOT INITIAL
                                       THEN |{ s_lifnr-low } - { me->get_lifnr_description( s_lifnr-low ) }|
                                       WHEN s_lifnr-high IS NOT INITIAL
                                       THEN |{ s_lifnr-low } até { s_lifnr-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).


    DATA(_beg_date) = COND #( LET x = s_vfdat-low  IN WHEN x IS NOT INITIAL THEN x+6 && '/' && x+4(2) && '/' && x(4) ELSE '' ).
    DATA(_end_date) = COND #( LET y = s_vfdat-high IN WHEN y IS NOT INITIAL THEN y+6 && '/' && y+4(2) && '/' && y(4) ELSE '' ).

    _column_key->add_text( text = 'VALIDADE:' )."SAP_FONTSIZE = CL_DD_AREA=>LARGE SAP_EMPHASIS = CL_DD_AREA=>EMPHASIS ).
    _column_value->add_text( text = |{ COND #( WHEN _end_date IS INITIAL AND _beg_date IS NOT INITIAL
                                       THEN |{ _beg_date  }|
                                       WHEN _end_date IS NOT INITIAL
                                       THEN |{ _beg_date } até { _end_date }|
                                       ELSE '*' ) }| ).

    "//build risk text
    document->new_line( ).
    document->add_text( text = 'Legenda das cores:' sap_fontsize = cl_dd_area=>large sap_style = cl_dd_area=>heading ).
    document->new_line(  ). document->underline( ).

    CALL METHOD document->add_table(
      EXPORTING
        no_of_columns = 1
      IMPORTING
        tablearea     = DATA(_doctable2) ).

    _doctable2->add_text( text = CONV #( _risk-critical ) sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_negative_int fix_lines = '' ).
    _doctable2->new_row( ).

    _doctable2->add_text( text = CONV #( _risk-warning )  sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_total_int fix_lines = '' ).
    _doctable2->new_row( ).

    _doctable2->add_text( text = CONV #( _risk-positive ) sap_fontsize = cl_dd_area=>large sap_color = cl_dd_area=>list_positive_int fix_lines = '' ).
    _doctable2->new_row( ).

    document->merge_document( ).
    document->display_document( parent = custom_header ).
  ENDMETHOD.

  METHOD create_docking.

    CREATE OBJECT docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = cl_gui_docking_container=>dock_at_top
        extension = 5000.

    CREATE OBJECT splitter
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        top        = 50
        parent     = docking
        rows       = 2
        columns    = 1.

    me->splitter->set_row_height( id = 1 height = 35 ).

    custom_header = me->splitter->get_container( row = 1 column = 1 ).
    custom_grid   = me->splitter->get_container( row = 2 column = 1 ).
  ENDMETHOD.

  METHOD set_outtab_data.
    DATA outtab TYPE ty_saida.

    DATA(_dismemberments) = me->get_dismemberments( ).
    DATA(_pedidos)        = me->get_pedidos( ).

    DATA(_risk) = zcl_risk_of_pesticide=>get_validity_risk( ).
    SORT pedidos BY ebeln.

    LOOP AT me->dismemberments INTO DATA(_dismemberment).

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = _dismemberment-matnr
        IMPORTING
          output       = _dismemberment-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      READ TABLE _pedidos INTO DATA(_pedido) WITH KEY ebeln = _dismemberment-ebeln BINARY SEARCH.
      TRY .
          DATA(_0016) = _zppt0016[ zlicha = _dismemberment-lfabr ].
        CATCH cx_sy_itab_line_not_found.
          CLEAR _0016.
      ENDTRY.

      DATA(_validity_risk_color) =
          COND #( LET  x = zcl_risk_of_pesticide=>check_risk_of_validity_date( _0016-dtval ) IN
                  WHEN x = 'C' THEN VALUE lvc_s_colo( col = col_negative int = 1 ) "//Critical
                  WHEN x = 'W' THEN VALUE lvc_s_colo( col = col_total    int = 1 ) "//Warning
                  WHEN x = 'P' THEN VALUE lvc_s_colo( col = col_positive int = 1 ) "//Positive
                  ELSE VALUE lvc_s_colo( col = col_negative int = 1 ) "//Critical
                ).
*_DISMEMBERMENT-DLABS
      CASE abap_true.
        WHEN p_entr. outtab-clabs =  _dismemberment-dlabs.
        WHEN p_open. outtab-clabs = ( _dismemberment-clabs - _dismemberment-dlabs ).
        WHEN OTHERS. outtab-clabs =  _dismemberment-clabs.
      ENDCASE.

      outtab-ebeln = _dismemberment-ebeln.
      outtab-matnr = _dismemberment-matnr.
      outtab-werks = _dismemberment-werks.
      outtab-lgort = _dismemberment-lgort.
      outtab-charg = _dismemberment-charg.
      outtab-lfabr = _dismemberment-lfabr.
      outtab-vfdat = _dismemberment-vfdat.
      outtab-umcha = _dismemberment-umcha.
      outtab-rsnum = _dismemberment-rsnum.
      outtab-dtval = _0016-dtval.
      outtab-maktx = me->get_material_description( _dismemberment-matnr ).
      outtab-name1 = |{ _pedido-lifnr } - { me->get_lifnr_description( _pedido-lifnr ) }|.
      outtab-color = VALUE #( ( fname = 'DTVAL' color = _validity_risk_color ) ).

      APPEND outtab TO gt_outtab.

    ENDLOOP.

    IF s_ebeln IS NOT INITIAL.
      DELETE gt_outtab WHERE ebeln NOT IN s_ebeln.
    ENDIF.

    IF  s_lifnr IS NOT INITIAL.
      DELETE gt_outtab WHERE lifnr NOT IN s_lifnr.
    ENDIF.

  ENDMETHOD.

  METHOD set_outtab_hierarchy.
  ENDMETHOD.

  METHOD display.
    DATA(_fieldcatalog) = me->get_fieldcatalog( ).
    DATA(_sort) = me->get_sort( ).

    DATA(_layout) = VALUE lvc_s_layo( ctab_fname = 'COLOR' ).

    CREATE OBJECT me->grid
      EXPORTING
        i_parent = me->custom_grid.

    SET HANDLER me->handle_hotspot_click FOR me->grid.

    CALL METHOD me->grid->set_table_for_first_display
      EXPORTING
        is_layout       = _layout
      CHANGING
        it_outtab       = gt_outtab
        it_fieldcatalog = _fieldcatalog
        it_sort         = _sort.
  ENDMETHOD.

  METHOD get_material_description.
    DATA: lv_material TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE maktx
      FROM makt
      INTO text
      WHERE matnr = lv_material.
  ENDMETHOD.

  METHOD get_werks_description.
    SELECT SINGLE name1 FROM t001w INTO text WHERE werks = werks.
  ENDMETHOD.

  METHOD get_lifnr_description.
    SELECT SINGLE name1 FROM lfa1 INTO text WHERE lifnr = lifnr.
  ENDMETHOD.

  METHOD get_fieldcatalog.

    fcat =
        VALUE #(
        ( fieldname = 'EBELN' coltext = 'Pedido'          outputlen = 12 )
        ( fieldname = 'MATNR' coltext = 'Material'        outputlen = 10 no_zero = abap_true )
        ( fieldname = 'MAKTX' coltext = 'Descrição'       outputlen = 25 )
        ( fieldname = 'WERKS' coltext = 'Centro'          outputlen = 7 )
        ( fieldname = 'LGORT' coltext = 'Depósito'        outputlen = 10 )
        ( fieldname = 'NAME1' coltext = 'Fornecedor'      outputlen = 25 no_zero = abap_true )

        (
          fieldname = 'CHARG'
          coltext = COND #( WHEN p_lote EQ abap_true THEN 'Lote' ELSE 'Etiqueta' )
          outputlen = 13 no_out  = COND #( WHEN p_desm EQ abap_true THEN abap_true ELSE abap_false )
          hotspot = abap_true
        )

        ( fieldname = 'UMCHA'
          coltext = 'Lote Receptor'
          outputlen = 13 no_out  = COND #( WHEN p_open EQ abap_true OR
                                                p_desm EQ abap_true OR
                                                p_gera EQ abap_true OR
                                                p_lote EQ abap_true THEN abap_true ELSE abap_false
                                         )
          hotspot = abap_true
         )

        ( fieldname = 'LFABR' coltext = 'Lote Fabricante' outputlen = 13 no_out = COND #( WHEN p_lote EQ abap_true THEN abap_true ELSE abap_false ) no_zero = abap_false   )
        ( fieldname = 'VFDAT' coltext = 'Validade'        outputlen = 10 no_out = COND #( WHEN p_lote EQ abap_true THEN abap_true ELSE abap_false ) no_zero = abap_false   )
        ( fieldname = 'DTVAL' coltext = 'Validade Real'   outputlen = 10 no_out = COND #( WHEN p_lote EQ abap_true THEN abap_true ELSE abap_false ) no_zero = abap_false   )
        ( fieldname = 'CLABS' coltext = 'Quantidade'      outputlen = 20 do_sum = abap_true ) ).

  ENDMETHOD.

  METHOD get_sort.
    CHECK p_lote IS INITIAL.
    sort =
    VALUE #(
            ( spos = '1' fieldname = 'LFABR'  subtot = abap_true )
           ).
  ENDMETHOD.

  METHOD handle_hotspot_click.
    TRY.
        DATA(_selected_row) = gt_outtab[ e_row_id-index ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    CASE e_column_id.
      WHEN 'CHARG'.
        SET PARAMETER ID 'MAT' FIELD _selected_row-matnr.
        SET PARAMETER ID 'WRK' FIELD _selected_row-werks.
        SET PARAMETER ID 'LAG' FIELD _selected_row-lgort.
        SET PARAMETER ID 'CHA' FIELD _selected_row-charg.

        SET PARAMETER ID 'LIF' FIELD ''.
        SET PARAMETER ID 'KUN' FIELD ''.
        SET PARAMETER ID 'BWA' FIELD ''.
        SET PARAMETER ID 'AUN' FIELD ''.
        SET PARAMETER ID 'KPO' FIELD ''.
        SET PARAMETER ID 'USR' FIELD ''.

        CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.
      WHEN 'UMCHA'.
        IF NOT _selected_row-umcha IS INITIAL.
          SET PARAMETER ID 'MAT' FIELD _selected_row-matnr.
          SET PARAMETER ID 'WRK' FIELD _selected_row-werks.
          SET PARAMETER ID 'LAG' FIELD _selected_row-umlgo.
          SET PARAMETER ID 'CHA' FIELD _selected_row-umcha.

          SET PARAMETER ID 'LIF' FIELD ''.
          SET PARAMETER ID 'KUN' FIELD ''.
          SET PARAMETER ID 'BWA' FIELD ''.
          SET PARAMETER ID 'AUN' FIELD ''.
          SET PARAMETER ID 'KPO' FIELD ''.
          SET PARAMETER ID 'USR' FIELD ''.

          CALL TRANSACTION 'MB51' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'RSNUM'.
        IF NOT _selected_row-rsnum IS INITIAL.
          SET PARAMETER ID 'RES' FIELD _selected_row-rsnum.
          CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD get_fab.
    SELECT SINGLE zlicha FROM zppt0016 INTO lfabr WHERE chargd = charg.
  ENDMETHOD.

  METHOD get_vencimento.
    SELECT SINGLE vfdat FROM zppt0016 INTO vfdat WHERE matnr EQ matnr AND charg = charg.
  ENDMETHOD.

  METHOD get_range.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-low
      IMPORTING
        output       = s_matnr-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-high
      IMPORTING
        output       = s_matnr-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    SELECT charg
      FROM zppt0011
      INTO TABLE @DATA(r_01)
      WHERE matnr IN @s_matnr
        AND werks IN @s_werks
        AND lgort IN @s_lgort
        AND charg IN @s_charg
        AND mark  EQ @abap_false.

*    SELECT UMCHA
*      FROM ZPPT0011
*      APPENDING TABLE R_01
*      WHERE MATNR IN S_MATNR
*        AND WERKS IN S_WERKS
*        AND LGORT IN S_LGORT
*        AND MARK  EQ ABAP_FALSE.

    SELECT chargd
      FROM zppt0016
      APPENDING TABLE r_01
      WHERE matnr IN s_matnr
        AND werks IN s_werks
        AND charg IN s_charg
        AND lgort IN s_lgort.

    range = VALUE #( FOR ls1 IN r_01 ( sign = 'I' option = 'EQ' low = ls1 ) ).

    SORT range.
    DELETE ADJACENT DUPLICATES FROM range COMPARING ALL FIELDS.

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
*BREAK-POINT.
  cl_main=>run( ).

*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pbo OUTPUT.
  IF r_main IS INITIAL.
    CREATE OBJECT r_main.
  ENDIF.

  r_main->process_before_output( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai INPUT.
  IF sy-ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
