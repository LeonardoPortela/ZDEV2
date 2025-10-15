*&---------------------------------------------------------------------*
*& Report  ZLESR0088                                                  &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 22/01/2015                                              &*
*& Descrição: Relatório de clientes e fornecedores                    &*
*& Transação: FI                                                      &*
*&--------------------------------------------------------------------&*
REPORT  zfir0060.

TYPE-POOLS vrm.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*
TABLES: lfa1,kna1,t001,vtbbewe, lfb1, knvv.

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES:BEGIN OF ty_saida_dados_venda,
        kunnr  TYPE knvv-kunnr,
        vkorg  TYPE knvv-vkorg,
        vtweg  TYPE knvv-vtweg,
        spart  TYPE knvv-spart,
        ktgrd  TYPE knvv-ktgrd,
        vtext  TYPE tvkot-vtext, "Denominação Organização de vendas
        vtextd TYPE tvtwt-vtext, "Denominação Canal de distribuição
        vtexta TYPE tspat-vtext, "Denominação Setor de atividade.
        vtextg TYPE tvktt-vtext,
        txt50  TYPE skat-txt50,
        akont  TYPE knb1-akont,
      END OF ty_saida_dados_venda.

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*
TYPES: BEGIN OF ty_saida,
         "Fornecedor
         chg_log    TYPE icon-id,
         lifnr      TYPE lfa1-lifnr,
         land1      TYPE lfa1-land1,
         name1      TYPE lfa1-name1,
         name2      TYPE lfa1-name2,
         name3      TYPE lfa1-name3,
         name4      TYPE lfa1-name4,
         ort01      TYPE lfa1-ort01,
         ort02      TYPE lfa1-ort02,
         pfach      TYPE lfa1-pfach,
         pstl2      TYPE lfa1-pstl2,
         pstlz      TYPE lfa1-pstlz,
         regio      TYPE lfa1-regio,
         sortl      TYPE lfa1-sortl,
         stras      TYPE lfa1-stras,
         adrnr      TYPE lfa1-adrnr,
         mcod1      TYPE lfa1-mcod1,
         mcod2      TYPE lfa1-mcod2,
         mcod3      TYPE lfa1-mcod3,
         anred      TYPE lfa1-anred,
         bahns      TYPE lfa1-bahns,
         bbbnr      TYPE lfa1-bbbnr,
         bbsnr      TYPE lfa1-bbsnr,
         begru      TYPE lfa1-begru,
         brsch      TYPE lfa1-brsch,
         bubkz      TYPE lfa1-bubkz,
         datlt      TYPE lfa1-datlt,
         dtams      TYPE lfa1-dtams,
         dtaws      TYPE lfa1-dtaws,
         erdat      TYPE lfa1-erdat,
         ernam      TYPE lfa1-ernam,
         esrnr      TYPE lfa1-esrnr,
         konzs      TYPE lfa1-konzs,
         ktokk      TYPE lfa1-ktokk,
         kunnr      TYPE lfa1-kunnr,
         lnrza      TYPE lfa1-lnrza,
         loevm      TYPE lfa1-loevm,
         sperr      TYPE lfa1-sperr,
         sperm      TYPE lfa1-sperm,
         spras      TYPE lfa1-spras,
         stcd1      TYPE lfa1-stcd1,
         stcd2      TYPE lfa1-stcd2,
         stcd3      TYPE lfa1-stcd3,
         stcdx      TYPE lfa1-stcd3, "MODIFICAÇÃO 14/09/2016 - NÚMERO DO RG
         stkza      TYPE lfa1-stkza,
         stkzu      TYPE lfa1-stkzu,
         telbx      TYPE lfa1-telbx,
         telf1      TYPE lfa1-telf1,
         telf2      TYPE lfa1-telf2,
         telfx      TYPE lfa1-telfx,
         teltx      TYPE lfa1-teltx,
         telx1      TYPE lfa1-telx1,
         xcpdk      TYPE lfa1-xcpdk,
         xzemp      TYPE lfa1-xzemp,
         vbund      TYPE lfa1-vbund,
         fiskn      TYPE lfa1-fiskn,

*CódRegTribut.- nº - LFA1-CRTN
         crtn       TYPE lfa1-crtn,
*Descrição Cod RegT- Com o LFA1-CRTN busca descrição na tabela J_1BCRTN-_HIGH
         val_text   TYPE val_text,
*Contribuinte ICMS - LFA1-ICMSTAXPAY
         icmstaxpay TYPE lfa1-icmstaxpay,
*Descrição Cont ICMS- Com o LFA1-ICMSTAXPAY busca descrição na tabela J_1BTICMSTAXPAYT-J_1BICMSTAXPAYX
         jtaxpayx   TYPE j_1bticmstaxpayt-j_1bicmstaxpayx,
*Tp.princ.set.ind. - LFA1-INDTYP
         indtyp     TYPE lfa1-indtyp,
*Descrição Tp Princ - Com o LFA1-INDTYP busca descrição na tabela J_1BTINDTYPT-J_1BINDTYPX
         indtypx    TYPE j_1btindtypt-j_1bindtypx,

         "Cliente
         aufsd      TYPE kna1-aufsd,
         bahne      TYPE kna1-bahne,
         exabl      TYPE kna1-exabl,
         faksd      TYPE kna1-faksd,
         knazk      TYPE kna1-knazk,
         knrza      TYPE kna1-knrza,
         ktokd      TYPE kna1-ktokd,
         kukla      TYPE kna1-kukla,
         lifsd      TYPE kna1-lifsd,
         locco      TYPE kna1-locco,
         niels      TYPE kna1-niels,
         counc      TYPE kna1-counc,
         cityc      TYPE kna1-cityc,
         rpmkr      TYPE kna1-rpmkr,
         stkzn      TYPE kna1-stkzn,

         "Dados Bancários
         banks      TYPE knbk-banks,
         bankl      TYPE knbk-bankl,
         bankn      TYPE knbk-bankn,
         bkont      TYPE knbk-bkont,
         bvtyp      TYPE knbk-bvtyp,
         bkref      TYPE knbk-bkref,

         stenr      TYPE lfa1-stenr,
         kraus      TYPE lfa1-kraus,
         gbdat      TYPE lfa1-gbdat,
         qtd_d      TYPE vtbbewe-atage,
         bukrs      TYPE lfb1-bukrs,
         akont      TYPE lfb1-akont,
       END OF ty_saida,

       BEGIN OF ty_header_historic_change,
         date_from   TYPE udate,
         date_upto   TYPE udate,
         transaction TYPE tcode,
         account     TYPE char10,
         username    TYPE username,
       END OF ty_header_historic_change,

       BEGIN OF ty_historic_change,
         value_old   TYPE cdpos-value_old,
         value_new   TYPE cdpos-value_new,
         operation   TYPE zoperacao,
         fieldname   TYPE dfies-fieldtext,
         transaction TYPE cdhdr-tcode,
         username    TYPE cdhdr-username,
         change_date TYPE cdhdr-udate,
         change_time TYPE cdhdr-utime,
       END OF ty_historic_change,

       BEGIN OF ty_dup,
         stcd1 TYPE lfa1-stcd1,
         stcd2 TYPE lfa1-stcd2,
         stcd3 TYPE lfa1-stcd3,
         count TYPE i,
         kunnr TYPE kna1-kunnr,
       END OF ty_dup.


TYPES: BEGIN OF ty_bsik,
         bukrs TYPE  bsik-bukrs,
         lifnr TYPE  bsik-lifnr,
         gjahr TYPE  bsik-gjahr,
         budat TYPE  bsik-budat,
       END OF ty_bsik.

TYPES: BEGIN OF ty_bsak,
         bukrs TYPE  bsik-bukrs,
         lifnr TYPE  bsik-lifnr,
         gjahr TYPE  bsik-gjahr,
         budat TYPE  bsik-budat,
       END OF ty_bsak.


TYPES: BEGIN OF ty_bsad,
         bukrs TYPE  bsad-bukrs,
         kunnr TYPE  bsad-kunnr,
         gjahr TYPE  bsad-gjahr,
         budat TYPE  bsad-budat,
       END OF ty_bsad.


TYPES: BEGIN OF ty_bsid,
         bukrs TYPE  bsad-bukrs,
         kunnr TYPE  bsad-kunnr,
         gjahr TYPE  bsad-gjahr,
         budat TYPE  bsad-budat,
       END OF ty_bsid.


*TYPES BEGIN OF TY_FOR_B.
*        INCLUDE TYPE BSIK.
*TYPES: END OF TY_FOR_B.
*
*TYPES: BEGIN OF TY_CLI_B.
*        INCLUDE TYPE BSAD.
*TYPES: END OF TY_CLI_B.


*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*
DATA: it_lfa1              TYPE TABLE OF lfa1,
      it_kna1              TYPE TABLE OF kna1,
      it_knb1              TYPE TABLE OF knb1,
      it_lfbk              TYPE STANDARD TABLE OF lfbk,
      t_knb1               TYPE STANDARD TABLE OF knb1,
      w_knb1               TYPE knb1,
      t_lfb1               TYPE STANDARD TABLE OF lfb1,
      w_lfb1               TYPE lfb1,
      it_knbk              TYPE STANDARD TABLE OF knbk,
      it_bsik              TYPE TABLE OF ty_bsik,
      it_bsak              TYPE TABLE OF ty_bsak,
      it_bsad              TYPE TABLE OF ty_bsad,
      it_bsid              TYPE TABLE OF ty_bsid,
      it_for_b             TYPE TABLE OF ty_bsik,
      wa_for_b             TYPE  ty_bsik,
      it_cli_b             TYPE TABLE OF ty_bsad,
      wa_cli_b             TYPE ty_bsad,
      it_dup               TYPE TABLE OF ty_dup,
      it_knvv              TYPE TABLE OF knvv,
*      it_kna1              TYPE TABLE OF kna1,
      it_skat              TYPE TABLE OF skat,
*      it_knb1              TYPE TABLE OF knb1,
      it_tvkot             TYPE TABLE OF tvkot,
      it_tvtwt             TYPE TABLE OF tvtwt,
      it_tspat             TYPE TABLE OF tspat,
      it_tvktt             TYPE TABLE OF tvktt,
      it_saida             TYPE TABLE OF ty_saida WITH HEADER LINE,
      it_saida_dados_venda TYPE TABLE OF ty_saida_dados_venda WITH HEADER LINE,
      tg_historic_changes  TYPE TABLE OF ty_historic_change.

*=============================================================================*
*RANGES                                                                       *
*=============================================================================*
RANGES: pessoa FOR kna1-stkzn.

DATA: tgjahr TYPE RANGE OF bsak-gjahr,
      wgjahr LIKE LINE OF tgjahr.

*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*
DATA: wa_lfa1                   TYPE lfa1,
      wa_kna1                   TYPE kna1,
      wa_dup                    TYPE ty_dup,
      wa_saida                  TYPE ty_saida,
      wg_historic_change_header TYPE ty_header_historic_change.

*=============================================================================*
*WORK AREA  TELA                                                              *
*=============================================================================*
DATA: wa_cont   TYPE REF TO cl_gui_custom_container,
      wa_alv    TYPE REF TO  cl_gui_alv_grid,
      wa_layout TYPE lvc_s_layo.


DATA: dt_atual TYPE sy-datum,
      dt_ini   TYPE sy-datum,
      st_fim   TYPE sy-datum.

*----------------------------------------------------------------------*
***INCLUDE ZFIR0060_0001 .                                             *
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION

*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      container_log    TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      alv_log          TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      variant_log      TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.

*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat   TYPE TABLE OF lvc_s_fcat,
     it_fcat_v TYPE TABLE OF lvc_s_fcat.
DATA:it_list    TYPE vrm_values,
     list_value TYPE vrm_values.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-007.

  PARAMETER ck_cli RADIOBUTTON GROUP tpp DEFAULT 'X' USER-COMMAND muda_tela.
  PARAMETER ck_for RADIOBUTTON GROUP tpp.
  PARAMETER ck_log RADIOBUTTON GROUP tpp.
SELECTION-SCREEN:END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-012.
  SELECTION-SCREEN SKIP 1.
  "Cliente
  SELECT-OPTIONS: s_kunnr  FOR kna1-kunnr ,
                  s_vkorg  FOR knvv-vkorg ,
                  s_vtweg  FOR knvv-vtweg ,
                  s_spart  FOR knvv-spart ,
                  s_land1  FOR kna1-land1,
                  p_bukrs  FOR lfb1-bukrs,
                  s_ktokd  FOR kna1-ktokd,
                  s1_erdat  FOR kna1-erdat,
                  s1_ernam  FOR kna1-ernam.
  "FORNECEDOR
  SELECT-OPTIONS: s_lifnr  FOR lfa1-lifnr,
                  s_land_1 FOR lfa1-land1 ,
                  s_ktokk  FOR lfa1-ktokk,
                  s_erdat  FOR lfa1-erdat,
                  s_ernam  FOR lfa1-ernam.

  SELECT-OPTIONS: s_usern  FOR sy-uname NO-EXTENSION NO INTERVALS,
                  s_date   FOR sy-datum NO-EXTENSION,
                  s_trans  FOR sy-tcode NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN:END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-006.
  PARAMETERS s_fis TYPE c AS CHECKBOX DEFAULT ' ' .
*SELECTION-SCREEN COMMENT (13) TEXT-003.

  PARAMETERS s_jur TYPE c AS CHECKBOX DEFAULT  ' '.
*SELECTION-SCREEN COMMENT (15) TEXT-004.

  PARAMETERS s_dup TYPE c AS CHECKBOX DEFAULT  ' '.
*SELECTION-SCREEN COMMENT (15) TEXT-005.

  PARAMETERS s_ban TYPE c AS CHECKBOX DEFAULT  ' '.
*SELECTION-SCREEN COMMENT (15) TEXT-005.

  PARAMETERS s_ina TYPE c AS CHECKBOX DEFAULT  ' ' USER-COMMAND muda_tela.


*SELECTION-SCREEN: BEGIN OF BLOCK b4  WITH FRAME TITLE text-013.
  PARAMETERS s_vend TYPE c  AS CHECKBOX  DEFAULT ' ' .
*SELECTION-SCREEN:END OF BLOCK b4.

  SELECTION-SCREEN SKIP 1.


  SELECT-OPTIONS: s_bukrs  FOR t001-bukrs,
                  s_qds    FOR vtbbewe-atage NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.



INITIALIZATION.

*  CK_CLI = ABAP_TRUE.

AT SELECTION-SCREEN.
  IF ck_log IS NOT INITIAL AND sy-ucomm = 'ONLI'.
    IF s_usern-low IS INITIAL.
      MESSAGE TEXT-011 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ELSEIF s_date-low IS INITIAL.
      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF ck_cli IS NOT INITIAL.
      IF ( screen-name CS 'S_LIFNR' OR screen-name CS 'S_LAND_1' OR screen-name CS 'S_KTOKK' )
      OR ( screen-name CS 'S_USERN' OR screen-name CS 'S_DATE'   OR screen-name CS 'S_TRANS' )
        OR ( screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' OR screen-name CS 'S_ERDAT'
        OR screen-name CS 'S_ERNAM' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.

    ELSEIF ck_for IS NOT INITIAL.
      IF ( screen-name CS 'S_KUNNR' OR screen-name CS 'S_LAND1' OR screen-name CS 'S_KTOKD'  )
      OR ( screen-name CS 'S_USERN' OR screen-name CS 'S_DATE'  OR screen-name CS 'S_TRANS'  )
        OR ( screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' OR screen-name CS 'S_VKORG'
        OR screen-name CS 'S_VTWEG' OR screen-name CS 'S_SPART' OR screen-name CS 'S_VEND'
        OR screen-name CS 's1_erdat' OR screen-name CS 's1_ernam' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF ( screen-name CS 'S_LIFNR' OR screen-name CS 'S_LAND_1' OR screen-name CS 'S_KTOKK'
        OR screen-name CS 'P_BUKRS' )
      OR ( screen-name CS 'S_KUNNR' OR screen-name CS 'S_LAND1'  OR screen-name CS 'S_KTOKD' )
      OR ( screen-name CS 'S_FIS'   OR screen-name CS 'S_JUR'    OR screen-name CS 'S_DUP' OR screen-name CS 'S_BAN'  OR screen-name  CS 'S_INA' )
      OR ( screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS'    OR screen-name CS 'S_VKORG' OR screen-name CS 'S_VTWEG'
      OR screen-name CS 'S_SPART' OR screen-name CS 'S_VEND' OR screen-name CS 'S_ERDAT' OR screen-name CS 'S_ERNAM'
         OR screen-name CS 's1_erdat' OR screen-name CS 's1_ernam').
        screen-invisible = 1.
        screen-input     = 0.
      ELSEIF ( screen-name CS 'S_USERN' OR screen-name CS 'S_DATE' ).
        screen-required = 2.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT SCREEN.

    IF s_fis IS NOT INITIAL.
      IF ( screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.
    ELSEIF  s_jur IS NOT INITIAL.
      IF (  screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.
    ELSEIF s_dup IS NOT INITIAL.
      IF (  screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.
    ELSEIF       s_ban IS NOT INITIAL.
      IF (  screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' ).
        screen-invisible = 1.
        screen-input = 0.
      ENDIF.
    ELSEIF s_ina IS NOT INITIAL.
      IF (  screen-name CS 'S_BUKRS' OR screen-name CS 'S_QDS' ).
        screen-invisible = 0.
        screen-input = 1.

      ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.


*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  IF s_vend IS INITIAL.
    gf_first_display = 'X'.
*  DATA TABLE TYPE RSIS_T_RANGE.
*  PERFORM SET_INTERVAL TABLES TABLE USING 'ASD' '123'.

    IF ( s_fis  IS INITIAL ) AND ( s_jur  IS INITIAL ) AND
       ( s_dup  IS INITIAL ) AND ( s_ban  IS INITIAL ) AND
       ( s_ina  IS INITIAL ).

      MESSAGE 'Favor selecione um dos tipos! ' TYPE 'S'.
      EXIT.
    ENDIF.


    IF s_ina IS NOT INITIAL.
      IF s_bukrs IS INITIAL.
        MESSAGE 'Campo empresa obrigatório!' TYPE 'S'.
        EXIT.
      ENDIF.
      IF s_qds IS INITIAL.
        MESSAGE 'Campo Qte Dias obrigatório!' TYPE 'S'.
        EXIT.
      ELSEIF  s_qds-low > 720  OR  s_qds-high > 720.
        MESSAGE 'O Limite e de 720 dias' TYPE 'S'.
        EXIT.
      ENDIF.
    ENDIF.

    PERFORM: f_selecionar_dados,               " Form selecionar dado
             f_organizar_dados,                " ORGANIZAR DADOS
             f_alv.                            "Saida ALV

    CALL SCREEN 0100.
  ELSE.
    PERFORM: f_selecionar_dados_mest_venda,               " Form selecionar dado
               f_organizar_dados_mest_venda,                " ORGANIZAR DADOS
               f_alv_mest_venda.

    CALL SCREEN 0100.
  ENDIF.


*---------- Inclementação  -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN 'CHG_LOG'.
        READ TABLE it_saida INTO DATA(_saida) INDEX e_row_id-index.
        IF ck_cli IS NOT INITIAL.
          MOVE _saida-kunnr TO wg_historic_change_header-account.
        ELSE.
          MOVE _saida-lifnr TO wg_historic_change_header-account.
        ENDIF.

        IF ck_log IS NOT INITIAL.
          wg_historic_change_header-transaction = s_trans-low.
          wg_historic_change_header-date_from   = s_date-low.
          wg_historic_change_header-date_upto   = s_date-high.
          wg_historic_change_header-username    = s_usern-low.

          PERFORM set_historic_changes
            USING
             wg_historic_change_header-transaction
             wg_historic_change_header-date_from
             wg_historic_change_header-date_upto
             wg_historic_change_header-account
             wg_historic_change_header-username.
        ENDIF.
    ENDCASE.

    CALL SCREEN 0101 STARTING AT 5 5.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*

FORM f_selecionar_dados.

*  RANGES: PESSOA FOR KNA1-STKZN.
  CLEAR: pessoa[], pessoa.


  IF s_fis NE s_jur.
    pessoa-sign   = 'I'.
    pessoa-option = 'EQ'.

    IF s_fis EQ abap_true.
      pessoa-low    = abap_true.
      pessoa-high   = abap_true.
    ELSEIF s_jur EQ abap_true.
      pessoa-low    = abap_false.
      pessoa-high   = abap_false.
    ENDIF.
    APPEND pessoa.
  ENDIF.

  IF s_dup = 'X'.
    IF ck_cli IS NOT INITIAL.
      SELECT stcd1 stcd2 stcd3 COUNT(*)
          INTO TABLE it_dup
          FROM kna1 AS a
           WHERE kunnr IN s_kunnr
           AND land1 IN s_land1
           AND ktokd IN s_ktokd
           AND stkzn IN pessoa
          AND ( stcd1 NE '' OR stcd2 NE '' )
          AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) )
         GROUP BY stcd1 stcd2 stcd3.

      DELETE  it_dup WHERE count = 1.

      IF it_dup[] IS NOT INITIAL.


        IF s_ban IS NOT INITIAL.
          PERFORM busca_dados_bancarios USING 'K' 'X'.

        ELSE.

          SELECT *
            FROM kna1 AS a
            INTO TABLE it_kna1
            FOR ALL ENTRIES IN it_dup
            WHERE stcd1 EQ it_dup-stcd1
              AND stcd2 EQ it_dup-stcd2
              AND stcd3 EQ it_dup-stcd3
              AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).

          "Seleção dados cliente ( Setor ativ, org venda, canal distrib.
          SELECT * FROM knvv INTO TABLE it_knvv FOR ALL ENTRIES IN it_kna1 WHERE kunnr EQ it_kna1-kunnr.

        ENDIF.

      ENDIF.
    ELSE.
      SELECT  stcd1 stcd2 stcd3 COUNT(*)
          INTO TABLE it_dup
          FROM lfa1 AS a
          WHERE lifnr IN s_lifnr
          AND land1 IN s_land_1
          AND ktokk IN s_ktokk
          AND stkzn IN pessoa
          AND ( stcd1 NE '' OR stcd2 NE '' )
         AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) )
         GROUP BY stcd1 stcd2 stcd3.

      DELETE  it_dup WHERE count = 1.

      IF it_dup[] IS NOT INITIAL.

        IF s_ban IS NOT INITIAL.
          PERFORM busca_dados_bancarios USING 'L' 'X'.
        ELSE.

          SELECT *
             FROM lfa1 AS a
             INTO TABLE it_lfa1
             FOR ALL ENTRIES IN it_dup
             WHERE stcd1 EQ it_dup-stcd1
               AND stcd2 EQ it_dup-stcd2
               AND stcd3 EQ it_dup-stcd3
               AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) ).

        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.

    IF ck_log IS  INITIAL .
      IF s_ban IS NOT INITIAL.

        PERFORM busca_dados_bancarios USING 'K' ' '.
        PERFORM busca_dados_bancarios USING 'L' ' '.

      ELSEIF s_ina IS NOT INITIAL.

        IF ck_for IS NOT INITIAL.

          CLEAR: dt_atual, dt_ini, wgjahr, tgjahr[].

          dt_atual = sy-datum.
          dt_ini = ( dt_atual - s_qds-low ).

          wgjahr-sign   = 'I'.
          wgjahr-option = 'EQ'.
          wgjahr-low    = dt_ini+0(4).
          wgjahr-high   = dt_atual+0(4).
          APPEND wgjahr TO tgjahr.

          IF s_lifnr IS INITIAL.

            SELECT *
              FROM lfa1 INTO TABLE it_lfa1
               WHERE lifnr IN ( SELECT  lifnr FROM bsak
                                WHERE bukrs IN s_bukrs
                                AND   lifnr = lfa1~lifnr
                                AND   budat <= dt_ini )
               AND land1 IN  s_land_1
               AND ktokk IN  s_ktokk
               AND sperr NE 'X'
               AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ lfa1~lifnr AND bukrs IN p_bukrs ) ).

          ELSE.


            SELECT lifnr
                FROM bsak
                INTO TABLE @DATA(t_bsak)
                WHERE bukrs IN @s_bukrs
                  AND lifnr IN @s_lifnr
                  AND budat <= @dt_ini.

            SELECT * FROM lfa1 AS a
                INTO TABLE it_lfa1
                FOR ALL ENTRIES IN t_bsak
                WHERE lifnr = t_bsak-lifnr
                  AND land1 IN  s_land_1
                  AND ktokk IN  s_ktokk
                  AND sperr NE 'X'
                  AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) ).

*            SELECT *
*              FROM LFA1 INTO TABLE IT_LFA1
*               WHERE  LIFNR IN ( SELECT  LIFNR FROM BSAK
*                                  WHERE LIFNR IN  S_LIFNR
*                                  AND   BUKRS IN S_BUKRS
*                                  AND   BUDAT <= DT_INI )
*             AND    LAND1 IN  S_LAND_1
*             AND    KTOKK IN  S_KTOKK
*             AND    SPERR NE 'X'.

          ENDIF.

          CHECK it_lfa1[] IS NOT INITIAL.

          SELECT
            bukrs
            lifnr
            gjahr
            budat
          FROM bsak
          INTO TABLE it_bsak
          FOR ALL ENTRIES IN it_lfa1
          WHERE
            bukrs IN s_bukrs        AND
            lifnr EQ it_lfa1-lifnr  AND
            gjahr IN tgjahr.

          MOVE-CORRESPONDING it_bsak TO it_for_b.

          SELECT
            bukrs
            lifnr
            gjahr
            budat
          FROM bsik
          INTO  TABLE it_bsik
          FOR ALL ENTRIES IN it_lfa1
          WHERE
            bukrs IN s_bukrs        AND
            lifnr EQ it_lfa1-lifnr  AND
            gjahr IN tgjahr         AND
            budat <= dt_ini.

          SORT it_bsik BY bukrs lifnr gjahr budat ASCENDING.

          LOOP AT it_bsik INTO DATA(wa_bsik).
            APPEND wa_bsik TO it_for_b.
            CLEAR wa_bsik.
          ENDLOOP.

*          IF IT_FOR_B[] IS INITIAL.

          DATA(it_lfa1_bkp) = it_lfa1[].


          SELECT
            bukrs
            lifnr
            gjahr
            budat
          FROM bsak
          INTO TABLE it_bsak
          FOR ALL ENTRIES IN it_lfa1
          WHERE
            bukrs IN s_bukrs        AND
            lifnr EQ it_lfa1-lifnr  AND
            gjahr NOT IN tgjahr.

          MOVE-CORRESPONDING it_bsak TO it_for_b.

          SELECT
            bukrs
            lifnr
            gjahr
            budat
          FROM bsik
          INTO  TABLE it_bsik
          FOR ALL ENTRIES IN it_lfa1
          WHERE
            bukrs IN s_bukrs        AND
            lifnr EQ it_lfa1-lifnr  AND
            gjahr NOT IN tgjahr         AND
            budat <= dt_ini.

          SORT it_bsik BY bukrs lifnr gjahr budat ASCENDING.

          LOOP AT it_bsik INTO wa_bsik.
            APPEND wa_bsik TO it_for_b.
            CLEAR wa_bsik.
          ENDLOOP.

*          ENDIF.

          CHECK  it_for_b[] IS INITIAL.

          SELECT *
            FROM kna1 AS a
            INTO CORRESPONDING FIELDS OF TABLE it_kna1
            FOR ALL ENTRIES IN it_lfa1
          WHERE stcd1   EQ it_lfa1-stcd1
            AND stcd2   EQ it_lfa1-stcd2
            AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).

          CHECK it_kna1 IS NOT INITIAL.

          "Seleção dados cliente ( Setor ativ, org venda, canal distrib.
          SELECT * FROM knvv INTO TABLE it_knvv
          FOR ALL ENTRIES IN it_kna1
          WHERE kunnr EQ it_kna1-kunnr.

          SELECT  bukrs  kunnr  gjahr  budat
            FROM bsad
            INTO TABLE it_bsad
            FOR ALL ENTRIES IN it_kna1
          WHERE bukrs   IN s_bukrs
          AND   kunnr   EQ it_kna1-kunnr
          AND   gjahr   IN tgjahr
          AND   budat <=  dt_ini.

          MOVE-CORRESPONDING it_bsad TO it_cli_b.


          SELECT  bukrs  kunnr  gjahr  budat
            FROM bsid
            INTO  TABLE it_bsid
            FOR ALL ENTRIES IN it_kna1
          WHERE bukrs   IN s_bukrs
          AND   kunnr   EQ it_kna1-kunnr
          AND   gjahr   IN tgjahr
          AND   budat <=  dt_ini.

          LOOP AT it_bsid INTO DATA(wa_bsid).
            APPEND wa_bsid  TO it_cli_b.
            CLEAR wa_bsid.
          ENDLOOP.

        ELSEIF s_ina IS NOT INITIAL.

          CLEAR: dt_atual, dt_ini, wgjahr.
          REFRESH tgjahr.

          dt_atual = sy-datum.
          dt_ini = ( dt_atual - s_qds-low ).

          wgjahr-sign   = 'I'.
          wgjahr-option = 'EQ'.
          wgjahr-low    = dt_ini+0(4).
          wgjahr-high   = dt_atual+0(4).
          APPEND wgjahr TO tgjahr.

          SELECT *
            FROM kna1 AS a
            INTO CORRESPONDING FIELDS OF TABLE it_kna1
          WHERE kunnr IN s_kunnr
           AND  land1 IN s_land1
           AND  ktokd IN s_ktokd
           AND  sperr NE 'X'
            AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).

          CHECK it_kna1 IS NOT INITIAL.

          SELECT *
            FROM bsad
            INTO CORRESPONDING FIELDS OF TABLE it_bsad
            FOR ALL ENTRIES IN  it_kna1
          WHERE  kunnr EQ it_kna1-kunnr
            AND bukrs IN s_bukrs
            AND gjahr IN tgjahr
            AND budat <= dt_ini.

          MOVE-CORRESPONDING it_bsad TO it_cli_b.

          SELECT *
            FROM bsid
            INTO CORRESPONDING FIELDS OF TABLE it_bsid
            FOR ALL ENTRIES IN  it_kna1
          WHERE bukrs IN s_bukrs
            AND kunnr EQ it_kna1-kunnr
            AND gjahr IN tgjahr
            AND budat <= dt_ini.

          LOOP AT it_bsid INTO wa_bsid.
            APPEND wa_bsid TO it_cli_b.
            CLEAR wa_bsid.
          ENDLOOP.

          CHECK  it_cli_b IS INITIAL.

          SELECT *
            FROM lfa1 AS a
            INTO CORRESPONDING FIELDS OF TABLE it_lfa1
           FOR ALL ENTRIES IN it_kna1
         WHERE stcd1  EQ it_kna1-stcd1
          AND  stcd2  EQ it_kna1-stcd2
          AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) ).

          CHECK it_lfa1 IS NOT INITIAL.


          SELECT *
            FROM bsak
            INTO CORRESPONDING FIELDS OF TABLE it_bsak
            FOR ALL ENTRIES IN it_lfa1
         WHERE lifnr EQ it_lfa1-lifnr
         AND   bukrs IN s_bukrs
         AND   gjahr IN tgjahr
         AND   budat <= dt_ini.

          MOVE-CORRESPONDING it_bsak TO it_for_b.

          SELECT *
            FROM bsik
            INTO CORRESPONDING FIELDS OF TABLE it_bsik
            FOR ALL ENTRIES IN it_lfa1
           WHERE lifnr EQ it_lfa1-lifnr
            AND  bukrs IN s_bukrs
            AND  gjahr IN tgjahr
            AND  budat <= dt_ini.

          LOOP AT it_bsik INTO wa_bsik.
            APPEND wa_bsik TO it_for_b.
            CLEAR wa_bsik.
          ENDLOOP.

        ENDIF.

      ELSE.
*--> IR116986 / CS1037935
        IF s1_erdat IS NOT INITIAL.
          LOOP AT s1_erdat.
            s_erdat-sign   = s1_erdat-sign.
            s_erdat-option = s1_erdat-option.
            s_erdat-low    = s1_erdat-low.
            s_erdat-high   = s1_erdat-high.
            APPEND s_erdat TO s_erdat.
          ENDLOOP.
        ENDIF.
        IF s1_ernam[] IS NOT INITIAL.
          LOOP AT s1_ernam.
            s_ernam-sign   = s1_ernam-sign.
            s_ernam-option = s1_ernam-option.
            s_ernam-low    = s1_ernam-low.
            s_ernam-high   = s1_ernam-high.
            APPEND s_ernam TO s_ernam.
          ENDLOOP.
        ENDIF.
*<-- IR116986 / CS1037935
        SELECT *
          FROM kna1 AS a
          INTO TABLE it_kna1
          WHERE kunnr IN s_kunnr
            AND land1 IN s_land1
            AND ktokd IN s_ktokd
*--> IR116986 / CS1037935
            AND erdat IN s_erdat
            AND ernam IN s_ernam
*<-- IR116986 / CS1037935
            AND stkzn IN pessoa
            AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).

        SELECT *
          FROM lfa1 AS a
          INTO TABLE it_lfa1
         WHERE lifnr IN s_lifnr
           AND land1 IN s_land_1
           AND ktokk IN s_ktokk
*--> IR116986 / CS1037935
            AND erdat IN s_erdat
            AND ernam IN s_ernam
*<-- IR116986 / CS1037935
          AND stkzn IN pessoa
          AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) ).

      ENDIF.

    ELSE.
      SELECT *
        FROM kna1 AS a
       INNER JOIN cdhdr AS b ON a~kunnr = b~objectid
        INTO CORRESPONDING FIELDS OF TABLE it_kna1
       WHERE b~username IN s_usern
         AND b~udate    IN s_date
         AND b~tcode    IN s_trans
         AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).

      SELECT *
        FROM lfa1 AS a
       INNER JOIN cdhdr AS b ON a~lifnr = b~objectid
        INTO CORRESPONDING FIELDS OF TABLE it_lfa1
       WHERE b~username IN s_usern
         AND b~udate    IN s_date
         AND b~tcode    IN s_trans
        AND ( EXISTS ( SELECT * FROM lfb1 AS b WHERE lifnr EQ a~lifnr AND bukrs IN p_bukrs ) ).

      DELETE ADJACENT DUPLICATES FROM it_kna1.
      DELETE ADJACENT DUPLICATES FROM it_lfa1.
    ENDIF.
  ENDIF.

  IF it_kna1 IS NOT INITIAL.
    SELECT * FROM knb1 INTO TABLE t_knb1 FOR ALL ENTRIES IN it_kna1 WHERE kunnr EQ it_kna1-kunnr AND bukrs IN p_bukrs.
  ENDIF.

  IF  it_lfa1 IS NOT INITIAL.
    SELECT * FROM lfb1 INTO TABLE t_lfb1 FOR ALL ENTRIES IN it_lfa1 WHERE lifnr EQ it_lfa1-lifnr AND bukrs IN p_bukrs.
  ENDIF.



ENDFORM.                    "F_SELECIONA_DADOS
"F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
FORM f_organizar_dados.



  IF ck_cli IS NOT INITIAL.
    "Cliente

    IF s_ban IS NOT INITIAL.
      PERFORM preenche_dados_bancarios USING 'K'.

    ELSEIF s_ina IS NOT INITIAL.

      SORT it_cli_b BY budat ASCENDING.
      SORT it_for_b BY budat ASCENDING.

      LOOP AT t_knb1 INTO w_knb1.
        wa_saida-bukrs = w_knb1-bukrs.
        wa_saida-akont = w_knb1-akont.
        LOOP AT it_kna1 INTO wa_kna1 WHERE kunnr EQ w_knb1-kunnr.

          READ TABLE it_cli_b INTO wa_cli_b WITH KEY kunnr = wa_kna1-kunnr.
          IF sy-subrc = 0.
            wa_saida-kunnr   =  wa_kna1-kunnr.
            wa_saida-name1   =  wa_kna1-name1.
            wa_saida-ort01   =  wa_kna1-ort01.
            wa_saida-regio   =  wa_kna1-regio.
            wa_saida-land1   =  wa_kna1-land1.
            wa_saida-ktokd   =  wa_kna1-ktokd.

            CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
              EXPORTING
                i_datum_bis = sy-datum
                i_datum_von = wa_cli_b-budat
              IMPORTING
                e_tage      = wa_saida-qtd_d.

            APPEND wa_saida TO it_saida.
            CLEAR: wa_saida, wa_cli_b.

          ELSE.

            READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd1 = wa_kna1-stcd1
                                                     stcd2 = wa_kna1-stcd2.
            IF sy-subrc = 0.
              READ TABLE it_for_b INTO wa_for_b WITH KEY lifnr = wa_lfa1-lifnr.
              IF sy-subrc = 0.

                wa_saida-lifnr   =  wa_lfa1-lifnr.
                wa_saida-name1   =  wa_lfa1-name1.
                wa_saida-ort01   =  wa_lfa1-ort01.
                wa_saida-regio   =  wa_lfa1-regio.
                wa_saida-land1   =  wa_lfa1-land1.
                wa_saida-ktokk   =  wa_lfa1-ktokk.

                CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
                  EXPORTING
                    i_datum_bis = sy-datum
                    i_datum_von = wa_for_b-budat
                  IMPORTING
                    e_tage      = wa_saida-qtd_d.

                IF s_qds IS NOT INITIAL AND wa_saida-qtd_d >= s_qds-low.
                  APPEND wa_saida TO it_saida.
                ENDIF.


                READ TABLE t_lfb1 INTO w_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
                IF sy-subrc EQ 0.
                  wa_saida-bukrs = w_lfb1-bukrs.
                  wa_saida-akont = w_lfb1-akont.
                ENDIF.
                CLEAR: wa_saida, wa_for_b, w_lfb1.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDLOOP.

    ELSE.
      CLEAR: w_lfb1.

      LOOP AT t_knb1 INTO w_knb1.
        wa_saida-bukrs = w_knb1-bukrs.
        wa_saida-akont = w_knb1-akont.
        LOOP AT it_kna1 INTO wa_kna1 WHERE kunnr EQ w_knb1-kunnr.
          wa_saida-chg_log = icon_protocol.
          wa_saida-kunnr = wa_kna1-kunnr.
          wa_saida-land1 = wa_kna1-land1.
          wa_saida-name1 = wa_kna1-name1.
          wa_saida-name2 = wa_kna1-name2.
          wa_saida-ort01 = wa_kna1-ort01.
          wa_saida-pstlz = wa_kna1-pstlz.
          wa_saida-regio = wa_kna1-regio.
          wa_saida-sortl = wa_kna1-sortl.
          wa_saida-stras = wa_kna1-stras.
          wa_saida-telf1 = wa_kna1-telf1.
          wa_saida-telfx = wa_kna1-telfx.
          wa_saida-xcpdk = wa_kna1-xcpdk.
          wa_saida-adrnr = wa_kna1-adrnr.
          wa_saida-mcod1 = wa_kna1-mcod1.
          wa_saida-mcod2 = wa_kna1-mcod2.
          wa_saida-mcod3 = wa_kna1-mcod3.
          wa_saida-anred = wa_kna1-anred.
          wa_saida-aufsd = wa_kna1-aufsd.
          wa_saida-bahne = wa_kna1-bahne.
          wa_saida-bahns = wa_kna1-bahns.
          wa_saida-bbbnr = wa_kna1-bbbnr.
          wa_saida-bbsnr = wa_kna1-bbsnr.
          wa_saida-begru = wa_kna1-begru.
          wa_saida-brsch = wa_kna1-brsch.
          wa_saida-bubkz = wa_kna1-bubkz.
          wa_saida-datlt = wa_kna1-datlt.
          wa_saida-erdat = wa_kna1-erdat.
          wa_saida-ernam = wa_kna1-ernam.
          wa_saida-exabl = wa_kna1-exabl.
          wa_saida-faksd = wa_kna1-faksd.
          wa_saida-fiskn = wa_kna1-fiskn.
          wa_saida-knazk = wa_kna1-knazk.
          wa_saida-knrza = wa_kna1-knrza.
          wa_saida-konzs = wa_kna1-konzs.
          wa_saida-ktokd = wa_kna1-ktokd.
          wa_saida-kukla = wa_kna1-kukla.
          wa_saida-lifnr = wa_kna1-lifnr.
          wa_saida-lifsd = wa_kna1-lifsd.
          wa_saida-locco = wa_kna1-locco.
          wa_saida-loevm = wa_kna1-loevm.
          wa_saida-name3 = wa_kna1-name3.
          wa_saida-name4 = wa_kna1-name4.
          wa_saida-niels = wa_kna1-niels.
          wa_saida-ort02 = wa_kna1-ort02.
          wa_saida-pfach = wa_kna1-pfach.
          wa_saida-pstl2 = wa_kna1-pstl2.
          wa_saida-counc = wa_kna1-counc.
          wa_saida-cityc = wa_kna1-cityc.
          wa_saida-rpmkr = wa_kna1-rpmkr.
          wa_saida-sperr = wa_kna1-sperr.
          wa_saida-spras = wa_kna1-spras.
          wa_saida-stcd1 = wa_kna1-stcd1.
          wa_saida-stcd2 = wa_kna1-stcd2.
          wa_saida-stcd3 = wa_kna1-stcd3.
          wa_saida-stkza = wa_kna1-stkza.
          wa_saida-stkzu = wa_kna1-stkzu.
          wa_saida-telbx = wa_kna1-telbx.
          wa_saida-stkzn = wa_kna1-stkzn.

          APPEND wa_saida TO it_saida.
          CLEAR: w_lfb1.
        ENDLOOP.
      ENDLOOP.
*    CLEAR:IT_SAIDA.

    ENDIF.

* Verificar se teve ORDEM DE VENDAS durante período escolhido:
    IF it_saida[] IS NOT INITIAL.

      CLEAR: dt_atual, dt_ini.

      dt_atual = sy-datum.
      dt_ini = ( dt_atual - s_qds-low ).

      SELECT vb~kunnr
      FROM vbak AS vb
      INTO TABLE @DATA(t_kunnr)
      FOR ALL ENTRIES IN @it_saida
      WHERE vb~erdat >= @dt_ini AND
            vb~erdat <= @dt_atual AND
            vb~vkorg IN @s_bukrs   AND
            vb~auart IN ( 'ZDEF', 'ZELI', 'ZELV', 'ZEPT', 'ZEXI', 'ZEXP', 'ZFEX',
                          'ZFFE', 'ZFTE', 'ZFUE', 'ZFUT', 'ZMIT', 'ZODF', 'ZOFE',
                          'ZOSM', 'ZPER', 'ZPOR', 'ZSAN', 'ZSEM' ) AND
            vb~kunnr = @it_saida-kunnr.

      IF sy-subrc = 0.

        DELETE ADJACENT DUPLICATES FROM t_kunnr[] COMPARING kunnr.
        SORT t_kunnr[] BY kunnr ASCENDING.

        LOOP AT t_kunnr[] INTO DATA(w_kunnr).

          DELETE it_saida[] WHERE kunnr = w_kunnr-kunnr.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ELSE.

    IF ck_for IS NOT INITIAL AND s_ban IS NOT INITIAL.
      PERFORM preenche_dados_bancarios USING 'L'.
    ELSEIF ck_for IS NOT INITIAL AND s_ina IS NOT INITIAL.

      SORT it_for_b BY budat DESCENDING.
      CLEAR: w_lfb1.

      LOOP AT t_lfb1 INTO w_lfb1.
        wa_saida-bukrs = w_lfb1-bukrs.
        wa_saida-akont = w_lfb1-akont.

        LOOP AT it_lfa1 INTO wa_lfa1 WHERE lifnr EQ w_lfb1-lifnr.
          READ TABLE it_for_b INTO wa_for_b WITH KEY lifnr = wa_lfa1-lifnr.
          IF sy-subrc = 0.
            wa_saida-lifnr   =  wa_lfa1-lifnr.
            wa_saida-name1   =  wa_lfa1-name1.
            wa_saida-ort01   =  wa_lfa1-ort01.
            wa_saida-regio   =  wa_lfa1-regio.
            wa_saida-land1   =  wa_lfa1-land1.
            wa_saida-ktokk   =  wa_lfa1-ktokk.

            CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
              EXPORTING
                i_datum_bis = sy-datum
                i_datum_von = wa_for_b-budat
              IMPORTING
                e_tage      = wa_saida-qtd_d.

            IF s_qds IS NOT INITIAL AND wa_saida-qtd_d >= s_qds-low.
              APPEND wa_saida TO it_saida.
            ENDIF.
            CLEAR: wa_saida, wa_for_b.

          ELSE.

            READ TABLE it_kna1 INTO wa_kna1 WITH KEY stcd1 = wa_lfa1-stcd1
                                                     stcd2 = wa_lfa1-stcd2.
            IF sy-subrc = 0.
              READ TABLE it_cli_b INTO wa_cli_b WITH KEY kunnr = wa_kna1-kunnr.
              IF sy-subrc = 0.
                wa_saida-kunnr   =  wa_kna1-kunnr.
                wa_saida-name1   =  wa_kna1-name1.
                wa_saida-ort01   =  wa_kna1-ort01.
                wa_saida-regio   =  wa_kna1-regio.
                wa_saida-land1   =  wa_kna1-land1.
                wa_saida-ktokd   =  wa_kna1-ktokd.

                CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
                  EXPORTING
                    i_datum_bis = sy-datum
                    i_datum_von = wa_cli_b-budat
                  IMPORTING
                    e_tage      = wa_saida-qtd_d.

                APPEND wa_saida TO it_saida.
                CLEAR: wa_saida, wa_cli_b.
              ENDIF.
            ENDIF.
            CLEAR: wa_kna1.
          ENDIF.
          CLEAR: w_lfb1, wa_lfa1.
        ENDLOOP.
      ENDLOOP.

    ELSE.
      "Fornecedor
      CLEAR: w_lfb1.
      LOOP AT t_lfb1 INTO w_lfb1.
        wa_saida-bukrs = w_lfb1-bukrs.
        wa_saida-akont = w_lfb1-akont.

        LOOP AT it_lfa1 INTO wa_lfa1 WHERE lifnr EQ w_lfb1-lifnr.
          wa_saida-chg_log = icon_protocol.
          wa_saida-lifnr = wa_lfa1-lifnr.
          wa_saida-land1 = wa_lfa1-land1.
          wa_saida-name1 = wa_lfa1-name1.
          wa_saida-name2 = wa_lfa1-name2.
          wa_saida-name3 = wa_lfa1-name3.
          wa_saida-name4 = wa_lfa1-name4.
          wa_saida-ort01 = wa_lfa1-ort01.
          wa_saida-ort02 = wa_lfa1-ort02.
          wa_saida-pfach = wa_lfa1-pfach.
          wa_saida-pstl2 = wa_lfa1-pstl2.
          wa_saida-pstlz = wa_lfa1-pstlz.
          wa_saida-regio = wa_lfa1-regio.
          wa_saida-sortl = wa_lfa1-sortl.
          wa_saida-stras = wa_lfa1-stras.
          wa_saida-adrnr = wa_lfa1-adrnr.
          wa_saida-mcod1 = wa_lfa1-mcod1.
          wa_saida-mcod2 = wa_lfa1-mcod2.
          wa_saida-mcod3 = wa_lfa1-mcod3.
          wa_saida-anred = wa_lfa1-anred.
          wa_saida-bahns = wa_lfa1-bahns.
          wa_saida-bbbnr = wa_lfa1-bbbnr.
          wa_saida-bbsnr = wa_lfa1-bbsnr.
          wa_saida-begru = wa_lfa1-begru.
          wa_saida-brsch = wa_lfa1-brsch.
          wa_saida-bubkz = wa_lfa1-bubkz.
          wa_saida-datlt = wa_lfa1-datlt.
          wa_saida-dtams = wa_lfa1-dtams.
          wa_saida-dtaws = wa_lfa1-dtaws.
          wa_saida-erdat = wa_lfa1-erdat.
          wa_saida-ernam = wa_lfa1-ernam.
          wa_saida-esrnr = wa_lfa1-esrnr.
          wa_saida-konzs = wa_lfa1-konzs.
          wa_saida-ktokk = wa_lfa1-ktokk.
          wa_saida-kunnr = wa_lfa1-kunnr.
          wa_saida-lnrza = wa_lfa1-lnrza.
          wa_saida-loevm = wa_lfa1-loevm.
          wa_saida-sperr = wa_lfa1-sperr.
          wa_saida-sperm = wa_lfa1-sperm.
          wa_saida-spras = wa_lfa1-spras.
          wa_saida-stcd1 = wa_lfa1-stcd1.
          wa_saida-stcd2 = wa_lfa1-stcd2.
          wa_saida-stkza = wa_lfa1-stkza.
          wa_saida-stkzu = wa_lfa1-stkzu.
          wa_saida-telbx = wa_lfa1-telbx.
          wa_saida-telf1 = wa_lfa1-telf1.
          wa_saida-telf2 = wa_lfa1-telf2.
          wa_saida-telfx = wa_lfa1-telfx.
          wa_saida-teltx = wa_lfa1-teltx.
          wa_saida-telx1 = wa_lfa1-telx1.
          wa_saida-xcpdk = wa_lfa1-xcpdk.
          wa_saida-xzemp = wa_lfa1-xzemp.
          wa_saida-vbund = wa_lfa1-vbund.
          wa_saida-fiskn = wa_lfa1-fiskn.
          wa_saida-stkzn = wa_lfa1-stkzn.

          wa_saida-stenr = wa_lfa1-stenr.
          wa_saida-kraus = wa_lfa1-kraus.
          wa_saida-gbdat = wa_lfa1-gbdat.

* Campos new
          wa_saida-crtn = wa_lfa1-crtn.

          SELECT ddtext
            UP TO 1 ROWS
            FROM dd07t
            INTO @DATA(lv_ddtext)
            WHERE domname EQ 'J_1BCRTN'
              AND ddlanguage EQ @wa_lfa1-spras
              AND domvalue_l EQ @wa_lfa1-crtn.
          ENDSELECT.

          IF sy-subrc IS INITIAL AND lv_ddtext IS NOT INITIAL.
            wa_saida-val_text = lv_ddtext. " Dif
          ELSE.
            wa_saida-val_text = 'N/A'.
          ENDIF.

          wa_saida-icmstaxpay = wa_lfa1-icmstaxpay.

          SELECT SINGLE j_1bicmstaxpayx
            INTO @DATA(lv_text)
            FROM j_1bticmstaxpayt
            WHERE spras EQ @wa_lfa1-spras
              AND j_1bicmstaxpay EQ @wa_lfa1-icmstaxpay.

          IF sy-subrc IS INITIAL AND lv_text IS NOT INITIAL.
            wa_saida-jtaxpayx = lv_text. " Dif
          ELSE.
            wa_saida-jtaxpayx = 'N/A'.
          ENDIF.

          wa_saida-indtyp = wa_lfa1-indtyp.

          SELECT SINGLE j_1bindtypx
            INTO @DATA(lv_textx)
            FROM j_1btindtypt
            WHERE spras EQ @wa_lfa1-spras
              AND j_1bindtyp EQ @wa_lfa1-indtyp.

          IF sy-subrc IS INITIAL AND lv_textx IS NOT INITIAL.
            wa_saida-indtypx = lv_textx. " Dif
          ELSE.
            wa_saida-indtypx = 'N/A'.
          ENDIF.

          "MODIFICAÇÃO 14/09/2016 INÍCIO

          IF wa_saida-stkzn EQ 'X'.
            IF wa_saida-ktokk EQ 'ZPRF'.
              wa_saida-stcd3 = wa_lfa1-stcd3.
            ELSE.
              wa_saida-stcdx = wa_lfa1-stcd3.
            ENDIF.
          ELSE.
            wa_saida-stcd3 = wa_lfa1-stcd3.
          ENDIF.

          "MODIFICAÇÃO 14/09/2016 FIM

          APPEND wa_saida TO it_saida.
          CLEAR: wa_saida, w_lfb1.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

* Verificar se teve PEDIDO DE COMPRAS durante período escolhido:
    IF s_ina IS NOT INITIAL.
      IF it_saida[] IS NOT INITIAL.

        CLEAR: dt_atual, dt_ini.

        dt_atual = sy-datum.
        dt_ini = ( dt_atual - s_qds-low ).

        SELECT ek~lifnr
        FROM ekko AS ek
        INTO TABLE @DATA(t_lifnr)
        FOR ALL ENTRIES IN @it_saida
        WHERE ek~lifnr = @it_saida-lifnr AND
              ek~bukrs IN @s_bukrs       AND
              ek~bsart IN ( 'ZDBP', 'ZDEF', 'ZFTE', 'ZSEM', 'PCE', 'PCEF', 'PCEI',
                            'PCS', 'PCSI', 'PSEF' ) AND
              ek~aedat >= @dt_ini AND
              ek~aedat <= @dt_atual.

        IF sy-subrc = 0.

          DELETE ADJACENT DUPLICATES FROM t_lifnr[] COMPARING lifnr.
          SORT t_lifnr[] BY lifnr ASCENDING.

          LOOP AT t_lifnr[] INTO DATA(w_lifnr).

            DELETE it_saida[] WHERE lifnr = w_lifnr-lifnr.

          ENDLOOP.

        ENDIF.

      ENDIF.
    ENDIF.

    "Verifica se tem movimentação no Sigam
    IF it_saida[] IS NOT INITIAL.

      SELECT lifnr, dt_atualizacao
        FROM zfit0152
        INTO TABLE @DATA(t_zfit0152_forn)
        WHERE lifnr NE ' '.

      SELECT kunnr, dt_atualizacao
        FROM zfit0152
        INTO TABLE @DATA(t_zfit0152_cli)
        WHERE kunnr NE ' '.

      LOOP AT t_zfit0152_forn[] INTO DATA(w_zfit0152_forn).
        DELETE it_saida[] WHERE lifnr = w_zfit0152_forn-lifnr.
      ENDLOOP.

      LOOP AT t_zfit0152_cli[] INTO DATA(w_zfit0152_cli).
        DELETE it_saida[] WHERE kunnr = w_zfit0152_cli-kunnr.
      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.                    "F_ORGANIZAR_DADOS

*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv.

  IF ck_cli IS NOT INITIAL.

    IF s_ban IS NOT INITIAL.

      PERFORM alv_preenche_cat USING:
      'KUNNR'               'Nº Cliente  '                                               '13'  '' ''   '',
      'LAND1'               'País'                                                       '04'  '' ''   '',
      'NAME1'               'Nome 1'                                                     '30'  '' ''   '',
      'NAME2'               'Nome 2'                                                     '15'  '' ''   '',
      'ORT01'               'Local'                                                      '15'  '' ''   '',
      'PSTLZ'               'Cod.Postal'                                                 '10'  '' ''   '',
      'REGIO'               'Região'                                                     '06'  '' ''   '',
      'SORTL'               'C. de Seleção'                                              '10'  '' ''   '',
      'BEGRU'               'Grupo autorizações'                                         '12'  '' ''   '',
      'BRSCH'               'Cód. setor industrial'                                      '06'  '' ''   '',
      'BUBKZ'               'Dígito de controle da matrícula internacional da empresa'   '12'  '' ''   '',
      'KONZS'               'Chave do grupo de empresas'                                 '15'  '' ''   '',
      'KTOKD'               'C.cliente'                                                  '10'  '' ''   '',
      'KUKLA'               'Classificação de clientes'                                  '12'  '' ''   '',
      'LIFNR'               'Nº conta do fornecedor'                                     '12'  '' ''   '',
      'BUKRS'               'Empresa               '                                     '08'  '' ''   '',
      'BANKS'               'País do Banco  '                                            '03'  '' ''   '',
      'BANKL'               'Chave do Banco  '                                           '15'  '' ''   '',
      'BANKN'               'Nº Conta  '                                                 '18'  '' ''   '',
      'BKONT'               'CC  '                                                       '02'  '' ''   '',
      'BVTYP'               'Tipo de Banco  '                                            '04'  '' ''   '',
      'BKREF'               'Ind. de Ref.  '                                             '20'  '' ''   ''.

    ELSEIF s_ina IS NOT INITIAL.
      PERFORM alv_preenche_cat USING:
      'KUNNR'               'Código'               '13'  '' ''   '',
      'NAME1'               'Nome'                 '30'  '' ''   '',
      'ORT01'               'Cidade'               '30'  '' ''   '',
      'REGIO'               'UF'                   '03'  '' ''   '',
      'LAND1'               'Pais'                 '05'  '' ''   '',
      'KTOKK'               'Gpo.Ctas'             '08'  '' ''   '',
      'QTD_D'               'Qte Dias'             '06'  '' ''   ''.

    ELSE.

      PERFORM alv_preenche_cat USING:
         'CHG_LOG'             'Logs'                                                       '5'  '' ''   '',
         'KUNNR'               'Nº Cliente  '                                               '13'  '' ''   '',
         'LAND1'               'País'                                                       '04'  '' ''   '',
         'NAME1'               'Nome 1'                                                     '30'  '' ''   '',
         'NAME2'               'Nome 2'                                                     '15'  '' ''   '',
         'GBDAT'               'Dt. Nasc.'                                                  '10'  '' ''   '',
         'ORT01'               'Local'                                                      '15'  '' ''   '',
         'PSTLZ'               'Cod.Postal'                                                 '10'  '' ''   '',
         'REGIO'               'Região'                                                     '06'  '' ''   '',
         'SORTL'               'C. de Seleção'                                              '10'  '' ''   '',
         'STRAS'               'Rua e Nº'                                                   '30'  '' ''   '',
         'TELF1'               'telefone'                                                   '15'  '' ''   '',
         'TELFX'               'telefax'                                                    '15'  '' ''   '',
         'XCPDK'               'Código'                                                     '20'  '' ''   '',
         'ADRNR'               'Endereço'                                                   '12'  '' ''   '',
         'MCOD1'               'Pesquisa para utilização matchcode'                         '30'  '' ''   '',
         'MCOD2'               'Pesquisa para utilização matchcode'                         '30'  '' ''   '',
         'MCOD3'               'Pesquisa para utilização matchcode'                         '30'  '' ''   '',
         'ANRED'               'Forma de tratamento'                                        '15'  '' ''   '',
         'AUFSD'               'Bloqueio de ordem centralizado para cliente'                '20'  '' ''   '',
         'BAHNE'               'Estação ferroviária para expedição por expresso'            '20'  '' ''   '',
         'BAHNS'               'Registro Nacional dos Transp.de Cargas'                     '12'  '' ''   '',
         'BBBNR'               'Nº global de localização (parte 1)'                         '12'  '' ''   '',
         'BBSNR'               'Número global de localização (parte 2)'                     '12'  '' ''   '',
         'BEGRU'               'Grupo autorizações'                                         '12'  '' ''   '',
         'BRSCH'               'Cód. setor industrial'                                      '06'  '' ''   '',
         'BUBKZ'               'Dígito de controle da matrícula internacional da empresa'   '12'  '' ''   '',
         'DATLT'               'Nº linha de transmissão de dados'                           '12'  '' ''   '',
         'ERDAT'               'Data de criação do registro'                                '12'  '' ''   '',
         'ERNAM'               'Nome do responsável que adicionou o objeto'                 '12'  '' ''   '',
         'EXABL'               'Código: existência de pontos de descarga'                   '12'  '' ''   '',
         'FAKSD'               'Bloqueio centralizado de faturamento para cliente'          '12'  '' ''   '',
         'FISKN'               'Nº conta do registro mestre com o endereço fiscal'          '12'  '' ''   '',
         'KNAZK'               'Cal do horário de trabalho do cliente'                      '12'  '' ''   '',
         'KNRZA'               'Nº conta de um pag div'                                     '12'  '' ''   '',
         'KONZS'               'Chave do grupo de empresas'                                 '15'  '' ''   '',
         'KTOKD'               'C.cliente'                                                  '10'  '' ''   '',
         'KUKLA'               'Classificação de clientes'                                  '12'  '' ''   '',
         'LIFNR'               'Nº conta do fornecedor'                                     '12'  '' ''   '',
         'BUKRS'               'Empresa               '                                     '08'  '' ''   '',
         'AKONT'               'Conta Conciliação     '                                     '15'  '' ''   '',
         'LIFSD'               'Bloqueio de remessa centralizado para cliente'              '12'  '' ''   '',
         'LOCCO'               'Coordenadas do local'                                       '12'  '' ''   '',
         'LOEVM'               'Marcação central para eliminação do registro mestre'        '12'  '' ''   '',
         'NAME3'               'Nome 3'                                                     '12'  '' ''   '',
         'NAME4'               'Nome 4'                                                     '12'  '' ''   '',
         'NIELS'               'Região de Nielsen'                                          '12'  '' ''   '',
         'ORT02'               'Bairro'                                                     '12'  '' ''   '',
         'PFACH'               'CxPostal'                                                   '12'  '' ''   '',
         'PSTL2'               'Código postal da cx.postal'                                 '12'  '' ''   '',
         'COUNC'               'Código de distrito'                                         '12'  '' ''   '',
         'CITYC'               'Código da cidade'                                           '12'  '' ''   '',
         'RPMKR'               'Mercado regional'                                           '12'  '' ''   '',
         'STKZN'               'Pessoa fisica '                                             '08'  '' ''   '',
         'SPERR'               'Bloqueio central contabilização'                            '12'  '' ''   '',
         'SPRAS'               'Cód. idioma'                                                '08'  '' ''   '',
         'STENR'               'NºID Fiscal'                                                '15'  '' ''   '',
         'STCD1'               'Nº ID fiscal 1'                                             '12'  '' ''   '',
         'STCD2'               'Nº ID fiscal 2'                                             '12'  '' ''   '',
         'STCD3'               'Inscr. Estadual'                                            '12'  '' ''   '',
         'STKZA'               'Código:parceiro de negócios sujeito à taxa compensatória?'  '12'  '' ''   '',
         'STKZU'               'Sujeito a IVA'                                              '12'  '' ''   '',
         'TELBX'               'Nº telebox'                                                 '15'  '' ''   '',
         'KRAUS'               'Nº Informação'                                              '15'  '' ''   ''.


    ENDIF.

  ELSE.




    IF s_ban IS NOT INITIAL.

      PERFORM alv_preenche_cat USING:
      'LIFNR'               'N° Conta fornecedor'                                        '13'  '' ''   '',
      'BUKRS'               'Empresa               '                                     '08'  '' ''   '',
      'AKONT'               'Conta Conciliação     '                                     '15'  '' ''   '',
      'LAND1'               'País'                                                       '04'  '' ''   '',
      'NAME1'               'Nome 1'                                                     '30'  '' ''   '',
      'NAME2'               'Nome 2'                                                     '15'  '' ''   '',
      'ORT01'               'Local'                                                      '15'  '' ''   '',
      'PSTLZ'               'Cod.Postal'                                                 '10'  '' ''   '',
      'REGIO'               'Região'                                                     '06'  '' ''   '',
      'SORTL'               'C. de Seleção'                                              '10'  '' ''   '',
      'BEGRU'               'Grupo autorizações'                                         '12'  '' ''   '',
      'BRSCH'               'Cód. setor industrial'                                      '06'  '' ''   '',
      'BUBKZ'               'Dígito de controle da matrícula internacional da empresa'   '12'  '' ''   '',
      'KONZS'               'Chave do grupo de empresas'                                 '15'  '' ''   '',
      'KTOKK'               'Grupo de contas do fornecedor'                              '12'  '' ''   '',
      'KUNNR'               'Nº cliente'                                                 '15'  '' ''   '',
      'BANKS'               'País do Banco  '                                            '03'  '' ''   '',
      'BANKL'               'Chave do Banco  '                                           '15'  '' ''   '',
      'BANKN'               'Nº Conta  '                                                 '18'  '' ''   '',
      'BKONT'               'CC  '                                                       '02'  '' ''   '',
      'BVTYP'               'Tipo de Banco  '                                            '04'  '' ''   '',
      'BKREF'               'Ind. de Ref.  '                                             '20'  '' ''   ''.

    ELSEIF ck_for IS NOT INITIAL AND s_ina IS NOT INITIAL.
      PERFORM alv_preenche_cat USING:
      'LIFNR'              'Código'                       '13'  '' ''   '',
      'BUKRS'              'Empresa'                      '08'  '' ''   '',
      'AKONT'              'Conta Conciliação'            '15'  '' ''   '',
      'NAME1'              'Nome'                         '30'  '' ''   '',
      'ORT01'              'Cidade'                       '30'  '' ''   '',
      'REGIO'              'UF'                           '03'  '' ''   '',
      'LAND1'              'Pais'                         '05'  '' ''   '',
      'KTOKK'              'Gpo.Ctas'                     '08'  '' ''   '',
      'QTD_D'              'Qte Dias'                     '06'  '' ''   ''.

    ELSE.

      "Fornecedor
      PERFORM alv_preenche_cat USING:
           'CHG_LOG'             'Logs'                                                         '5'  '' ''   '',
           'LIFNR'               'N° Conta fornecedor'                                          '13'  '' ''   '',
           'AKONT'               'Conta Conciliação     '                                       '15'  '' ''   '',
           'BUKRS'               'Empresa               '                                       '08'  '' ''   '',
           'AKONT'               'Conta Conciliação     '                                       '15'  '' ''   '',
           'LAND1'               'País'                                                         '04'  '' ''   '',
           'NAME1'               'Nome1'                                                        '35'  '' ''   '',
           'NAME2'               'Nome2'                                                        '15'  '' ''   '',
           'NAME3'               'Nome3'                                                        '15'  '' ''   '',
           'NAME4'               'Nome4'                                                        '10'  '' ''   '',
           'GBDAT'               'Dt. Nasc.'                                                    '10'  '' ''   '',
           'ORT01'               'Local'                                                        '30'  '' ''   '',
           'ORT02'               'Bairro'                                                       '15'  '' ''   '',
           'PFACH'               'CxPostal'                                                     '15'  '' ''   '',
           'PSTL2'               'Cód cx.postal'                                                '15'  '' ''   '',
           'PSTLZ'               'Cód postal'                                                   '15'  '' ''   '',
           'REGIO'               'Região'                                                       '06'  '' ''   '',
           'SORTL'               'Campo de Seleção'                                             '12'  '' ''   '',
           'STRAS'               'Rua'                                                          '35'  '' ''   '',
           'ADRNR'               'Endereço'                                                     '15'  '' ''   '',
           'MCOD1'               'Conceito de pesquisa para utilização matchcode'               '30'  '' ''   '',
           'MCOD2'               'Conceito de pesquisa para utilização matchcode'               '15'  '' ''   '',
           'MCOD3'               'Conceito de pesquisa para utilização matchcode'               '30'  '' ''   '',
           'ANRED'               'Forma de tratamento'                                          '15'  '' ''   '',
           'BAHNS'               'Registro Nacional dos Transp.de Cargas'                       '12'  '' ''   '',
           'BBBNR'               'Nº global de localização (parte 1)'                           '12'  '' ''   '',
           'BBSNR'               'Número global de localização (parte 2)'                       '12'  '' ''   '',
           'BEGRU'               'Grupo autorizações'                                           '12'  '' ''   '',
           'BRSCH'               'Código do setor industrial'                                   '06'  '' ''   '',
           'BUBKZ'               'Dígito de controle da matrícula internacional da empresa'     '12'  '' ''   '',
           'DATLT'               'Nº linha de transmissão de dados'                             '12'  '' ''   '',
           'DTAMS'               'Notificação para intercâmbio de suporte de dados'             '12'  '' ''   '',
           'DTAWS'               'Instruções para intercâmbio de suporte de dados'              '12'  '' ''   '',
           'ERDAT'               'Data de criação do registro'                                  '12'  '' ''   '',
           'ERNAM'               'Grupo de Contas'                                              '12'  '' ''   '',
           'ESRNR'               'Nº participante NDR'                                          '12'  '' ''   '',
           'KONZS'               'Grupo de empresas'                                            '12'  '' ''   '',
           'KTOKK'               'Grupo de contas do fornecedor'                                '12'  '' ''   '',
           'KUNNR'               'Nº cliente'                                                   '15'  '' ''   '',
           'LNRZA'               'Nºconta do receb. alternativo do pagamento'                   '10'  '' ''   '',
           'LOEVM'               'Marcação central para eliminação do registro mestre'          '12'  '' ''   '',
           'SPERR'               'Bloq. central contabilização'                                 '08'  '' ''   '',
           'SPERM'               'Comp. bloqueadas a nível central'                             '08'  '' ''   '',
           'SPRAS'               'Cód. de idioma'                                               '06'  '' ''   '',
           'STCD1'               'Nº ID fiscal 1'                                               '15'  '' ''   '',
           'STCD2'               'Nº ID fiscal 2'                                               '12'  '' ''   '',
           'STKZN'               'P. fisica '                                                   '06'  '' ''   '',
           'STCD3'               'Incr. Estatual'                                               '12'  '' ''   '', "MODIFICAÇÃO 14/09/2016
           'STCDX'               'RG'                                                           '12'  '' ''   '', "MODIFICAÇÃO 14/09/2016
           'STKZA'               'Cód:parceiro de negócios sujeito à taxa compensatória'        '12'  '' ''   '',
           'STKZU'               'Sujeito a IVA'                                                '12'  '' ''   '',
           'TELBX'               'Nº telebox'                                                   '12'  '' ''   '',
           'STENR'               'NºID Fiscal'                                                  '15'  '' ''   '',
           'TELF1'               '1º Nº telefone'                                               '12'  '' ''   '',
           'TELF2'               '2º Nº telefone'                                               '12'  '' ''   '',
           'TELFX'               'Nº telefax'                                                   '12'  '' ''   '',
           'TELTX'               'Nº teletex'                                                   '12'  '' ''   '',
           'TELX1'               'Nº telex'                                                     '12'  '' ''   '',
           'XCPDK'               'Código: a conta é uma conta ocasional'                        '12'  '' ''   '',
           'XZEMP'               'Cód: permitido recebedor de pagamento divergente no doc.'     '08'  '' ''   '',
           'VBUND'               'Nº sociedade parceira'                                        '08'  '' ''   '',
           'FISKN'               'Nº da conta do registro mestre com endereço fiscal'           '12'  '' ''   '',
           'KRAUS'               'Nº Informação'                                                '15'  '' ''   '',
           'CRTN'                'CódRegTribut.'                                                '15'  '' ''   '',
           'VAL_TEXT'            'Descrição Cod RegT'                                           '15'  '' ''   '',
           'ICMSTAXPAY'          'Contribuinte ICMS'                                            '15'  '' ''   '',
           'JTAXPAYX'            'Descrição Cont ICMS'                                          '15'  '' ''   '',
           'INDTYP'              'Tp.princ.set.ind. '                                           '15'  '' ''   '',
           'INDTYPX'             'Descrição Tp Princ'                                           '15'  '' ''   ''.

    ENDIF.
  ENDIF.
ENDFORM.                    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT                                         *
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo TYPE c
                                p_desc  TYPE c
                                p_tam   TYPE c
                                p_hot   TYPE c
                                p_zero  TYPE c
                                p_sum   TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-outputlen = p_tam.

  IF wl_fcat-fieldname = 'CHG_LOG'.
    wl_fcat-icon = abap_true.
    wl_fcat-hotspot = abap_true.
  ENDIF.

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT                                          *
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = 'ZFIR0060_MAIN'.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT                                    *
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: lit_tcode TYPE TABLE OF tcode.

  SELECT SINGLE *
     FROM tvarvc INTO @DATA(lwa_tvarv)
    WHERE name EQ 'ZFI0069_FIX_XK_XD'
      AND low  EQ @sy-uname.

  IF sy-subrc NE 0.
    APPEND 'FIX_XK_XD' TO lit_tcode.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING lit_tcode.
  SET TITLEBAR  '0100'.
  MESSAGE s000(z_mm).
ENDMODULE.   " STATUS_0100 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT                                        *
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'FIX_XK_XD'.
        SUBMIT zmaster_data_fix_xk_xd VIA SELECTION-SCREEN AND RETURN.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML                                           *
*&---------------------------------------------------------------------*
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
*&      Form  ADD_TEXT                                                 *
*&---------------------------------------------------------------------*
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
*&      Module  CREATE_OBJECTS  OUTPUT                                 *
*&---------------------------------------------------------------------*
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

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

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
    gs_layout-zebra       = 'X'.
    gs_layout-cwidth_opt  = 'X'.
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

    IF s_vend IS INITIAL.


**   Send data to ALV grid
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'B'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida[].

    ELSE.
**   Send data to ALV grid
      CALL METHOD ctl_alv_resumo->set_table_for_first_display
        EXPORTING
          is_layout            = wa_layout
          is_variant           = gs_variant
          i_save               = 'B'
          it_toolbar_excluding = it_exclude_fcode
        CHANGING
          it_fieldcatalog      = it_fcat_v
          it_outtab            = it_saida_dados_venda[].

    ENDIF.
    PERFORM cria_html_cab.

    CALL METHOD ctl_alv_resumo->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = dg_dyndoc_id.

*    CLEAR: GF_FIRST_DISPLAY.

  ENDIF.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB                                            *
*&---------------------------------------------------------------------*
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
        qtd                     TYPE i,
        vl_cont                 TYPE i,
        qdias(10)               TYPE c.

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

  p_text = 'Relatório de clientes e fornecedores'.
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

  IF ck_cli IS NOT INITIAL.
    IF s_ina IS NOT INITIAL.

      IF s_bukrs IS NOT INITIAL.
        SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
          WHERE bukrs IN @s_bukrs.

        LOOP AT s_bukrs.
          IF s_bukrs-option NE 'EQ' AND s_bukrs-option NE 'BT'.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
            EXIT.
          ELSEIF s_bukrs-option EQ 'BT'.

            CONCATENATE 'Empresa:' s_bukrs-low '-' wa_t001-butxt  INTO sdydo_text_element SEPARATED BY space.

            CONCATENATE sdydo_text_element s_bukrs-high INTO sdydo_text_element SEPARATED BY space.
            EXIT.
          ELSE.
            vl_cont = vl_cont + 1.
            IF vl_cont GT 1.
              sdydo_text_element = 'Empresa: Multiplas Seleções'.
            ELSE.
              CONCATENATE 'Empresa:' s_bukrs-low  '-' wa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDLOOP.
        APPEND sdydo_text_element TO p_text_table.
        CLEAR: vl_cont, sdydo_text_element, wa_t001.
      ENDIF.

      IF s_qds IS NOT INITIAL.
        qdias =   s_qds-low.
        CONCATENATE  'Qdt Dias: ' qdias INTO  sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.

    ELSE.

      IF s_kunnr-low IS NOT INITIAL.
        CONCATENATE 'Cliente:' s_kunnr-low INTO sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.
      "Chave do país *********
      IF s_land1 IS NOT INITIAL.
        CONCATENATE 'País: ' s_land1-low INTO  sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.
      "Grupos *********
      IF s_ktokd IS NOT INITIAL.
        CONCATENATE 'Grupo de contas:'  s_ktokd-low INTO   sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.
    ENDIF.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CLEAR: p_text_table, sdydo_text_element.

  ELSE.

    IF s_ina IS NOT INITIAL.
      IF s_bukrs IS NOT INITIAL.
        SELECT SINGLE * FROM t001 INTO wa_t001
          WHERE bukrs IN s_bukrs.

        LOOP AT s_bukrs.
          IF s_bukrs-option NE 'EQ' AND s_bukrs-option NE 'BT'.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
            EXIT.
          ELSEIF s_bukrs-option EQ 'BT'.

            CONCATENATE 'Empresa:' s_bukrs-low '-' wa_t001-butxt  INTO sdydo_text_element SEPARATED BY space.

            CONCATENATE sdydo_text_element s_bukrs-high INTO sdydo_text_element SEPARATED BY space.
            EXIT.
          ELSE.
            vl_cont = vl_cont + 1.
            IF vl_cont GT 1.
              sdydo_text_element = 'Empresa: Multiplas Seleções'.
            ELSE.
              CONCATENATE 'Empresa:' s_bukrs-low  '-' wa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDLOOP.
        APPEND sdydo_text_element TO p_text_table.
        CLEAR: vl_cont, sdydo_text_element, wa_t001.
      ENDIF.

      IF s_qds IS NOT INITIAL.
        qdias = s_qds-low.
        CONCATENATE  'Qdt Dias: ' qdias INTO  sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.

    ELSE.
      IF s_lifnr-low IS NOT INITIAL.
        CONCATENATE 'Fornecedor: ' s_lifnr-low INTO  sdydo_text_element  SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.

      IF s_land_1-low IS NOT INITIAL.
        CONCATENATE 'País: ' s_land_1-low INTO  sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.

      IF s_ktokk IS NOT INITIAL.
        CONCATENATE 'Grupo de contas:'  s_ktokk-low INTO sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.

    ENDIF.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CLEAR: p_text_table, sdydo_text_element.

  ENDIF.


  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM                                            *
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
ENDFORM.                    " F_PEGA_IMAGEM.

FORM get_historic_changes TABLES
                          data_header  TYPE cdhdr_tab
                          data_details TYPE cdpos_tab
                          USING
                          transaction
                          username
                          date_from
                          date_upto
                          account.

  DATA(_date_interval) =
    VALUE rsis_t_range( ( sign   = 'I'
                          option = COND #( WHEN wg_historic_change_header-date_upto IS INITIAL THEN 'EQ' ELSE 'BT' )
                          low    = date_from
                          high   = date_upto
                      ) ).

  IF transaction IS INITIAL.
    SELECT *
      FROM cdhdr
      INTO TABLE data_header
     WHERE objectid EQ account
       AND udate    IN _date_interval.
  ELSE.
    SELECT *
      FROM cdhdr
      INTO TABLE data_header
     WHERE objectid EQ account
       AND tcode    EQ transaction
       AND udate    IN _date_interval.
  ENDIF.

  CHECK data_header[] IS NOT INITIAL.
  SELECT *
    FROM cdpos
    INTO TABLE data_details
 FOR ALL ENTRIES IN data_header
   WHERE changenr = data_header-changenr
     AND objectid = data_header-objectid.
ENDFORM.

FORM set_historic_changes USING transaction date_from date_upto account username.

  DATA changes_header  TYPE TABLE OF cdhdr.
  DATA changes_detail  TYPE TABLE OF cdpos.
  DATA fieldnames      TYPE TABLE OF dfies.
  DATA historic_change TYPE ty_historic_change.
  DATA description     TYPE string.
  DATA operation       TYPE char10.

  PERFORM get_historic_changes
   TABLES
     changes_header
     changes_detail
    USING
     transaction
     username
     date_from
     date_upto
     account.

  IF NOT changes_detail[] IS INITIAL.
    SORT changes_detail BY changenr.

    LOOP AT changes_header INTO DATA(_change_header).
      LOOP AT changes_detail INTO DATA(_change_detail) WHERE changenr = _change_header-changenr.

        CALL FUNCTION 'DDIF_FIELDINFO_GET'
          EXPORTING
            tabname        = _change_detail-tabname
            fieldname      = _change_detail-fname
          TABLES
            dfies_tab      = fieldnames
          EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.

        CASE _change_detail-chngind.
          WHEN 'I'.
            operation = 'Inserir'.
          WHEN 'U'.
            operation = 'Modificar'.
          WHEN 'E' OR 'D'.
            operation = 'Deletar'.
        ENDCASE.

        IF _change_detail-fname = 'KEY'.
          SELECT SINGLE ddtext
            FROM dd02t
            INTO description
           WHERE tabname    = _change_detail-tabname
             AND ddlanguage = sy-langu.

          historic_change-value_new = _change_detail-tabkey.

        ELSE.
          TRY.
              description = fieldnames[ 1 ]-fieldtext.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          historic_change-value_new = _change_detail-value_new.
        ENDIF.

        historic_change-username    = _change_header-username.
        historic_change-value_old   = _change_detail-value_old.
        historic_change-operation   = operation.
        historic_change-fieldname   = description.
        historic_change-transaction = _change_header-tcode.
        historic_change-change_date = _change_header-udate.
        historic_change-change_time = _change_header-utime.

        APPEND historic_change TO tg_historic_changes.
      ENDLOOP.
    ENDLOOP.

  ELSE.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

FORM display_logs.
  DATA fcat   TYPE lvc_t_fcat.
  DATA str    TYPE REF TO data.
  DATA layout TYPE lvc_s_layo.

  CREATE OBJECT container_log
    EXPORTING
      container_name = 'CUSTOM_001'.

  IF alv_log IS INITIAL.
    CREATE OBJECT alv_log
      EXPORTING
        i_parent          = container_log
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    ASSIGN 'TY_HISTORIC_CHANGE' TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA str TYPE (<fs_str>).

    fcat = CORRESPONDING #( cl_salv_data_descr=>read_structdescr(
                             CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      <fs_fcat>-col_opt = abap_true.
    ENDLOOP.

    layout-grid_title = |Histórico modificações: { wg_historic_change_header-account }|.
    variant_log-report = 'ZFIR0060'.

    CALL METHOD alv_log->set_table_for_first_display
      EXPORTING
        is_layout            = layout
        is_variant           = variant_log
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = fcat
        it_outtab            = tg_historic_changes.

  ELSE.
    CALL METHOD alv_log->refresh_table_display.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS '0101'.
  SET TITLEBAR '0101'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      CLEAR: wg_historic_change_header, tg_historic_changes.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_SEARCH'.
      CLEAR tg_historic_changes.

      IF wg_historic_change_header-date_from IS INITIAL.
        MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        PERFORM set_historic_changes
          USING
           wg_historic_change_header-transaction
           wg_historic_change_header-date_from
           wg_historic_change_header-date_upto
           wg_historic_change_header-account
           wg_historic_change_header-username.

        CALL METHOD alv_log->refresh_table_display.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0101 OUTPUT.
  IF ck_log IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'WG_HISTORIC_CHANGE_HEADER-USERNAME'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  PERFORM display_logs.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DADOS_BANCARIOS
*&---------------------------------------------------------------------*
FORM preenche_dados_bancarios  USING VALUE(p_value) TYPE char1.

  DATA: wa_knbk TYPE knbk,
        wa_lfbk TYPE lfbk.

  IF p_value EQ 'K'.

    SORT it_knbk BY kunnr ASCENDING.

    LOOP AT it_knbk INTO wa_knbk.
      LOOP AT it_kna1 INTO wa_kna1 WHERE kunnr EQ wa_knbk-kunnr.

        wa_saida-banks = wa_knbk-banks.
        wa_saida-bankl = wa_knbk-bankl.
        wa_saida-bankn = wa_knbk-bankn.
        wa_saida-bkont = wa_knbk-bkont.
        wa_saida-bvtyp = wa_knbk-bvtyp.
        wa_saida-bkref = wa_knbk-bkref.
        wa_saida-kunnr = wa_kna1-kunnr.
        wa_saida-land1 = wa_kna1-land1.
        wa_saida-name1 = wa_kna1-name1.
        wa_saida-name2 = wa_kna1-name2.
        wa_saida-ort01 = wa_kna1-ort01.
        wa_saida-pstlz = wa_kna1-pstlz.
        wa_saida-regio = wa_kna1-regio.
        wa_saida-sortl = wa_kna1-sortl.
        wa_saida-begru = wa_kna1-begru.
        wa_saida-brsch = wa_kna1-brsch.
        wa_saida-bubkz = wa_kna1-bubkz.
        wa_saida-konzs = wa_kna1-konzs.
        wa_saida-ktokd = wa_kna1-ktokd.
        wa_saida-kukla = wa_kna1-kukla.
        wa_saida-lifnr = wa_kna1-lifnr.
        APPEND wa_saida TO it_saida.

      ENDLOOP.
    ENDLOOP.

    SORT it_saida BY kunnr bvtyp ASCENDING.

  ELSEIF p_value EQ 'L'.

    SORT it_lfbk BY lifnr ASCENDING.

    LOOP AT it_lfbk INTO wa_lfbk.
      LOOP AT it_lfa1 INTO wa_lfa1 WHERE lifnr EQ wa_lfbk-lifnr.

        wa_saida-banks = wa_lfbk-banks.
        wa_saida-bankl = wa_lfbk-bankl.
        wa_saida-bankn = wa_lfbk-bankn.
        wa_saida-bkont = wa_lfbk-bkont.
        wa_saida-bvtyp = wa_lfbk-bvtyp.
        wa_saida-bkref = wa_lfbk-bkref.
        wa_saida-lifnr = wa_lfa1-lifnr.
        wa_saida-land1 = wa_lfa1-land1.
        wa_saida-name1 = wa_lfa1-name1.
        wa_saida-name2 = wa_lfa1-name2.
        wa_saida-ort01 = wa_lfa1-ort01.
        wa_saida-pstlz = wa_lfa1-pstlz.
        wa_saida-regio = wa_lfa1-regio.
        wa_saida-sortl = wa_lfa1-sortl.
        wa_saida-begru = wa_lfa1-begru.
        wa_saida-brsch = wa_lfa1-brsch.
        wa_saida-bubkz = wa_lfa1-bubkz.
        wa_saida-konzs = wa_lfa1-konzs.
        wa_saida-ktokk = wa_lfa1-ktokk.
        wa_saida-kunnr = wa_lfa1-kunnr.
        APPEND wa_saida TO it_saida.

      ENDLOOP.
    ENDLOOP.

    SORT it_saida BY lifnr bvtyp ASCENDING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_BANCARIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1186   text
*----------------------------------------------------------------------*
FORM busca_dados_bancarios USING VALUE(p_value) TYPE char1
                                 VALUE(p_dupli) TYPE char1.

  IF p_dupli EQ 'X'.
    IF p_value EQ 'K'.

      SELECT *
        FROM kna1
        INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_dup
        WHERE stcd1 EQ it_dup-stcd1
          AND stcd2 EQ it_dup-stcd2
          AND stcd3 EQ it_dup-stcd3
          AND EXISTS ( SELECT *
                         FROM knbk
                        WHERE kunnr EQ kna1~kunnr ).

      IF it_kna1 IS NOT INITIAL.

        SELECT *
          FROM knbk
          INTO TABLE it_knbk
          FOR ALL ENTRIES IN it_kna1
          WHERE kunnr EQ it_kna1-kunnr.

      ENDIF.

    ELSE.

      SELECT *
        FROM lfa1
        INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_dup
        WHERE stcd1 EQ it_dup-stcd1
          AND stcd2 EQ it_dup-stcd2
          AND stcd3 EQ it_dup-stcd3
          AND EXISTS ( SELECT *
                         FROM lfbk
                        WHERE lifnr EQ lfa1~lifnr ).

      IF it_lfa1 IS NOT INITIAL.

        SELECT *
          FROM lfbk
          INTO TABLE it_lfbk
          FOR ALL ENTRIES IN it_lfa1
          WHERE lifnr EQ it_lfa1-lifnr.

      ENDIF.

    ENDIF.
  ELSE.

    SELECT *
      FROM kna1
      INTO TABLE it_kna1
      WHERE kunnr IN s_kunnr
        AND land1 IN s_land1
        AND ktokd IN s_ktokd
        AND stkzn IN pessoa
        AND EXISTS ( SELECT *
                       FROM knbk
                      WHERE kunnr EQ kna1~kunnr ).

    SELECT *
      FROM lfa1
      INTO TABLE it_lfa1
     WHERE lifnr IN s_lifnr
       AND land1 IN s_land_1
       AND ktokk IN s_ktokk
       AND stkzn IN pessoa
       AND EXISTS ( SELECT *
                      FROM lfbk
                     WHERE lifnr EQ lfa1~lifnr ).

    IF it_kna1 IS NOT INITIAL.

      SELECT *
        FROM knbk
        INTO TABLE it_knbk
        FOR ALL ENTRIES IN it_kna1
        WHERE kunnr EQ it_kna1-kunnr.

    ENDIF.

    IF it_lfa1 IS NOT INITIAL.

      SELECT *
        FROM lfbk
        INTO TABLE it_lfbk
        FOR ALL ENTRIES IN it_lfa1
        WHERE lifnr EQ it_lfa1-lifnr.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS_MEST_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_dados_mest_venda .
*  CHECK s_kunnr IS NOT INITIAL.

*  IF s_kunnr IS NOT INITIAL OR p_bukrs IS NOT INITIAL.
*    FREE: it_kna1.
*    SELECT * FROM kna1 AS a INTO TABLE it_kna1
*    WHERE kunnr IN s_kunnr
*      AND ( EXISTS ( SELECT * FROM knb1 AS b WHERE kunnr EQ a~kunnr AND bukrs IN p_bukrs ) ).
*  ENDIF.

*  IF it_kna1 IS NOT INITIAL.
*    SELECT * FROM knvv
*      INTO TABLE it_knvv
*        FOR ALL ENTRIES IN it_kna1
*      WHERE kunnr EQ it_kna1-kunnr
*        AND vkorg IN it_kna1-kunnrs_vkorg
*        AND vtweg IN s_vtweg
*        AND spart IN s_spart.
*
*
*    SELECT * FROM knb1
*      INTO TABLE it_knb1
*       FOR ALL ENTRIES IN it_kna1
*     WHERE kunnr EQ it_kna1-kunnr.


*    SELECT * FROM skat
*      INTO TABLE it_skat
*       FOR ALL ENTRIES IN it_knb1
*     WHERE saknr EQ it_knb1-akont
*       AND spras EQ sy-langu.

*ELSE.
  IF p_bukrs IS NOT INITIAL.
    SELECT * FROM knvv
      INTO TABLE it_knvv
      WHERE kunnr IN s_kunnr
        AND vkorg IN p_bukrs
        AND vtweg IN s_vtweg
        AND spart IN s_spart.
  ELSE.
    SELECT * FROM knvv
    INTO TABLE it_knvv
    WHERE kunnr IN s_kunnr
      AND vkorg IN s_vkorg
      AND vtweg IN s_vtweg
      AND spart IN s_spart.
  ENDIF.

*  ENDIF.

  IF it_knvv IS NOT INITIAL.
    SELECT * FROM tvkot INTO TABLE it_tvkot FOR ALL ENTRIES IN it_knvv WHERE vkorg EQ it_knvv-vkorg AND spras EQ sy-langu.
    SELECT * FROM tvtwt INTO TABLE it_tvtwt FOR ALL ENTRIES IN it_knvv WHERE vtweg EQ it_knvv-vtweg AND spras EQ sy-langu.
    SELECT * FROM tspat INTO TABLE it_tspat FOR ALL ENTRIES IN it_knvv WHERE spart EQ it_knvv-spart AND spras EQ sy-langu.
    SELECT * FROM tvktt INTO TABLE it_tvktt FOR ALL ENTRIES IN it_knvv WHERE ktgrd EQ it_knvv-ktgrd AND spras EQ sy-langu.
    SELECT * FROM tvktt INTO TABLE it_tvktt FOR ALL ENTRIES IN it_knvv WHERE ktgrd EQ it_knvv-ktgrd AND spras EQ sy-langu.
    SELECT * FROM knb1  INTO TABLE it_knb1  FOR ALL ENTRIES IN it_knvv WHERE kunnr EQ it_knvv-kunnr AND bukrs EQ  it_knvv-vkorg.
    SELECT * FROM skat  INTO TABLE it_skat  FOR ALL ENTRIES IN it_knb1 WHERE saknr EQ it_knb1-akont AND spras EQ sy-langu.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS_MEST_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organizar_dados_mest_venda .

  CHECK it_knvv IS NOT INITIAL.

  SORT: it_knvv BY vkorg kunnr,
       it_tvkot BY vkorg,
       it_tvtwt BY vtweg,
       it_tspat BY spart,
       it_tvktt BY ktgrd,
       it_knb1  BY bukrs kunnr,
       it_skat  BY saknr.

  LOOP AT it_knvv INTO DATA(w_knvv).
    READ TABLE it_tvkot INTO DATA(ws_tvkot) WITH KEY vkorg = w_knvv-vkorg BINARY SEARCH.
    READ TABLE it_tvtwt INTO DATA(ws_tvtwt) WITH KEY vtweg = w_knvv-vtweg BINARY SEARCH.
    READ TABLE it_tspat INTO DATA(ws_tspat) WITH KEY spart = w_knvv-spart BINARY SEARCH.
    READ TABLE it_tvktt INTO DATA(ws_tvktt) WITH KEY ktgrd = w_knvv-ktgrd BINARY SEARCH.
    READ TABLE it_knb1 INTO DATA(ws_knb1)   WITH KEY bukrs = w_knvv-vkorg kunnr = w_knvv-kunnr BINARY SEARCH.
    READ TABLE it_skat INTO DATA(ws_skat)   WITH KEY saknr = ws_knb1-akont BINARY SEARCH .



    APPEND VALUE #(
                   kunnr  = w_knvv-kunnr
                   vkorg  = w_knvv-vkorg
                   vtweg  = w_knvv-vtweg
                   spart  = w_knvv-spart
                   ktgrd  = w_knvv-ktgrd
                   vtext  = ws_tvkot-vtext
                   vtextd = ws_tvtwt-vtext
                   vtexta = ws_tspat-vtext
                   vtextg = ws_tvktt-vtext
                   txt50  = ws_skat-txt50
                   akont  = ws_knb1-akont
                   ) TO it_saida_dados_venda.


    CLEAR: w_knvv, ws_tvkot, ws_tvtwt, ws_tspat, ws_tvktt, ws_knb1, ws_skat.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_MEST_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_mest_venda .
  FREE: it_fcat_v.
  PERFORM alv_preenche_cat_v USING:
      'kunnr '               'Nº Cliente            '        '15'  '' ''   '',
      'vkorg '               'Organização de vendas '        '10'  '' ''   '',
      'vtext '               'Desc.orga de vendas   '        '20'  '' ''   '',
      'SPART '               'Setor atividade       '        '10'  '' ''   '',
      'vtextd'               'Desc.atividade        '        '20'  '' ''   '',
      'vtweg '               'Canal de distribuição '        '10'  '' ''   '',
      'vtexta'               'Desc.canal Distrib    '        '20'  '' ''   '',
      'ktgrd '               'Grp class contabil    '        '05'  '' ''   '',
      'vtextg'               'Desc.grp clas.contabil'        '30'  '' ''   '',
      'AKONT'                'Nº conta do Razão     '        '15'  '' ''   '',
      'TXT50'                'Desc contas do Razão  '        '30'  '' ''   ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_V
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8274   text
*      -->P_8275   text
*      -->P_8276   text
*      -->P_8277   text
*      -->P_8278   text
*      -->P_8279   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat_v   USING   p_campo TYPE c
                                p_desc  TYPE c
                                p_tam   TYPE c
                                p_hot   TYPE c
                                p_zero  TYPE c
                                p_sum   TYPE c.
  DATA: wl_fcat_v TYPE lvc_s_fcat.

  wl_fcat_v-fieldname = p_campo.
  wl_fcat_v-scrtext_l = p_desc.
  wl_fcat_v-scrtext_m = p_desc.
  wl_fcat_v-scrtext_s = p_desc.
  wl_fcat_v-hotspot   = p_hot.
  wl_fcat_v-no_zero   = p_zero.
  wl_fcat_v-do_sum    = p_sum.
  wl_fcat_v-outputlen = p_tam.

*  IF wl_fcat_v-fieldname = 'CHG_LOG'.
*    wl_fcat_v-icon = abap_true.
*    wl_fcat_v-hotspot = abap_true.
*  ENDIF.

  APPEND wl_fcat_v TO it_fcat_v.

ENDFORM.
