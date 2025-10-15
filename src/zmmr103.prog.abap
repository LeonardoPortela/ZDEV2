*&---------------------------------------------------------------------*
*& *Report  ZMMR103
*& *Módulo  MM - Relatório de média de consumo
*& *Report Orientado a Objetos
*&---------------------------------------------------------------------*
*& *Data: 09.06.2015
*& *Enio Jesus
*&---------------------------------------------------------------------*

REPORT  zmmr103.
TABLES: mkpf, mard, marc.
TYPE-POOLS: slis.
TYPES:  kkblo_selfield TYPE slis_selfield.

TYPES: BEGIN OF ty_ped,
         matnr TYPE mard-matnr,       "Material
         werks TYPE mard-werks,       "Centro
         lgort TYPE mard-lgort,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         menge TYPE ekpo-menge,
       END OF ty_ped,

       BEGIN OF ty_res,
         matnr TYPE mard-matnr,       "Material
         werks TYPE mard-werks,       "Centro
         lgort TYPE mard-lgort,
         rsnum TYPE resb-rsnum,
         bdmng TYPE resb-bdmng,
       END OF ty_res,

       BEGIN OF ty_req,
         matnr TYPE mard-matnr,       "Material
         werks TYPE mard-werks,       "Centro
         lgort TYPE mard-lgort,
         banfn TYPE eban-banfn,
         bnfpo TYPE eban-bnfpo,
         menge TYPE eban-menge,
       END OF ty_req.
*----------------------------------------------------------------------*
*       INTERFACE ZIF_DATA_READER
*----------------------------------------------------------------------*
INTERFACE: zif_data_reader.
  TYPES: BEGIN OF ty_saida,
           matnr     TYPE mard-matnr,       "Material
           werks     TYPE mard-werks,       "Centro
           lgort     TYPE mard-lgort,       "Depósito
           maktx     TYPE makt-maktx,       "Desc. Material
           meins     TYPE mara-meins,       " Unidade de medida
           labst     TYPE mard-labst,       "Saldo Estoque
           mtart     TYPE mara-mtart,
           cperi(10) TYPE p DECIMALS 2, "Consumo Período
           cmedi(10) TYPE p DECIMALS 2, "Consumo Médio
           sesto(10) TYPE p DECIMALS 2, "Saldo Estoque
           vesto(10) TYPE p DECIMALS 2, "Valor Estoque
           tipo(1)   TYPE c, " Tipo movimento C ou V
           qponto    TYPE mseg-menge, " Ponto ressuprimento
           qlote     TYPE mseg-menge, " Lote minimo
           qtdev     TYPE mseg-menge, " Qtde periodo por tipo
           qtdec     TYPE mseg-menge, " Qtde periodo por tipo
           qtdet     TYPE mseg-menge, " Qtde periodo por tipo
           qtdetp    TYPE mseg-menge, " Qtde total periodo
           qtdem     TYPE mseg-menge, " media mes
           qtded     TYPE mseg-menge, " Media dia
           qrcpen    TYPE mseg-menge, " requisições de estoque pendente
           qpdpen    TYPE mseg-menge, " pedidos de estoque pendente
           qtrese    TYPE mseg-menge, " reservas aprovadas
           tcobe(10) TYPE p DECIMALS 2, " Tempo cobertura do estoque em dias
           trepo(10) TYPE p DECIMALS 2, " Tempo reposição em dias
           estse(10) TYPE p DECIMALS 2, " estoque segurança em dias
           tnego(10) TYPE p DECIMALS 2, " tempo negociação
           qtder(10) TYPE p DECIMALS 2, " quantidade a ser requisitada
         END OF ty_saida,

         BEGIN OF ty_mseg,
           werks      TYPE mseg-werks,
           budat_mkpf TYPE mseg-budat_mkpf,
           matnr      TYPE mseg-matnr,
           lgort      TYPE mseg-matnr,
           bwart      TYPE mseg-bwart,
         END OF ty_mseg.

  METHODS: read_data, generate_grid.
ENDINTERFACE.                    "ZIF_DATA_READER

DATA: it_saida      TYPE TABLE OF zif_data_reader=>ty_saida,
      wa_saida      TYPE zif_data_reader=>ty_saida,
      it_pedido     TYPE TABLE OF ty_ped,
      wa_pedido     TYPE ty_ped,
      it_res        TYPE TABLE OF ty_res,
      wa_res        TYPE ty_res,
      it_req        TYPE TABLE OF ty_req,
      wa_req        TYPE ty_req,
      it_pedido_aux TYPE TABLE OF ty_ped,
      wa_pedido_aux TYPE ty_ped,
      it_res_aux    TYPE TABLE OF ty_res,
      wa_res_aux    TYPE ty_res,
      it_req_aux    TYPE TABLE OF ty_req,
      wa_req_aux    TYPE ty_req.


DATA: wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.


DATA: g_custom_container3 TYPE REF TO cl_gui_custom_container,
      dg_splitter_1       TYPE REF TO cl_gui_splitter_container,
      dg_parent_1         TYPE REF TO cl_gui_container,
      dg_splitter_2       TYPE REF TO cl_gui_splitter_container,
      dg_parent_2         TYPE REF TO cl_gui_container,
      dg_parent_2a        TYPE REF TO cl_gui_container,
      dg_parent_alv       TYPE REF TO cl_gui_container,
      gs_layout           TYPE lvc_s_layo,
      gs_variant          TYPE disvariant,
      it_exclude_fcode    TYPE ui_functions,
      wa_exclude_fcode    LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id        TYPE REF TO cl_dd_document,
      ctl_alv3            TYPE REF TO cl_gui_alv_grid.

DATA: g_custom_container2 TYPE REF TO cl_gui_custom_container,
*      DG_SPLITTER_1       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
*      DG_PARENT_1         TYPE REF TO CL_GUI_CONTAINER,
*      DG_SPLITTER_2       TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
*      DG_PARENT_2         TYPE REF TO CL_GUI_CONTAINER,
*      DG_PARENT_2A        TYPE REF TO CL_GUI_CONTAINER,
*      DG_PARENT_ALV       TYPE REF TO CL_GUI_CONTAINER,
*      GS_LAYOUT           TYPE LVC_S_LAYO,
*      GS_VARIANT          TYPE DISVARIANT,
*      IT_EXCLUDE_FCODE    TYPE UI_FUNCTIONS,
*      WA_EXCLUDE_FCODE    LIKE LINE OF IT_EXCLUDE_FCODE,
*      DG_DYNDOC_ID        TYPE REF TO CL_DD_DOCUMENT,
      ctl_alv2            TYPE REF TO cl_gui_alv_grid.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
*      DG_SPLITTER_1      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
*      DG_PARENT_1        TYPE REF TO CL_GUI_CONTAINER,
*      DG_SPLITTER_2      TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
*      DG_PARENT_2        TYPE REF TO CL_GUI_CONTAINER,
*      DG_PARENT_2A       TYPE REF TO CL_GUI_CONTAINER,
*      DG_PARENT_ALV      TYPE REF TO CL_GUI_CONTAINER,
*      GS_LAYOUT          TYPE LVC_S_LAYO,
*      GS_VARIANT         TYPE DISVARIANT,
*      IT_EXCLUDE_FCODE   TYPE UI_FUNCTIONS,
*      WA_EXCLUDE_FCODE   LIKE LINE OF IT_EXCLUDE_FCODE,
*      DG_DYNDOC_ID       TYPE REF TO CL_DD_DOCUMENT,
      ctl_alv            TYPE REF TO cl_gui_alv_grid.


DATA: field2(20) TYPE c.


*DATA: IT_LISTHEADER TYPE SLIS_T_LISTHEADER,
*      WA_LISTHEADER TYPE SLIS_LISTHEADER.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN COMMENT /1(50) text001.
  SELECTION-SCREEN COMMENT /1(79) text002.
  SELECTION-SCREEN SKIP 1.
  SELECT-OPTIONS:
                  s_peri FOR mkpf-budat OBLIGATORY,
                  s_cent FOR mard-werks OBLIGATORY,
                  s_matn FOR mard-matnr,
                  s_lgor FOR mard-lgort.
SELECTION-SCREEN: END OF BLOCK b1.

PARAMETERS: r_agru   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' '.

INITIALIZATION.
  DATA: output_date TYPE sy-datum.

  text001 = 'Informações de consumo:'.
  text002 = 'Só serão exibidas informações com histórico de consumo no período de análise.'.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = '00'
      months    = '00'
      signum    = '-'
      years     = '01'
    IMPORTING
      calc_date = output_date.

  s_peri-low  = output_date.
  s_peri-high = sy-datum.
  APPEND s_peri.
*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_seleciona_dados DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_data_reader.

  PRIVATE SECTION.

*VARIÁVEIS
    DATA: n_months TYPE i,
          v_text   TYPE char45.

*TABELAS INTERNAS*
    DATA: it_mard   TYPE TABLE OF mard,
          it_mard_c TYPE TABLE OF mard,
          it_marc   TYPE TABLE OF marc,
          it_mara   TYPE TABLE OF mara,
          it_mseg   TYPE TABLE OF mseg,
          it_makt   TYPE TABLE OF makt,
          it_t156   TYPE TABLE OF t156,
          it_eban   TYPE TABLE OF eban,
          it_ekpo   TYPE TABLE OF ekpo,
          it_ekbe   TYPE TABLE OF ekbe,
          it_resb   TYPE TABLE OF resb,
          it_fcat   TYPE TABLE OF slis_fieldcat_alv,
          it_evento TYPE          slis_t_event,
          it_mbew   TYPE TABLE OF mbew.

*TABELAS WORK-ÁREAS*
    DATA: wa_mard   TYPE mard,
          wa_mard_c TYPE mard,
          wa_mseg   TYPE mseg,
          wa_makt   TYPE makt,
          wa_mara   TYPE mara,
          wa_marc   TYPE marc,
          wa_t156   TYPE t156,
          wa_eban   TYPE eban,
          wa_ekpo   TYPE ekpo,
          wa_ekbe   TYPE ekbe,
          wa_resb   TYPE resb,
          wa_mbew   TYPE mbew,
          wa_fcat   TYPE slis_fieldcat_alv,
          wa_evento TYPE slis_alv_event,
          wa_layout TYPE slis_layout_alv,
          wa_saida  TYPE zif_data_reader=>ty_saida.
ENDCLASS.                    "LCL_SELECIONA_DADOS DEFINITION

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_compras DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_monta_fieldcat DEFINITION.

  PUBLIC SECTION.
    METHODS: set_fieldcat IMPORTING i_campo  TYPE char30
                                    i_desc_l TYPE char40
                                    i_tam    TYPE num6.

    METHODS: get_fieldcat EXPORTING e_campo  TYPE char30
                                    e_desc_l TYPE char40
                                    e_tam    TYPE num6.
    DATA: at_campo  TYPE char30,
          at_desc_l TYPE char40,
          at_tam    TYPE num6.

ENDCLASS.                    "LCL_MONTA_FIELDCAT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_seleciona_dados IMPLEMENTATION.
  METHOD zif_data_reader~read_data.

    DATA: vqtde_migo TYPE mseg-menge,
          vqtde_miun TYPE mseg-menge,
          vqtde_lote TYPE mseg-menge,
          vqtde_aux  TYPE mseg-menge,
          vqtde_au2  TYPE p DECIMALS 0,
          tabix      TYPE sy-tabix.

    SELECT m1~werks
           m1~budat_mkpf
           m1~matnr
           m1~lgort
           m1~bwart
           m1~menge
           m1~shkzg
      INTO CORRESPONDING FIELDS OF TABLE it_mseg
      FROM mseg AS m1

     INNER JOIN t156 AS b ON m1~bwart = b~bwart
     INNER JOIN mard AS c ON m1~werks = c~werks
                         AND m1~matnr = c~matnr
                         AND m1~lgort = c~lgort
*                         AND C~LVORM <> 'X' "IR075583
     INNER JOIN marc AS d ON m1~werks = d~werks
                         AND m1~matnr = d~matnr
*                         AND D~LVORM <> 'X' "IR075583
*                         AND D~WEBAZ GT 0
    WHERE m1~werks      IN s_cent
      AND m1~budat_mkpf IN s_peri
      AND m1~matnr      IN s_matn
      AND m1~lgort      IN s_lgor
      AND b~bwart       IN ('261','201','702','Z91','601','202','262','Z92','701').

    SELECT *
      FROM resb AS a
     INNER JOIN mard AS m ON a~matnr = m~matnr
                         AND a~werks = m~werks
                         AND a~lgort = m~lgort
*                         AND M~LVORM NE 'X'
     INNER JOIN marc      ON  marc~werks = m~werks
                        AND marc~matnr = m~matnr
*                        AND MARC~LVORM NE 'X'
*                        AND MARC~WEBAZ GT 0
     INTO CORRESPONDING FIELDS OF TABLE it_mard
    WHERE a~matnr IN s_matn
      AND a~werks IN s_cent
      AND a~lgort IN s_lgor
      AND a~xwaok EQ 'X'
      AND a~kzear EQ ''
      AND a~xloek EQ ''.

    "INCLUIR MATERIAIS COM SALDO
    SELECT *
      FROM mard
      INNER JOIN marc
      ON  marc~werks = mard~werks
      AND marc~matnr = mard~matnr
*      AND MARC~LVORM NE 'X'
*      AND MARC~WEBAZ GT 0
     APPENDING CORRESPONDING FIELDS OF TABLE it_mard
    WHERE mard~matnr IN s_matn
      AND mard~werks IN s_cent
      AND mard~lgort IN s_lgor
      AND mard~labst GT 0.
*      AND MARD~LVORM NE 'X'.

    IF ( it_mseg[] IS NOT INITIAL ) OR ( it_mard[] IS NOT INITIAL ).

      IF it_mseg[] IS NOT INITIAL.
        SELECT *
          FROM mard
          INNER JOIN marc
              ON  marc~werks = mard~werks
              AND marc~matnr = mard~matnr
*              AND MARC~LVORM NE 'X'
*              AND MARC~WEBAZ GT 0
          APPENDING CORRESPONDING FIELDS OF TABLE it_mard
           FOR ALL ENTRIES IN it_mseg
         WHERE mard~matnr = it_mseg-matnr
           AND mard~werks = it_mseg-werks
           AND mard~lgort = it_mseg-lgort.
*           AND MARD~LVORM NE 'X'.
      ENDIF.


      SORT it_mard BY matnr werks lgort.
      DELETE ADJACENT DUPLICATES FROM it_mard COMPARING matnr werks lgort.

      CHECK it_mard[] IS NOT INITIAL.

      SELECT *
          FROM mbew
          INTO CORRESPONDING FIELDS OF TABLE it_mbew
           FOR ALL ENTRIES IN it_mard
         WHERE matnr = it_mard-matnr
           AND bwkey = it_mard-werks.

      "Reservas abertas
      SELECT *
       FROM resb
       INTO CORRESPONDING FIELDS OF TABLE it_resb
        FOR ALL ENTRIES IN it_mard
      WHERE matnr = it_mard-matnr
        AND werks = it_mard-werks
        AND lgort = it_mard-lgort
        AND xwaok EQ 'X'
        AND kzear EQ ''
        AND xloek EQ ''.

      "Requisições de Compras de estoque
      SELECT *
       FROM eban
       INTO CORRESPONDING FIELDS OF TABLE it_eban
        FOR ALL ENTRIES IN it_mard
      WHERE matnr = it_mard-matnr
        AND werks = it_mard-werks
        AND lgort = it_mard-lgort
        AND loekz EQ ''
        AND knttp EQ ''. "apenas estoque

      "pedidos de compras estoque
      IF it_eban[] IS NOT INITIAL.
        SELECT *
        FROM ekpo
        INTO CORRESPONDING FIELDS OF TABLE it_ekpo
         FOR ALL ENTRIES IN it_eban
       WHERE banfn = it_eban-banfn
         AND bnfpo = it_eban-bnfpo
         AND elikz EQ ''
         AND loekz EQ ''.
      ENDIF.

      "migo
      IF it_ekpo[] IS NOT INITIAL.
        SELECT *
          FROM ekbe
          INTO CORRESPONDING FIELDS OF TABLE it_ekbe
          FOR ALL ENTRIES IN it_ekpo
          WHERE ebeln = it_ekpo-ebeln
          AND   ebelp = it_ekpo-ebelp
          AND   bewtp = 'E'. "migo apenas
      ENDIF.

      SELECT *
        FROM marc
        INTO CORRESPONDING FIELDS OF TABLE it_marc
         FOR ALL ENTRIES IN it_mard
       WHERE matnr = it_mard-matnr
         AND werks = it_mard-werks.
*         AND LVORM NE 'X'.

      SELECT *
        FROM mara
        INTO CORRESPONDING FIELDS OF TABLE it_mara
         FOR ALL ENTRIES IN it_mard
       WHERE matnr = it_mard-matnr.

      SELECT  *
        FROM makt
        INTO CORRESPONDING FIELDS OF TABLE it_makt
         FOR ALL ENTRIES IN it_mard
       WHERE matnr = it_mard-matnr
         AND spras = sy-langu.

      CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES'
        EXPORTING
          i_datum_bis = s_peri-high
          i_datum_von = s_peri-low
        IMPORTING
          e_monate    = n_months.


      it_mard_c[] = it_mard[].
      SORT it_mard BY matnr werks.
      DELETE ADJACENT DUPLICATES FROM it_mard_c COMPARING matnr werks.
      LOOP AT it_mard_c INTO wa_mard_c.
        CLEAR wa_mard_c-labst.
        tabix = sy-tabix.
        LOOP AT it_mard INTO wa_mard WHERE matnr = wa_mard_c-matnr
                                     AND   werks = wa_mard_c-werks.
          ADD wa_mard-labst TO wa_mard_c-labst.
        ENDLOOP.
        MODIFY it_mard_c FROM wa_mard_c INDEX tabix TRANSPORTING labst.
        "
        IF r_agru = 'X'. "Agrupa deposito
          wa_mard_c-lgort = 'AGRU'.
          MODIFY it_mard_c FROM wa_mard_c INDEX tabix TRANSPORTING lgort.
        ENDIF.
      ENDLOOP.
      IF r_agru = 'X'. "Agrupa deposito
        "
        "Reservas abertas sem deposito
        SELECT *
         FROM resb
         INTO CORRESPONDING FIELDS OF TABLE it_resb
          FOR ALL ENTRIES IN it_mard
        WHERE matnr = it_mard-matnr
          AND werks = it_mard-werks
          AND lgort =  ''
          AND xwaok EQ 'X'
          AND kzear EQ ''
          AND xloek EQ ''.

        LOOP AT it_mseg INTO wa_mseg.
          wa_mseg-lgort = 'AGRU'.
          MODIFY it_mseg FROM wa_mseg INDEX sy-tabix TRANSPORTING lgort.
        ENDLOOP.
        "
        LOOP AT it_resb INTO wa_resb.
          wa_resb-lgort = 'AGRU'.
          MODIFY it_resb FROM wa_resb INDEX sy-tabix TRANSPORTING lgort.
        ENDLOOP.
        "
        LOOP AT it_eban INTO wa_eban.
          wa_eban-lgort = 'AGRU'.
          MODIFY it_eban FROM wa_eban INDEX sy-tabix TRANSPORTING lgort.
        ENDLOOP.

        LOOP AT it_ekpo INTO wa_ekpo.
          wa_ekpo-lgort = 'AGRU'.
          MODIFY it_ekpo FROM wa_ekpo INDEX sy-tabix TRANSPORTING lgort.
        ENDLOOP.

        it_mard[] = it_mard_c[].

      ENDIF.

      SORT: it_makt BY matnr,
            it_mara BY matnr,
            it_marc BY matnr werks,
            it_resb BY matnr werks lgort,
            it_eban BY matnr werks lgort,
            it_ekpo BY matnr werks lgort,
            it_ekbe BY ebeln ebelp ,
            it_mseg BY matnr werks lgort,
            it_mard BY matnr werks lgort,
            it_mard_c BY matnr werks lgort.

      REFRESH: it_res, it_req, it_pedido.

      LOOP AT it_mard INTO wa_mard.
        CLEAR: wa_mseg, wa_saida.

        READ TABLE: it_makt INTO wa_makt WITH KEY matnr = wa_mard-matnr BINARY SEARCH.

        READ TABLE: it_mseg INTO wa_mseg WITH KEY matnr = wa_mard-matnr
                                                  werks = wa_mard-werks BINARY SEARCH.

        CLEAR: wa_saida-vesto, wa_saida-cperi.
        LOOP AT it_mseg INTO wa_mseg WHERE matnr = wa_mard-matnr
                                     AND   werks = wa_mard-werks
                                     AND   lgort = wa_mard-lgort.
          IF wa_mseg-bwart = '601'.
            ADD wa_mseg-menge TO wa_saida-qtdev.
          ELSE.
            IF wa_mseg-shkzg = 'H'.
              ADD wa_mseg-menge TO wa_saida-qtdec.
            ELSE.
              SUBTRACT wa_mseg-menge FROM wa_saida-qtdec.
            ENDIF.
          ENDIF.
          IF wa_mseg-shkzg = 'H'.
            wa_saida-cperi = wa_saida-cperi + wa_mseg-menge. "Consumo Período
          ELSE.
            wa_saida-cperi = wa_saida-cperi - wa_mseg-menge. "Consumo Período
          ENDIF.
        ENDLOOP.

        "resevas abertas
        LOOP AT it_resb INTO wa_resb WHERE matnr = wa_mard-matnr
                                     AND   werks = wa_mard-werks
                                     AND   lgort = wa_mard-lgort.

          IF '202_262_Z92' CS wa_resb-bwart.
            wa_saida-qtrese = wa_saida-qtrese - ( wa_resb-bdmng -  wa_resb-enmng ).
          ELSE.
            wa_saida-qtrese = wa_saida-qtrese + ( wa_resb-bdmng -  wa_resb-enmng ).
          ENDIF.


          wa_res-matnr = wa_mard-matnr.
          wa_res-werks = wa_mard-werks.
          wa_res-lgort = wa_mard-lgort.
          wa_res-rsnum = wa_resb-rsnum.
          wa_res-bdmng = wa_resb-bdmng.
          COLLECT wa_res INTO it_res.
        ENDLOOP.

        "requisições pendentes QRCPEN
        LOOP AT it_eban INTO wa_eban WHERE matnr = wa_mard-matnr
                                     AND   werks = wa_mard-werks
                                     AND   lgort = wa_mard-lgort.
          wa_saida-qrcpen = wa_saida-qrcpen + ( wa_eban-menge - wa_eban-bsmng ).
          wa_req-matnr = wa_mard-matnr.
          wa_req-werks = wa_mard-werks.
          wa_req-lgort = wa_mard-lgort.
          wa_req-banfn = wa_eban-banfn.
          wa_req-bnfpo = wa_eban-bnfpo.
          wa_req-menge = ( wa_eban-menge - wa_eban-bsmng ).
          COLLECT wa_req INTO it_req.
        ENDLOOP.

        "pedidos pendentes
        vqtde_migo = 0.
        LOOP AT it_ekpo INTO wa_ekpo WHERE matnr = wa_mard-matnr
                                       AND werks = wa_mard-werks
                                       AND lgort = wa_mard-lgort.
          wa_saida-qpdpen = wa_saida-qpdpen + wa_ekpo-menge.
          wa_pedido-matnr = wa_mard-matnr.
          wa_pedido-werks = wa_mard-werks.
          wa_pedido-lgort = wa_mard-lgort.
          wa_pedido-ebeln = wa_ekpo-ebeln.
          wa_pedido-ebelp = wa_ekpo-ebelp.
          wa_pedido-menge = wa_ekpo-menge.
          COLLECT wa_pedido INTO it_pedido.

          LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln
                                         AND ebelp = wa_ekpo-ebelp.
            vqtde_miun = 0.
            IF wa_ekbe-shkzg = 'S'.
              ADD wa_ekbe-menge TO vqtde_migo.
              SUBTRACT wa_ekbe-menge FROM vqtde_miun.
            ELSE.
              SUBTRACT wa_ekbe-menge FROM vqtde_migo.
              ADD wa_ekbe-menge TO vqtde_miun.
            ENDIF.
            wa_pedido-menge =  vqtde_miun.
            COLLECT wa_pedido INTO it_pedido.
          ENDLOOP.
        ENDLOOP.

        wa_saida-qpdpen = wa_saida-qpdpen - vqtde_migo.

        LOOP AT it_mbew INTO wa_mbew WHERE matnr = wa_mard-matnr
                                       AND bwkey = wa_mard-werks.
          wa_saida-vesto = wa_mbew-salk3.
          IF r_agru NE 'X'. "Agrupa deposito.
            READ TABLE it_mard_c INTO wa_mard_c WITH KEY matnr = wa_mard-matnr
                                                         werks = wa_mard-werks BINARY SEARCH.
            IF wa_mard_c-labst GT 0.
              wa_saida-vesto = wa_mbew-salk3 * ( wa_mard-labst / wa_mard_c-labst ) .
            ENDIF.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_mard-matnr
          IMPORTING
            output = wa_saida-matnr.   "Material

        READ TABLE: it_mara INTO wa_mara WITH KEY matnr = wa_mard-matnr BINARY SEARCH.

*        IF ( WA_MARA-MEINS EQ 'ST' ).
*          WA_SAIDA-MEINS = 'PEÇ'.
*          WA_SAIDA-MTART = WA_MARA-MTART.
*        ELSE.
        wa_saida-meins = wa_mara-meins.
        wa_saida-mtart = wa_mara-mtart.
*        ENDIF.



*       WA_SAIDA-MEINS = WA_MARA-MEINS.

        wa_saida-werks = wa_mard-werks.      "Centro
        wa_saida-lgort = wa_mard-lgort.      "Depósito
        wa_saida-labst = wa_mard-labst.      "Saldo Estoque
        wa_saida-maktx = wa_makt-maktx.      "Desc. Material
        IF n_months = 0.
          n_months = 1.
        ENDIF.
        wa_saida-cmedi = wa_saida-qtdec / n_months."Consumo Médio Mês ALRS
        wa_saida-qtded = wa_saida-cmedi / 30."Consumo Médio dia

        IF wa_saida-qtded GT 0.
          wa_saida-tcobe = wa_mard-labst / wa_saida-qtded.
        ELSE.
          wa_saida-tcobe = 0.
        ENDIF.

        READ TABLE: it_marc INTO wa_marc WITH KEY matnr = wa_mard-matnr
                                                  werks = wa_mard-werks BINARY SEARCH.
        wa_saida-trepo = wa_marc-plifz.
        wa_saida-tnego = wa_marc-webaz.
        wa_saida-estse = wa_marc-shzet.
        wa_saida-sesto = wa_mard-labst.      "Saldo Estoque

        wa_saida-qlote = wa_marc-bstmi.
        "ponto de ressuprimento
        wa_saida-qponto = ( wa_saida-estse + wa_saida-trepo + wa_saida-tnego ) * wa_saida-qtded.

        vqtde_aux = wa_saida-labst + wa_saida-qrcpen + wa_saida-qpdpen - wa_saida-qtrese.
        IF vqtde_aux GE wa_saida-qponto.
          wa_saida-qtder = 0.
        ELSE.
          vqtde_aux = wa_saida-qponto - wa_saida-labst.
          IF vqtde_aux NE 0.
            "TETO (lote minimo)
            wa_saida-qtder = ( wa_saida-trepo  * wa_saida-qtded ) + ( wa_saida-qponto - wa_saida-labst + wa_saida-qtrese - wa_saida-qrcpen - wa_saida-qpdpen ).
          ELSE.
            vqtde_aux = wa_saida-labst - wa_saida-qrcpen .
            IF wa_saida-qponto  GE vqtde_aux.
              "TETO
              wa_saida-qtder = ( wa_saida-trepo  * wa_saida-qtded ) + ( wa_saida-qponto - wa_saida-labst + wa_saida-qtrese ) - ( wa_saida-qrcpen + wa_saida-qpdpen ).
            ELSE.
              wa_saida-qtder = 0.
            ENDIF.
          ENDIF.
          "WA_SAIDA-QTDER  = ( ( WA_SAIDA-QTRESE + ( WA_SAIDA-QTRESE - WA_SAIDA-LABST ) ) - ( WA_SAIDA-QPDPEN + WA_SAIDA-QRCPEN ) ).
        ENDIF.

        IF wa_saida-qtder NE wa_saida-qlote AND wa_saida-qlote GT 0 AND wa_saida-qtder NE 0.
          vqtde_lote = wa_saida-qtder DIV wa_saida-qlote.
          vqtde_aux  = wa_saida-qtder MOD wa_saida-qlote.
          IF vqtde_aux GT 0.
            ADD 1 TO vqtde_lote.
          ENDIF.
          wa_saida-qtder = vqtde_lote * wa_saida-qlote.
        ENDIF.

        APPEND wa_saida TO it_saida.
        CLEAR wa_saida.
        SORT it_saida.
      ENDLOOP.

      DELETE it_res    WHERE bdmng = 0.
      DELETE it_pedido WHERE menge = 0.
      DELETE it_req    WHERE menge = 0.

      SORT: it_res    BY matnr werks lgort rsnum,
            it_req    BY matnr werks lgort banfn,
            it_pedido BY matnr werks lgort ebeln.
    ELSE.
      v_text = 'Não existe dados para processamento.'.
      MESSAGE v_text TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "ZIF_DATA_READER~READ_DATA


  METHOD zif_data_reader~generate_grid.
    DATA: r_monta_fieldcat TYPE REF TO lcl_monta_fieldcat.

    CREATE OBJECT r_monta_fieldcat.
    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'MATNR'
                                              i_desc_l = 'Material'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'MTART'
                                              i_desc_l = 'Tipo Material'
                                              i_tam    = 10 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'MAKTX'
                                              i_desc_l = 'Desc. Material'
                                              i_tam    = 30 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'WERKS'
                                              i_desc_l = 'Centro'
                                              i_tam    = 6 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'SESTO'
                                              i_desc_l = 'Saldo Estoque'
                                              i_tam    = 12 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'MEINS'
                                              i_desc_l = 'UM'
                                              i_tam    = 5 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.



    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'VESTO'
                                              i_desc_l = 'Valor Estoque R$'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'LGORT'
                                              i_desc_l = 'Deposito'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.


    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTDETP'
                                              i_desc_l = 'Mv.Per.Tipo'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.


    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTDEV'
                                              i_desc_l = 'V-Venda'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTDEC'
                                              i_desc_l = 'C-Consumo'
                                              i_tam    = 8 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'CPERI' "'QTDET'
                                              i_desc_l = 'Mv. Total Per.'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'CMEDI' "'QTDEM'
                                              i_desc_l = 'Mv.Média Mês .'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTDED'
                                              i_desc_l = 'Mv.Média Dia .'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'TCOBE'
                                              i_desc_l = 'Tp.Cober'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'TREPO'
                                              i_desc_l = 'Tp.Repos'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'TNEGO'
                                              i_desc_l = 'Tp.Negoc'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'ESTSE'
                                              i_desc_l = 'Est.Seg'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QPONTO'
                                              i_desc_l = 'Ponto Ressup'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QLOTE'
                                              i_desc_l = 'Lote Mínimo'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QRCPEN'
                                              i_desc_l = 'RC Pend.'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    wa_fcat-hotspot = 'X'.
    APPEND wa_fcat TO it_fcat.
    CLEAR wa_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QPDPEN'
                                              i_desc_l = 'Pedidos Pend.'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    wa_fcat-hotspot = 'X'.
    APPEND wa_fcat TO it_fcat.
    CLEAR wa_fcat.


    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTRESE'
                                              i_desc_l = 'Reservas Aprov.'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    wa_fcat-hotspot = 'X'.
    APPEND wa_fcat TO it_fcat.
    CLEAR wa_fcat.

    r_monta_fieldcat->set_fieldcat( EXPORTING i_campo  = 'QTDER'
                                              i_desc_l = 'Qtde Sugerida RC'
                                              i_tam    = 15 ).

    r_monta_fieldcat->get_fieldcat( IMPORTING e_desc_l = me->wa_fcat-seltext_l
                                              e_campo  = me->wa_fcat-fieldname
                                              e_tam    = me->wa_fcat-outputlen ).
    APPEND wa_fcat TO it_fcat.

    CLEAR wa_evento.
    wa_evento-name = slis_ev_user_command.
    wa_evento-form = 'USER_COMMAND'.
    APPEND wa_evento TO it_evento.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = wa_layout
        it_fieldcat        = it_fcat
        it_events          = it_evento
        i_save             = 'X'
      TABLES
        t_outtab           = it_saida
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
  ENDMETHOD.                    "ZIF_DATA_READER~GENERATE_GRID
ENDCLASS.                    "LCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_monta_fieldcat IMPLEMENTATION.
  METHOD set_fieldcat.
    me->at_campo  = i_campo.
    me->at_desc_l = i_desc_l.
    me->at_tam    = i_tam.
  ENDMETHOD.                    "SET_FIELDCAT

  METHOD get_fieldcat.
    e_campo  = me->at_campo.
    e_desc_l = me->at_desc_l.
    e_tam    = me->at_tam.
  ENDMETHOD.                    "GET_FIELDCAT
ENDCLASS.                    "LCL_MONTA_FIELDCAT IMPLEMENTATION

*----------------------------------------------------------------------*
*       START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: r_read_data     TYPE REF TO lcl_seleciona_dados,
        r_generate_grid TYPE REF TO lcl_seleciona_dados.

  CREATE OBJECT r_read_data.
  r_read_data->zif_data_reader~read_data( ).

  CREATE OBJECT r_generate_grid.
  r_generate_grid->zif_data_reader~generate_grid( ).

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE kkblo_selfield.



  READ TABLE it_saida INTO wa_saida INDEX selfield-tabindex.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_saida-matnr
    IMPORTING
      output = wa_saida-matnr.   "Material

  CASE  selfield-fieldname.
    WHEN 'QTRESE'. "reservas
      REFRESH it_res_aux .
      LOOP AT it_res INTO wa_res WHERE matnr = wa_saida-matnr
                                 AND   werks = wa_saida-werks
                                 AND   lgort = wa_saida-lgort.
        wa_res_aux-bdmng = wa_res-bdmng.
        wa_res_aux-lgort = wa_res-lgort.
        wa_res_aux-matnr = wa_res-matnr.
        wa_res_aux-rsnum = wa_res-rsnum.
        wa_req_aux-werks = wa_res-werks.

        APPEND wa_res_aux TO it_res_aux .
        CLEAR wa_res_aux.
      ENDLOOP.
      field2 = 'QTRESE'.
      PERFORM imprimi_alv_res.

    WHEN 'QRCPEN'. "requisições
      REFRESH it_req_aux.
      LOOP AT it_req INTO wa_req WHERE matnr = wa_saida-matnr
                                 AND   werks = wa_saida-werks
                                 AND   lgort = wa_saida-lgort.
        wa_req_aux-banfn = wa_req-banfn.
        wa_req_aux-matnr = wa_req-matnr.
        wa_req_aux-werks = wa_req-werks.
        wa_req_aux-lgort = wa_req-lgort.
        wa_req_aux-bnfpo = wa_req-bnfpo.
        wa_req_aux-menge = wa_req-menge.

        APPEND wa_req_aux TO it_req_aux.
        CLEAR wa_req_aux.
      ENDLOOP.
      field2 = 'QRCPEN'.
      PERFORM imprimi_alv_req.

    WHEN 'QPDPEN'. "pedidos
      REFRESH it_pedido_aux.
      LOOP AT it_pedido INTO wa_pedido WHERE matnr = wa_saida-matnr
                                       AND   werks = wa_saida-werks
                                       AND   lgort = wa_saida-lgort.

        wa_pedido_aux-matnr = wa_pedido-matnr.
        wa_pedido_aux-werks = wa_pedido-werks.
        wa_pedido_aux-lgort = wa_pedido-lgort.
        wa_pedido_aux-ebeln = wa_pedido-ebeln.
        wa_pedido_aux-ebelp = wa_pedido-ebelp.
        wa_pedido_aux-menge = wa_pedido-menge.

        APPEND wa_pedido_aux TO it_pedido_aux.
        CLEAR wa_pedido_aux.
      ENDLOOP.
      field2 = 'QPDPEN'.
      PERFORM imprimi_alv_ped.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.


FORM imprimi_alv_res.
  REFRESH it_fieldcatalog.
  "CLEAR: IT_FIELDCATALOG[].
  PERFORM preenche_cat USING:
       'RSNUM'           'Reserva'                         '14'     ''      'X' '' '',
       'BDMNG'           'Quantidade'                      '15'     ''       '' '' ''  .
  CALL SCREEN 0100.
ENDFORM.


FORM imprimi_alv_req.
  REFRESH it_fieldcatalog.
  " CLEAR: IT_FIELDCATALOG[].
  PERFORM preenche_cat USING:
       'BANFN'           'Requisição'         '14'     ''      'X' '' '' ,
       'MENGE'           'Quantidade'         '15'     ''      ''  '' '' .

  CALL SCREEN 0102.

ENDFORM.

FORM imprimi_alv_ped.
  REFRESH it_fieldcatalog.

  "CLEAR: IT_FIELDCATALOG[].
  PERFORM preenche_cat USING:
       'EBELN'           'Pedido'             '14'     ''      'X' '' ''  ,
       'MENGE'           'Quantidade'         '15'     ''      ''  '' '' .
  CALL SCREEN 0101.

ENDFORM.


FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just).

  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.


  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.


  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  CASE  field2.
    WHEN 'QTRESE'. "reservas
      PERFORM alv_reserva.

    WHEN 'QRCPEN'. "requisições
      PERFORM alv_requisicao.

    WHEN 'QPDPEN'. "pedidos
      PERFORM alv_pedido.
  ENDCASE.

  CLEAR field2 .

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


FORM user_command_0100 USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE  lvc_s_col
                             p_es_row_no   TYPE  lvc_s_roid.


  READ TABLE it_res_aux    INTO wa_res_aux    INDEX e_row_id-index.
  READ TABLE it_req_aux    INTO wa_req_aux    INDEX e_row_id-index.
  READ TABLE it_pedido_aux INTO wa_pedido_aux INDEX e_row_id-index.


  CASE  p_e_column_id-fieldname.
    WHEN 'RSNUM'.
      SET PARAMETER ID 'RES' FIELD wa_res_aux-rsnum.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
    WHEN 'BANFN'  .
      SET PARAMETER ID 'BAN' FIELD wa_req_aux-banfn.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
    WHEN 'EBELN'.
      SET PARAMETER ID 'BES' FIELD wa_pedido_aux-ebeln.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.

FORM alv_reserva.



  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 0.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 0
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    "CLEAR: IT_EXCLUDE_FCODE, IT_EXCLUDE_FCODE[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_res_aux.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.
  ELSE.
    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.


FORM alv_requisicao.


  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2
      EXPORTING
        container_name              = 'CONTAINER1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container2
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 0.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 0
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv2
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv2.

    CALL METHOD ctl_alv2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_req_aux.


    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.
  ELSE.
    CALL METHOD ctl_alv2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.


FORM alv_pedido.



  IF g_custom_container3 IS INITIAL.
    CREATE OBJECT g_custom_container3
      EXPORTING
        container_name              = 'CONTAINER2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container3
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 0.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 0
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 0.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].


    CREATE OBJECT ctl_alv3
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER: lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv3.

    CALL METHOD ctl_alv3->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_pedido_aux.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.
  ELSE.
    CALL METHOD ctl_alv3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.
