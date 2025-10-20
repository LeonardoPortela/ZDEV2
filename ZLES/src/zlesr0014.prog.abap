*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: AMAGGI                                                  *
* Autor......: Vagner Rodrigues dos Santos                             *
* Data.......: 04.09.2010                                              *
* Descrição  : Programa de geração de uma lista contendo as faturas de *
*              frete de terceiros.                                     *
* Transação..:                                                         *
* Projeto....: INOVAR                                                  *
* Cód Espec..:                                                         *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                                         *
* Observações:                                                         *
*---------------------------------------------------------------------*

REPORT zlesr0014 MESSAGE-ID zles.

INCLUDE zlesr0014_top.

INCLUDE zlesr0014_simulacao.

INCLUDE zlesr00140200.

INCLUDE zlesr00140201.

INCLUDE zlesr00140202.

INCLUDE zlesr00140203.

INCLUDE zlesr00149999.

*----------------------------------------------------------------------*
* Critérios de seleção

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-038.
  PARAMETERS: ra1 RADIOBUTTON GROUP rb1 USER-COMMAND sel DEFAULT 'X',
              ra2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-043.
  PARAMETERS: ra5 RADIOBUTTON GROUP rb2 DEFAULT 'X',
              ra3 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK bl_001 WITH FRAME TITLE TEXT-010.
  SELECT-OPTIONS: s_tpft      FOR t173-vsart,
                  s_tknum     FOR zlest0034-tknum,
                  s_fknum     FOR zlest0034-fknum,
                  s_ebeln     FOR zlest0034-ebeln,
                  s_ebelp     FOR zlest0034-ebelp,
                  s_lblni     FOR zlest0034-lblni,
                  s_forne     FOR lfa1-lifnr,
                  s_erdat     FOR essr-erdat        MODIF ID ra1,
                  s_dtcon     FOR zlest0034-zdt_mov MODIF ID ra2.
SELECTION-SCREEN: END OF BLOCK bl_001.

*----------------------------------------------------------------------*
* Inicializar os coeficientes utilizados pelo programa
INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

  IF NOT ra1 IS INITIAL.

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'RA1'.
          screen-active = 1.
          MODIFY SCREEN.
        WHEN 'RA2'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ELSEIF NOT ( ra2 IS INITIAL ).

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'RA1'.
          screen-active = 0.
          MODIFY SCREEN.
        WHEN 'RA2'.
          screen-active = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF ( s_tpft IS INITIAL ).
    MESSAGE 'Deve ser Informado o Tipo de Transporte' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF NOT ra1 IS INITIAL.
    IF s_erdat[] IS INITIAL.
      MESSAGE 'Deve ser informado a Data da Folha Serviço' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.
  IF NOT ( ra2 IS INITIAL ).
    IF s_dtcon[] IS INITIAL.
      MESSAGE 'Deve ser informado a Data de Movimento' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDIF.

  IF NOT ra3 IS INITIAL.
    IF s_forne[] IS INITIAL.
      MESSAGE i897(sd) WITH TEXT-i01.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  "Obter dados da tabela zlest0032.
  PERFORM z_ler_dados_diversos.
  "Obter dados da nota fiscal de serviço de transporte
  PERFORM z_obter_dados_nf.
  "Agrupar os dados em uma única tabela
  PERFORM z_agrupar_dados.

  IF ra3 IS INITIAL.
    PERFORM z_processar_dados.
  ELSE.
    EXPORT ti_dados TO MEMORY ID 'ZCOMP'.
    SUBMIT zlesr0017 AND RETURN.
  ENDIF.

END-OF-SELECTION.

  "  PERFORM z_processar_dados.

*&---------------------------------------------------------------------*
*&      Form  Z_LER_DADOS_DIVERSOS
*&---------------------------------------------------------------------*
*  Obter dados de várias tabelas..
*----------------------------------------------------------------------*
FORM z_ler_dados_diversos.

  REFRESH ti_dados.

  IF NOT ra1 IS INITIAL.

    SELECT z~tknum z~fknum v~exti1 v~exti2 z~ebeln z~ebelp
           z~lblni v~shtyp v~tdlnr p~meins l~name1 e~lifnr
           v~vsart z~add03 f~budat
      INTO TABLE ti_dados_tab
      FROM zlest0032 AS z
     INNER JOIN essr AS f ON f~lblni = z~lblni
     INNER JOIN vttk AS v ON v~tknum = z~tknum
     INNER JOIN ekko AS e ON e~ebeln = z~ebeln
     INNER JOIN ekpo AS p ON p~ebeln = z~ebeln AND p~ebelp = z~ebelp
     INNER JOIN lfa1 AS l ON l~lifnr = v~tdlnr
     WHERE z~tknum IN s_tknum
       AND z~fknum IN s_fknum
       AND z~ebeln IN s_ebeln
       AND z~ebelp IN s_ebelp
       AND z~lblni IN s_lblni
       AND z~add03 NE c_0000000001
       AND z~belnr EQ space
       AND e~lifnr IN s_forne
       AND f~erdat IN s_erdat
       AND v~vsart IN s_tpft.

  ELSE.

    SELECT z~tknum z~fknum v~exti1 v~exti2 z~ebeln z~ebelp
           z~lblni v~shtyp v~tdlnr p~meins l~name1 e~lifnr
           v~vsart z~add03 f~budat
      INTO TABLE ti_dados_tab
      FROM zlest0032 AS z
     INNER JOIN zlest0034 AS c ON c~tknum = z~tknum
     INNER JOIN essr AS f ON f~lblni = z~lblni
     INNER JOIN vttk AS v ON v~tknum = z~tknum
     INNER JOIN ekko AS e ON e~ebeln = z~ebeln
     INNER JOIN ekpo AS p ON p~ebeln = z~ebeln AND p~ebelp = z~ebelp
     INNER JOIN lfa1 AS l ON l~lifnr = v~tdlnr
     WHERE z~tknum   IN s_tknum
       AND z~fknum   IN s_fknum
       AND z~ebeln   IN s_ebeln
       AND z~ebelp   IN s_ebelp
       AND z~lblni   IN s_lblni
       AND z~add03   NE c_0000000001
       AND z~belnr   NE space
       AND e~lifnr   IN s_forne
       AND c~zdt_mov IN s_dtcon
       AND v~vsart IN s_tpft.

  ENDIF.


  IF ( ti_dados_tab[] IS INITIAL ).
    PERFORM: dados_nao_encontrado.
  ENDIF.

ENDFORM.                    " Z_LER_DADOS_DIVERSOS
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
* Obter os dados da nota fiscal de serviço de transporte a partir do
* documento de transporte.
*----------------------------------------------------------------------*
FORM z_obter_dados_nf.

  TYPES: BEGIN OF ty_vbrp.
           INCLUDE STRUCTURE vbrp.
  TYPES:   refkey TYPE j_1brefkey.
  TYPES: END OF ty_vbrp.

  TYPES: BEGIN OF y_vbfa_docm,
           vbeln        TYPE vbfa-vbeln,
           mjahr        TYPE vbfa-mjahr,
           posnn        TYPE vbfa-posnn,
           vbeln_35(35) TYPE c,
         END OF y_vbfa_docm,

         BEGIN OF ty_rbkp,
           xblnr     TYPE rbkp-xblnr,
           belnr     TYPE rbkp-belnr,
           gjahr     TYPE rbkp-gjahr,
           stblg     TYPE rbkp-stblg,
           belnr_ano TYPE j_1bnflin-refkey,
         END OF ty_rbkp.

  DATA: it_vttp             TYPE TABLE OF vttp INITIAL SIZE 0 WITH HEADER LINE,
        it_vbrp             TYPE TABLE OF vbrp INITIAL SIZE 0 WITH HEADER LINE,
        it_vbrpr            TYPE TABLE OF ty_vbrp INITIAL SIZE 0 WITH HEADER LINE,
        it_linnf            TYPE TABLE OF j_1bnflin INITIAL SIZE 0 WITH HEADER LINE,
        it_docnf            TYPE TABLE OF j_1bnfdoc INITIAL SIZE 0 WITH HEADER LINE,
        it_zfiwrt0008       TYPE TABLE OF zfiwrt0008,
        gw_zfiwrt0008       TYPE zfiwrt0008,
        wa_vttp             TYPE vttp,
        wa_zmmt_ee_zgr_docs TYPE zmmt_ee_zgr_docs,
        it_vbfap_docm       TYPE TABLE OF y_vbfa_docm,
        wa_vbfap_docm       TYPE y_vbfa_docm,
        vl_tabix            TYPE sy-tabix,
        vl_mblnr            TYPE mblnr,
        wa_vbrp             TYPE vbrp,
        wa_vttk             TYPE vttk,
        wa_vbrpr            TYPE ty_vbrp,
        wa_linnf            TYPE j_1bnflin,
        wa_docnf            TYPE j_1bnfdoc,

        tl_likp             TYPE TABLE OF likp,
        wl_likp             TYPE likp,
        tl_lips             TYPE TABLE OF lips,
        wl_lips             TYPE lips,
        tl_ekbe             TYPE TABLE OF ekbe,
        wl_ekbe             TYPE ekbe,
        tl_rbkp             TYPE TABLE OF ty_rbkp,
        wl_rbkp             TYPE ty_rbkp,
        tabix               TYPE sy-tabix.

  DATA: wl_nfe       TYPE c LENGTH 9,
        wl_serie     TYPE c LENGTH 3,
        wl_nfe_serie TYPE c LENGTH 12.


  DATA: belnr_ano TYPE j_1bnflin-refkey.
* Obter as notas fiscais a partir dos documentos de transporte
  REFRESH: ti_notas.

  SELECT * INTO TABLE it_vttp
    FROM vttp
     FOR ALL ENTRIES IN ti_dados_tab
   WHERE tknum EQ ti_dados_tab-tknum.

  CHECK NOT it_vttp[] IS INITIAL.

  "  if wa_vttk-shtyp = c_z020.

  SELECT * INTO TABLE it_vbrp          "#EC CI_DB_OPERATION_OK[2768887]
    FROM vbrp AS p
     FOR ALL ENTRIES IN it_vttp
   WHERE vgbel EQ it_vttp-vbeln
     AND EXISTS ( SELECT *
                    FROM j_1bnflin AS l
                   INNER JOIN j_1bnfdoc AS d ON ( d~mandt EQ l~mandt AND d~docnum EQ l~docnum )
                   WHERE l~mandt  EQ p~mandt
                     AND l~refkey EQ p~vbeln
                     AND l~mandt  EQ d~mandt
                     AND l~docnum EQ d~docnum
                     AND d~cancel NE 'X'
                     AND d~direct EQ '2'
                     AND d~doctyp EQ '1' ) AND p~draft = space .

*  IF ( SY-SUBRC NE 0 ).
*
*    SELECT * FROM LIPS INTO TABLE TL_LIPS
*      FOR ALL ENTRIES IN IT_VTTP
*     WHERE VBELN EQ IT_VTTP-VBELN.
*
*    CHECK NOT TL_LIPS[] IS INITIAL.
*
*    SELECT * FROM LIKP INTO TABLE TL_LIKP
*      FOR ALL ENTRIES IN IT_VTTP
*     WHERE VBELN EQ IT_VTTP-VBELN.
*
*    SELECT * FROM EKBE INTO TABLE TL_EKBE
*      FOR ALL ENTRIES IN TL_LIPS
*     WHERE EBELN EQ TL_LIPS-VGBEL
*       AND VGABE EQ '2'
*       AND GJAHR EQ TL_LIPS-ERDAT(4).
*
*    SELECT XBLNR BELNR GJAHR STBLG FROM RBKP INTO TABLE TL_RBKP
*      FOR ALL ENTRIES IN TL_EKBE
*    WHERE BELNR EQ TL_EKBE-BELNR
*      AND GJAHR EQ TL_EKBE-GJAHR
*      AND STBLG EQ ''.
*
*   IF ( SY-SUBRC EQ 0 ).
*
*      LOOP AT TL_RBKP INTO WL_RBKP.
*
*        TABIX  = SY-TABIX.
*        READ TABLE TL_LIKP INTO WL_LIKP WITH KEY LIFEX = WL_RBKP-XBLNR.
*
*        IF ( SY-SUBRC NE 0 ).
*          DELETE TL_RBKP INDEX TABIX.
*        ELSE.
*          CLEAR: BELNR_ANO.
*          CONCATENATE WL_RBKP-BELNR WL_RBKP-GJAHR INTO BELNR_ANO.
*          WL_RBKP-BELNR_ANO = BELNR_ANO.
*          MODIFY TL_RBKP INDEX TABIX FROM WL_RBKP.
*        ENDIF.
*
*      ENDLOOP.
*
*    SELECT * INTO TABLE IT_LINNF
*      FROM J_1BNFLIN
*       FOR ALL ENTRIES IN TL_RBKP
*     WHERE REFKEY EQ TL_RBKP-BELNR_ANO
*       AND REFTYP EQ C_BI.
*
*    IF NOT IT_LINNF[] IS INITIAL.
*
*      SELECT * INTO TABLE IT_DOCNF
*        FROM J_1BNFDOC
*         FOR ALL ENTRIES IN IT_LINNF
*       WHERE DOCNUM EQ IT_LINNF-DOCNUM.
*
*    ENDIF.
*   ENDIF.
*
*  ELSE.

  "CHECK NOT IT_VTTP[] IS INITIAL.

  IF NOT it_vbrp[] IS INITIAL.

    LOOP AT it_vbrp INTO wa_vbrp.
      MOVE-CORRESPONDING wa_vbrp TO wa_vbrpr.
      wa_vbrpr-refkey = wa_vbrp-vbeln.
      APPEND wa_vbrpr TO it_vbrpr.
    ENDLOOP.

    SELECT * INTO TABLE it_linnf
      FROM j_1bnflin
       FOR ALL ENTRIES IN it_vbrpr
     WHERE refkey EQ it_vbrpr-refkey
       AND refitm EQ it_vbrpr-posnr
       AND reftyp EQ c_bi.

    IF NOT it_linnf[] IS INITIAL.

      SELECT * INTO TABLE it_docnf
        FROM j_1bnfdoc
         FOR ALL ENTRIES IN it_linnf
       WHERE docnum EQ it_linnf-docnum.

    ENDIF.

  ENDIF.
  "ENDIF.

  SORT it_vttp  BY tknum.
  SORT it_vbrpr  BY vgbel.
  SORT it_linnf BY refkey refitm.
  SORT it_docnf BY docnum.

  LOOP AT it_vttp INTO wa_vttp.

    SELECT SINGLE * INTO wa_vttk
      FROM vttk
     WHERE tknum EQ wa_vttp-tknum.

    IF wa_vttk-shtyp = 'Z020'.

      " Obter o documento de material
      CLEAR:  wa_vbfap_docm, it_vbfap_docm.

      SELECT vbeln mjahr posnn vbeln INTO TABLE it_vbfap_docm
        FROM vbfa
       WHERE vbelv = wa_vttp-vbeln
         AND vbtyp_v = 'J'   "Entrega
         AND vbtyp_n = 'R'.  "Movimento de Mercadoria

* Eliminar o documento de material estornado.
      LOOP AT it_vbfap_docm INTO wa_vbfap_docm.
        vl_tabix = sy-tabix.

        CLEAR vl_mblnr.
        SELECT mblnr INTO vl_mblnr
          FROM mseg
         UP TO 1 ROWS
         WHERE smbln = wa_vbfap_docm-vbeln
           AND sjahr = wa_vbfap_docm-mjahr
           AND bwart = '864'.   "Movimento de estorno
        ENDSELECT.

        IF sy-subrc EQ 0.
          DELETE it_vbfap_docm INDEX vl_tabix.
        ELSE.
* Agrupar o documento de material e o ano em um mesmo campo.
          CONCATENATE wa_vbfap_docm-vbeln_35  wa_vbfap_docm-mjahr INTO wa_vbfap_docm-vbeln_35.
          MODIFY it_vbfap_docm FROM wa_vbfap_docm INDEX vl_tabix TRANSPORTING vbeln_35.
        ENDIF.
      ENDLOOP.

      READ TABLE it_vbfap_docm INDEX 1 INTO wa_vbfap_docm.

      SELECT SINGLE *
        INTO wa_linnf
        FROM j_1bnflin
       WHERE refkey = wa_vbfap_docm-vbeln_35
         AND refitm = wa_vbfap_docm-posnn
         AND reftyp = 'MD'. "Documento de material

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * INTO wa_docnf
          FROM j_1bnfdoc
         WHERE docnum EQ wa_linnf-docnum.

        st_notas-tknum  = wa_vttp-tknum.
        st_notas-docnum = wa_docnf-docnum.

        IF wa_docnf-nfe IS INITIAL.
          MOVE wa_docnf-nfnum TO wa_docnf-nfenum.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_docnf-nfenum
            IMPORTING
              output = wa_docnf-nfenum.
        ENDIF.
        st_notas-nfenum = wa_docnf-nfenum.
        APPEND st_notas TO ti_notas.
      ENDIF.

    ELSEIF wa_vttk-shtyp = 'Z021'.

      SELECT SINGLE * INTO wa_zmmt_ee_zgr_docs
        FROM zmmt_ee_zgr_docs
       WHERE av_vbeln EQ wa_vttp-vbeln.

      IF ( sy-subrc IS INITIAL ) AND ( NOT wa_zmmt_ee_zgr_docs-docnum IS INITIAL ).

        st_notas-tknum   = wa_vttp-tknum.
        st_notas-vbeln_k = wa_vttp-vbeln.

        SELECT SINGLE * INTO wa_linnf
          FROM j_1bnflin
         WHERE docnum EQ wa_zmmt_ee_zgr_docs-docnum.

        SELECT SINGLE * INTO wa_docnf
          FROM j_1bnfdoc
         WHERE docnum EQ wa_zmmt_ee_zgr_docs-docnum.

        IF wa_docnf-nfe IS INITIAL.
          MOVE wa_docnf-nfnum TO wa_docnf-nfenum.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_docnf-nfenum
            IMPORTING
              output = wa_docnf-nfenum.
        ENDIF.

        st_notas-docnum = wa_docnf-docnum.
        st_notas-nfenum = wa_docnf-nfenum.
        APPEND st_notas TO ti_notas.

      ENDIF.

      "Modificação para Encontrar nota fiscal - Victor Hugo - 10.09.2013 - inicio

      SELECT SINGLE * FROM lips INTO wl_lips WHERE vbeln EQ wa_vttp-vbeln.
      IF ( sy-subrc EQ 0 ).

        SELECT SINGLE * FROM likp INTO wl_likp WHERE vbeln EQ wa_vttp-vbeln.

        IF ( sy-subrc EQ 0 ).

          SELECT * FROM ekbe INTO TABLE tl_ekbe
           WHERE ebeln EQ wl_lips-vgbel
             AND vgabe EQ '2'
             AND gjahr EQ wl_lips-erdat(4).

          CHECK NOT tl_ekbe[] IS INITIAL.

          SELECT xblnr belnr gjahr stblg FROM rbkp INTO TABLE tl_rbkp
            FOR ALL ENTRIES IN tl_ekbe
          WHERE belnr EQ tl_ekbe-belnr
            AND gjahr EQ tl_ekbe-gjahr
            AND stblg EQ ''.

          LOOP AT tl_rbkp INTO wl_rbkp.

            tabix = sy-tabix.

            IF (  wl_rbkp-xblnr NE wl_likp-lifex ).
              DELETE tl_rbkp INDEX tabix.
            ELSE.

              CLEAR: belnr_ano.
              CONCATENATE wl_rbkp-belnr wl_rbkp-gjahr INTO belnr_ano.


              CLEAR: wa_linnf, wa_docnf.
              SELECT SINGLE * INTO wa_linnf FROM j_1bnflin WHERE refkey EQ belnr_ano AND  reftyp EQ 'LI'.

              IF ( sy-subrc EQ 0 ).

                SELECT SINGLE * INTO wa_docnf FROM j_1bnfdoc WHERE docnum EQ wa_linnf-docnum.

                st_notas-tknum   = wa_vttp-tknum.
                st_notas-vbeln_k = wa_vttp-vbeln.
                st_notas-docnum  = wa_docnf-docnum.

                IF ( wa_docnf-nfe EQ 'X' ).
                  st_notas-nfenum = wa_docnf-nfenum.
                ELSE.
                  st_notas-nfenum = wa_docnf-nfnum.
                ENDIF.
                APPEND st_notas TO ti_notas.

              ENDIF.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
      "Modificação para Encontrar nota fiscal - Victor Hugo - 10.09.2013 - fim

    ELSEIF ( wa_vttk-shtyp EQ 'Z027' ).
      SELECT SINGLE * FROM lips INTO wl_lips WHERE vbeln EQ wa_vttp-vbeln.
      IF ( sy-subrc EQ 0 ).
        SELECT SINGLE * FROM likp INTO wl_likp WHERE vbeln EQ wa_vttp-vbeln.

        IF ( sy-subrc EQ 0 ).

          REFRESH: it_zfiwrt0008[], it_docnf[], it_linnf[].

          SELECT * FROM zfiwrt0008
            INTO TABLE it_zfiwrt0008
          WHERE ebeln EQ wl_lips-vgbel.

          CHECK NOT it_zfiwrt0008[] IS INITIAL.

          SELECT * FROM j_1bnfdoc
            INTO TABLE it_docnf
            FOR ALL ENTRIES IN it_zfiwrt0008
          WHERE docnum EQ it_zfiwrt0008-docnum.

          IF ( sy-subrc EQ 0 ).

            CLEAR: tabix.

            LOOP AT it_docnf INTO wa_docnf.
              tabix = sy-tabix.

              CLEAR: wl_nfe, wl_serie, wl_nfe_serie.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_docnf-nfenum
                IMPORTING
                  output = wl_nfe.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_docnf-series
                IMPORTING
                  output = wl_serie.

              CONCATENATE wl_nfe '-' wl_serie INTO wl_nfe_serie.


              IF ( wl_nfe_serie NE wl_likp-lifex ).
                DELETE it_docnf INDEX tabix.
              ENDIF.
              CLEAR: wa_docnf, tabix.
            ENDLOOP.

            IF  NOT ( it_docnf[] IS INITIAL ).
              SELECT * INTO TABLE it_linnf
               FROM j_1bnflin
              FOR ALL ENTRIES IN it_docnf
              WHERE docnum EQ it_docnf-docnum.

              CLEAR: st_notas.
              LOOP AT it_linnf INTO wa_linnf.

                st_notas-tknum   = wa_vttp-tknum.
                st_notas-vbeln_k = wa_vttp-vbeln.
                st_notas-docnum  = wa_linnf-docnum.
                READ TABLE it_docnf INTO wa_docnf WITH KEY docnum = st_notas-docnum.
                st_notas-nfenum  = wa_docnf-nfenum.

                APPEND st_notas TO ti_notas.

                CLEAR: st_notas, wa_docnf, wa_linnf.
              ENDLOOP.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      CLEAR: st_notas.
      st_notas-tknum   = wa_vttp-tknum.
      st_notas-vbeln_k = wa_vttp-vbeln.
      READ TABLE it_vbrpr INTO wa_vbrpr WITH KEY vgbel = wa_vttp-vbeln BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        st_notas-vbeln_f = wa_vbrpr-vbeln.
        st_notas-posnn   = wa_vbrpr-posnr.
        READ TABLE it_linnf INTO wa_linnf WITH KEY refkey = wa_vbrpr-refkey refitm = wa_vbrpr-posnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          st_notas-docnum = wa_linnf-docnum.
          READ TABLE it_docnf INTO wa_docnf WITH KEY docnum = wa_linnf-docnum BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF wa_docnf-nfe IS INITIAL.
              MOVE wa_docnf-nfnum TO wa_docnf-nfenum.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_docnf-nfenum
                IMPORTING
                  output = wa_docnf-nfenum.
            ENDIF.
            st_notas-nfenum = wa_docnf-nfenum.
            APPEND st_notas TO ti_notas.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " Z_OBTER_DADOS_NF
*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSAR_DAOS
*&---------------------------------------------------------------------*
* Processar os dados encontrados, alimentando a estrutura e tabelas da
* bapi e a tabela local.
*----------------------------------------------------------------------*
FORM z_processar_dados.

  "CHECK ti_dados[] IS NOT INITIAL.

* Classificar a tabela por fornecedor e documento de transporte
  SORT ti_dados BY tdlnr ASCENDING
                   tknum ASCENDING.

  PERFORM: z_estrutura_alv,
           z_montar_cabec_alv USING st_top_header,
           z_montar_field_cat,
           z_selecionar_eventos,
           z_apresentar_alv.

ENDFORM.                    " Z_MONTAR_BAPI_NF
*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
* Alimentar a estrutura do ALV
*----------------------------------------------------------------------*
FORM z_estrutura_alv .

  st_layout-cell_merge        = c_x.
  st_layout-expand_all        = c_x.
  st_layout-zebra             = c_x.
  st_layout-window_titlebar   = TEXT-013. " Frete de terceiros
  st_layout-detail_titlebar   = TEXT-013.
  st_layout-colwidth_optimize = c_x.
  st_layout-box_fieldname     = c_box.
  st_layout-box_tabname       = c_ti_dados.

ENDFORM.                    " Z_ESTRUTURA_ALV
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_CABEC_ALV
*&---------------------------------------------------------------------*
* Montar o cabeçalho do ALV
*----------------------------------------------------------------------*
FORM z_montar_cabec_alv USING  p_top_header TYPE slis_t_listheader.

  DATA: ls_line   TYPE slis_listheader,  "Títulos no ALV.
        vl_transp TYPE lfa1-name1.

  CLEAR: p_top_header.

  CLEAR ls_line.
  ls_line-typ  = c_h.             "Header
  ls_line-info = TEXT-012.
  APPEND ls_line TO p_top_header.

*  clear ls_line.
*  ls_line-typ  = c_s.             "Selection
*  ls_line-info = text-008.
*  append ls_line to p_top_header.

* Documento de transporte
  CLEAR ls_line-info.
  ls_line-typ = c_a.
  IF s_tknum[] IS NOT INITIAL.
    LOOP AT s_tknum.
      IF s_tknum-high IS NOT INITIAL.
        CONCATENATE TEXT-t05
                    s_tknum-low
                    c_ate
                    s_tknum-high INTO ls_line-info SEPARATED BY space.
      ELSE.
        CONCATENATE TEXT-t05
                    s_tknum-low INTO ls_line-info SEPARATED BY space.
      ENDIF.
      APPEND ls_line TO p_top_header.
    ENDLOOP.
  ENDIF.

* Custo de frete
  CLEAR ls_line-info.
  IF s_tknum[] IS NOT INITIAL.
    LOOP AT s_fknum.
      IF s_fknum-high IS NOT INITIAL.
        CONCATENATE TEXT-t06
                    s_fknum-low
                    c_ate
                    s_fknum-high INTO ls_line-info SEPARATED BY space.
      ELSE.
        CONCATENATE TEXT-t06
                    s_fknum-low INTO ls_line-info SEPARATED BY space.
      ENDIF.
      APPEND ls_line TO p_top_header.
    ENDLOOP.
  ENDIF.

* Pedido de compra
  CLEAR ls_line-info.
  IF s_ebeln[] IS NOT INITIAL.
    LOOP AT s_ebeln.
      IF s_ebeln-high IS NOT INITIAL.
        CONCATENATE TEXT-t14
                    s_ebeln-low
                    c_ate
                    s_ebeln-high INTO ls_line-info SEPARATED BY space.
      ELSE.
        CONCATENATE TEXT-t14
                    s_ebeln-low INTO ls_line-info SEPARATED BY space.
      ENDIF.
      APPEND ls_line TO p_top_header.
    ENDLOOP.
  ENDIF.

* Pedido de compra
  CLEAR ls_line-info.
  IF s_ebelp[] IS NOT INITIAL.
    LOOP AT s_ebelp.
      IF s_ebelp-high IS NOT INITIAL.
        CONCATENATE TEXT-t28
                    s_ebelp-low
                    c_ate
                    s_ebelp-high INTO ls_line-info SEPARATED BY space.
      ELSE.
        CONCATENATE TEXT-t28
                    s_ebelp-low INTO ls_line-info SEPARATED BY space.
      ENDIF.
      APPEND ls_line TO p_top_header.
    ENDLOOP.
  ENDIF.

* Folha de serviço
  CLEAR ls_line-info.
  IF s_lblni[] IS NOT INITIAL.
    LOOP AT s_lblni.
      IF s_lblni-high IS NOT INITIAL.
        CONCATENATE TEXT-t15
                    s_lblni-low
                    c_ate
                    s_lblni-high INTO ls_line-info SEPARATED BY space.
      ELSE.
        CONCATENATE TEXT-t15
                    s_lblni-low INTO ls_line-info SEPARATED BY space.
      ENDIF.
      APPEND ls_line TO p_top_header.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " Z_MONTAR_CABEC_ALV
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_montar_field_cat .

  CLEAR: ti_fieldcat.

  PERFORM z_estrutura_fieldcat USING:
  c_tdlnr            c_ti_dados TEXT-t01 TEXT-t01 ' ' 01,
  c_name1            c_ti_dados TEXT-t02 TEXT-t02 ' ' 02,
  c_status           c_ti_dados TEXT-t03 TEXT-t03 ' ' 03,
  c_shtyp            c_ti_dados TEXT-t04 TEXT-t04 ' ' 04,
  c_tknum            c_ti_dados TEXT-t05 TEXT-t05 'X' 05,
  c_fknum            c_ti_dados TEXT-t06 TEXT-t06 'X' 06,
  c_nfenum           c_ti_dados TEXT-t07 TEXT-t07 ' ' 07,
  c_zdt_mov          c_ti_dados TEXT-t08 TEXT-t08 ' ' 08,
  c_zdt_vencto       c_ti_dados TEXT-t09 TEXT-t09 ' ' 09,
  c_nr_conhec        c_ti_dados TEXT-t10 TEXT-t10 ' ' 10,
  c_series           c_ti_dados TEXT-t53 TEXT-t53 ' ' 11,
  c_zdt_conhec       c_ti_dados TEXT-t11 TEXT-t11 ' ' 12,
  c_ebeln            c_ti_dados TEXT-t14 TEXT-t14 'X' 13,
  c_lblni            c_ti_dados TEXT-t15 TEXT-t15 ' ' 14,
  c_zpeso_origem     c_ti_dados TEXT-t16 TEXT-t16 ' ' 15,
  c_zpeso_destino    c_ti_dados TEXT-t12 TEXT-t12 ' ' 16,
  c_zpeso_diferenca  c_ti_dados TEXT-t20 TEXT-t20 ' ' 17,
  c_zdt_chegada      c_ti_dados TEXT-t13 TEXT-t13 ' ' 18,
  c_gewei            c_ti_dados TEXT-t17 TEXT-t17 ' ' 19,
  c_dmbtr            c_ti_dados TEXT-t18 TEXT-t18 ' ' 20,
  c_dmbtr_doc        c_ti_dados TEXT-t40 TEXT-t40 ' ' 21,
  c_kbetr            c_ti_dados TEXT-t39 TEXT-t39 ' ' 22,
  c_zquebra          c_ti_dados TEXT-t21 TEXT-t21 ' ' 23,
  c_zvlr_quebra      c_ti_dados TEXT-t23 TEXT-t23 ' ' 24,
  c_zperda           c_ti_dados TEXT-t22 TEXT-t22 ' ' 25,
  c_zvlr_perda       c_ti_dados TEXT-t24 TEXT-t24 ' ' 26,
  c_zvlr_liq_pagar   c_ti_dados TEXT-t25 TEXT-t25 ' ' 27,
  c_valor_pedagio    c_ti_dados TEXT-t54 TEXT-t54 ' ' 28,
  c_matnr            c_ti_dados TEXT-t26 TEXT-t26 ' ' 29,
  c_maktx            c_ti_dados TEXT-t27 TEXT-t27 ' ' 30,
  'RE_BELNR'         c_ti_dados TEXT-t29 TEXT-t29 'X' 31,
  'RE_GJAHR'         c_ti_dados TEXT-t30 TEXT-t30 ' ' 32,
  'EN_DOCNUM'        c_ti_dados TEXT-t31 TEXT-t31 'X' 33,
  'BUKRS'            c_ti_dados TEXT-t32 TEXT-t32 ' ' 34,
  'WERKS'            c_ti_dados TEXT-t33 TEXT-t33 ' ' 35,
  'WAERS'            c_ti_dados TEXT-t34 TEXT-t34 ' ' 36,
  'KURST'            c_ti_dados TEXT-t35 TEXT-t35 ' ' 37,
  'IVA'              c_ti_dados TEXT-t36 TEXT-t36 ' ' 38,
  'MATNS'            c_ti_dados TEXT-t37 TEXT-t37 ' ' 39,
  'MAKTS'            c_ti_dados TEXT-t38 TEXT-t38 ' ' 41,
  c_bvtyp            c_ti_dados TEXT-t41 TEXT-t41 ' ' 42,
  c_regio_emissor    c_ti_dados TEXT-t42 TEXT-t42 ' ' 43,
  c_regio_receptor   c_ti_dados TEXT-t43 TEXT-t43 ' ' 44,
  c_base_icms        c_ti_dados TEXT-t44 TEXT-t44 ' ' 45,
  c_base_pis         c_ti_dados TEXT-t45 TEXT-t45 ' ' 46,
  c_base_cofins      c_ti_dados TEXT-t46 TEXT-t46 ' ' 47,
  c_rate_icms        c_ti_dados TEXT-t47 TEXT-t47 ' ' 48,
  c_rate_pis         c_ti_dados TEXT-t48 TEXT-t48 ' ' 49,
  c_rate_cofins      c_ti_dados TEXT-t49 TEXT-t49 ' ' 50,
  c_valor_icms       c_ti_dados TEXT-t50 TEXT-t50 ' ' 51,
  c_valor_pis        c_ti_dados TEXT-t51 TEXT-t51 ' ' 52,
  c_valor_cofins     c_ti_dados TEXT-t52 TEXT-t52 ' ' 53,
  c_docnum           c_ti_dados TEXT-t55 TEXT-t55 'X' 54,
  c_valor_mercadoria c_ti_dados TEXT-t56 TEXT-t56 ' ' 55.

ENDFORM.                    " Z_MONTAR_FIELD_CAT

*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_FIELDCAT
*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat  USING    p_fieldname
                                    p_tabname
                                    p_texto_grande
                                    p_texto_medio
                                    p_hot
                                    posicao.
  CLEAR st_fieldcat.
  st_fieldcat-fieldname     = p_fieldname.
  st_fieldcat-tabname       = p_tabname.
  st_fieldcat-seltext_l     = p_texto_grande.
  st_fieldcat-seltext_m     = p_texto_medio.
  st_fieldcat-hotspot       = p_hot.
  st_fieldcat-col_pos       = posicao.
  APPEND st_fieldcat TO ti_fieldcat.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_APRESENTAR_ALV
*&---------------------------------------------------------------------*
* Apresentar o ALV
*----------------------------------------------------------------------*
FORM z_apresentar_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = c_z_top_of_page
      is_layout              = st_layout
      it_fieldcat            = ti_fieldcat
      it_events              = ti_events
      i_default              = c_x
      i_save                 = c_a
    TABLES
      t_outtab               = ti_dados
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_APRESENTAR_ALV
*&---------------------------------------------------------------------*
*&      Form  YF_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*  Topo do ALV
*----------------------------------------------------------------------*
FORM z_top_of_page .                                        "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = st_top_header.

ENDFORM.                    " YF_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONAR_EVENTOS
*&---------------------------------------------------------------------*
* Definir os eventos
*----------------------------------------------------------------------*
FORM z_selecionar_eventos .

* Variáveis
  DATA: lf_event TYPE slis_alv_event.

  CLEAR: ti_events.

* Obter eventos permitidos para lista grid
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = ti_events.

* Verificar se evento foi encontrado
  LOOP AT ti_events INTO lf_event.
    CASE lf_event-name.
      WHEN slis_ev_user_command.
        MOVE 'Z_USER_COMMAND' TO lf_event-form.
      WHEN slis_ev_pf_status_set.
        MOVE 'Z_PF_STATUS_SET' TO lf_event-form.
    ENDCASE.
    MODIFY ti_events FROM lf_event.
  ENDLOOP.
ENDFORM.                    " Z_SELECIONAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  Z_PF_STATUS_SET
*&---------------------------------------------------------------------*
* Especificar barra de status
*----------------------------------------------------------------------*
FORM z_pf_status_set USING pt_extab TYPE slis_t_extab.
* Acionar barra de comandos
  CLEAR it_fcode[].

  IF NOT ra2 IS INITIAL.
    wa_fcode = c_gravar.
    APPEND wa_fcode TO it_fcode.
    wa_fcode = c_editar.
    APPEND wa_fcode TO it_fcode.
    wa_fcode = c_simular.
    APPEND wa_fcode TO it_fcode.
    wa_fcode = c_gerar.
    APPEND wa_fcode TO it_fcode.
  ENDIF.

  SET PF-STATUS 'STANDARD' EXCLUDING it_fcode.
ENDFORM.                    "z_pf_status_set
*&---------------------------------------------------------------------*
*&      Form  Z_USER_COMMAND
*&---------------------------------------------------------------------*
* Ações do usuário *
*----------------------------------------------------------------------*
FORM z_user_command USING pv_ucomm LIKE sy-ucomm
                          pf_selfield TYPE slis_selfield.

  DATA: vg_tknum  TYPE tknum,
        vg_ebeln  TYPE ebeln,
        vg_fknum  TYPE fknum,
        tab_linha TYPE zftte_dados,
        gf_nfobjn LIKE j_1binterf-nfobjn.

  CASE pv_ucomm.
    WHEN c_editar.
      PERFORM z_apresentar_tela.
* Atualizar o ALV com os dados informados pelo usuário
      pf_selfield-refresh = c_x.
    WHEN c_gravar.
      PERFORM z_gravar_dados.
    WHEN c_simular.
      PERFORM z_simular_dados USING c_x.
    WHEN c_eventos.
      PERFORM z_mostra_eventos.
    WHEN c_gerar.
      PERFORM z_simular_dados USING space.
      PERFORM z_gerar_miro.
      pf_selfield-refresh = c_x.
    WHEN c_estornar.
      PERFORM z_estorno_miro_fiscal.
      pf_selfield-refresh = c_x.
    WHEN '&IC1'. "Selecionar detalhes
      CASE pf_selfield-fieldname.
        WHEN 'TKNUM'.
          "Deletar todos os dados diferntes da seleção
          IF NOT pf_selfield-value IS INITIAL.
            CLEAR: vg_tknum.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = pf_selfield-value
              IMPORTING
                output = vg_tknum.

            SET PARAMETER ID 'TNR' FIELD vg_tknum.
            CALL TRANSACTION  'VT03N' AND SKIP FIRST SCREEN.
          ELSE.
            MESSAGE e058(zles).
          ENDIF.
        WHEN 'EBELN'.
          IF NOT pf_selfield-value IS INITIAL.
            CLEAR: vg_ebeln.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = pf_selfield-value
              IMPORTING
                output = vg_ebeln.

            SET PARAMETER ID 'BES' FIELD vg_ebeln.
            CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
          ELSE.
            MESSAGE e061(zles).
          ENDIF.
        WHEN 'FKNUM'.
          IF NOT pf_selfield-value IS INITIAL.
            CLEAR: vg_fknum.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = pf_selfield-value
              IMPORTING
                output = vg_fknum.

            SET PARAMETER ID 'FKK' FIELD vg_fknum.
            CALL TRANSACTION  'VI03' AND SKIP FIRST SCREEN.
          ELSE.
            MESSAGE e061(zles).
          ENDIF.
        WHEN 'RE_BELNR'.
          IF NOT pf_selfield-value IS INITIAL.
            READ TABLE ti_dados INTO tab_linha INDEX pf_selfield-tabindex.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = tab_linha-re_belnr
              IMPORTING
                output = tab_linha-re_belnr.
            SET PARAMETER ID 'RBN' FIELD tab_linha-re_belnr.
            SET PARAMETER ID 'GJR' FIELD tab_linha-re_gjahr.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          ELSE.
            MESSAGE e059(zles).
          ENDIF.
        WHEN 'EN_DOCNUM'.
          IF NOT pf_selfield-value IS INITIAL.
            READ TABLE ti_dados INTO tab_linha INDEX pf_selfield-tabindex.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = tab_linha-en_docnum
              IMPORTING
                output = tab_linha-en_docnum.
            CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
              EXPORTING
                doc_number         = tab_linha-en_docnum
              IMPORTING
                obj_number         = gf_nfobjn
              EXCEPTIONS
                document_not_found = 1
                docum_lock         = 2
                OTHERS             = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
              EXPORTING
                obj_number         = gf_nfobjn
              EXCEPTIONS
                object_not_found   = 1
                scr_ctrl_not_found = 2
                OTHERS             = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ELSE.
            MESSAGE e060(zles).
          ENDIF.
        WHEN 'DOCNUM'.
          IF NOT pf_selfield-value IS INITIAL.
            READ TABLE ti_dados INTO tab_linha INDEX pf_selfield-tabindex.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT' "Converter o campo inteiro para char
              EXPORTING
                input  = tab_linha-docnum
              IMPORTING
                output = tab_linha-docnum.
            CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
              EXPORTING
                doc_number         = tab_linha-docnum
              IMPORTING
                obj_number         = gf_nfobjn
              EXCEPTIONS
                document_not_found = 1
                docum_lock         = 2
                OTHERS             = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
            CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
              EXPORTING
                obj_number         = gf_nfobjn
              EXCEPTIONS
                object_not_found   = 1
                scr_ctrl_not_found = 2
                OTHERS             = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ELSE.
            MESSAGE e060(zles).
          ENDIF.
      ENDCASE.

  ENDCASE.

ENDFORM.                    "z_user_command
*&---------------------------------------------------------------------*
*&      Form  Z_AGRUPAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_agrupar_dados .

  DATA: st_zlest0034 TYPE zlest0034,
        st_zlest0061 TYPE TABLE OF zlest0061,
        wl_zlest0061 TYPE zlest0061.

  DATA: it_imp_ret      TYPE TABLE OF zles0043_imp_ret WITH HEADER LINE,
        wa_imp_rettidos TYPE zles0043_imp_retidos.

  SORT ti_notas BY tknum.

  CLEAR: ti_eventos.

  CHECK ti_dados_tab[] IS NOT INITIAL.

  SELECT * INTO TABLE it_imp_ret
    FROM zles0043_imp_ret
     FOR ALL ENTRIES IN ti_dados_tab[]
   WHERE tknum EQ ti_dados_tab-tknum.

  SELECT * FROM zlest0061
    INTO TABLE st_zlest0061
    FOR ALL ENTRIES IN ti_dados_tab[]
  WHERE tknum EQ ti_dados_tab-tknum
    AND fknum EQ ti_dados_tab-fknum
    AND ck_anulado EQ abap_false.

  LOOP AT ti_dados_tab INTO st_dados_tab.
    CLEAR: st_dados, st_notas.
    MOVE-CORRESPONDING st_dados_tab TO st_dados.

* Obter o número da nota a partir do número do documento de transporte
    READ TABLE ti_notas INTO st_notas WITH KEY tknum = st_dados_tab-tknum
                        BINARY SEARCH.

    CLEAR: st_zlest0034.

* Obter os dados já gravados na tablea zlest0034.
    SELECT SINGLE * INTO st_zlest0034
                    FROM zlest0034
                   WHERE tknum = st_dados_tab-tknum.

    IF ( sy-subrc EQ 0 ) AND ( st_dados_tab-fknum = st_zlest0034-fknum ).

      IF ( st_zlest0034-nfenum IS INITIAL ) AND ( NOT st_notas IS INITIAL ).
        st_zlest0034-nfenum = st_notas-nfenum.
        st_zlest0034-docnum = st_notas-docnum.
        MODIFY zlest0034 FROM st_zlest0034.
      ENDIF.

      IF st_zlest0034-bukrs IS INITIAL.
        PERFORM busca_info_custo   USING st_zlest0034-fknum CHANGING st_zlest0034-bukrs st_zlest0034-werks st_zlest0034-waers st_zlest0034-kurst st_zlest0034-kbetr.
        MODIFY zlest0034 FROM st_zlest0034.
      ENDIF.

      IF st_zlest0034-re_belnr  IS INITIAL AND
         st_zlest0034-re_gjahr  IS INITIAL AND
         st_zlest0034-en_docnum IS INITIAL.

        IF st_zlest0034-fknum NE st_dados-fknum.
          PERFORM busca_info_custo   USING st_zlest0034-fknum CHANGING st_zlest0034-bukrs st_zlest0034-werks st_zlest0034-waers st_zlest0034-kurst st_zlest0034-kbetr.
        ENDIF.

        IF st_zlest0034-lblni NE st_dados-lblni.
          st_zlest0034-lblni = st_dados-lblni.
          PERFORM busca_ekpe USING st_zlest0034.
        ENDIF.

      ELSE.

        IF st_zlest0034-fknum NE st_dados-fknum.
          CLEAR st_eventos.
          st_eventos-icone = icon_led_red.
          st_eventos-type  = c_w.
          CONCATENATE 'Documento custo' st_zlest0034-fknum 'alterado p/' st_dados-fknum
                      '- estornar miro e fiscal!' INTO st_eventos-msg_text SEPARATED BY space.
          APPEND st_eventos TO ti_eventos.
        ENDIF.

        IF st_zlest0034-lblni NE st_dados-lblni.
          CLEAR st_eventos.
          st_eventos-icone = icon_led_red.
          st_eventos-type  = c_w.
          CONCATENATE 'Folha se Serviço' st_zlest0034-lblni 'alterado p/' st_dados-lblni
                      '- estornar miro e fiscal!' INTO st_eventos-msg_text SEPARATED BY space.
          APPEND st_eventos TO ti_eventos.
        ENDIF.

      ENDIF.

*      "Tipo de Transporte.
      CASE st_dados-vsart.
          "Aquaviario
        WHEN '03'.

          IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.
            READ TABLE st_zlest0061 INTO wl_zlest0061 WITH KEY tknum = st_dados-tknum fknum = st_dados-fknum.
            IF ( sy-subrc EQ 0 ) AND ( wl_zlest0061-waerk EQ 'USD').
              MOVE: wl_zlest0061-tax_dolar TO st_dados-tax_dolar.
            ENDIF.
          ENDIF.

      ENDCASE.

      MOVE:  st_zlest0034-nr_conhec   TO st_dados-nr_conhec,
             st_zlest0034-series      TO st_dados-series.

      MOVE: st_zlest0034-dmbtr        TO st_dados-dmbtr,
            st_zlest0034-dmbtr        TO st_dados-dmbtr_doc.

      MOVE: sy-datum                      TO st_dados-zdt_mov,
            st_zlest0034-zdt_vencto       TO st_dados-zdt_vencto,
            st_zlest0034-zdt_conhec       TO st_dados-zdt_conhec,
            st_zlest0034-zpeso_destino    TO st_dados-zpeso_destino,
            st_zlest0034-zdt_chegada      TO st_dados-zdt_chegada,
            st_zlest0034-zpeso_origem     TO st_dados-zpeso_origem,
            st_zlest0034-zpeso_diferenca  TO st_dados-zpeso_diferenca,
            st_zlest0034-zquebra          TO st_dados-zquebra,
            st_zlest0034-zperda           TO st_dados-zperda,
            st_zlest0034-zvlr_quebra      TO st_dados-zvlr_quebra,
            st_zlest0034-zvlr_perda       TO st_dados-zvlr_perda,
            st_zlest0034-zvlr_liq_pagar   TO st_dados-zvlr_liq_pagar,
            st_zlest0034-matnr            TO st_dados-matnr,
            st_zlest0034-matns            TO st_dados-matns,
            st_zlest0034-gewei            TO st_dados-gewei,
            st_zlest0034-re_belnr         TO st_dados-re_belnr,
            st_zlest0034-re_gjahr         TO st_dados-re_gjahr,
            st_zlest0034-en_docnum        TO st_dados-en_docnum,
            st_zlest0034-bukrs            TO st_dados-bukrs,
            st_zlest0034-werks            TO st_dados-werks,
            st_zlest0034-waers            TO st_dados-waers,
            st_zlest0034-kurst            TO st_dados-kurst,
            st_zlest0034-iva              TO st_dados-iva,
            st_zlest0034-bvtyp            TO st_dados-bvtyp,
            st_zlest0034-nfe              TO st_dados-nfe,
            st_zlest0034-multimodal       TO st_dados-multimodal,
            "ST_ZLEST0034-DMBTR            TO ST_DADOS-DMBTR,
            "ST_ZLEST0034-DMBTR_DOC        TO ST_DADOS-DMBTR_DOC,
            st_zlest0034-lfgja            TO st_dados-lfgja,
            st_zlest0034-lifnr            TO st_dados-lifnr,
            st_zlest0034-kalsm            TO st_dados-kalsm,
            st_zlest0034-regio_emissor    TO st_dados-regio_emissor,
            st_zlest0034-regio_receptor   TO st_dados-regio_receptor,
            st_zlest0034-base_icms        TO st_dados-base_icms,
            st_zlest0034-base_pis         TO st_dados-base_pis,
            st_zlest0034-base_cofins      TO st_dados-base_cofins,
            st_zlest0034-rate_icms        TO st_dados-rate_icms,
            st_zlest0034-rate_pis         TO st_dados-rate_pis,
            st_zlest0034-rate_cofins      TO st_dados-rate_cofins,
            st_zlest0034-valor_icms       TO st_dados-valor_icms,
            st_zlest0034-valor_pis        TO st_dados-valor_pis,
            st_zlest0034-valor_cofins     TO st_dados-valor_cofins,
            st_zlest0034-valor_pedagio    TO st_dados-valor_pedagio,
            st_zlest0034-nfenum           TO st_dados-nfenum,
            st_zlest0034-docnum           TO st_dados-docnum,
            st_zlest0034-valor_mercadoria TO st_dados-valor_mercadoria,
            st_dados_tab-add03            TO st_dados-add03.


    ELSE.

      MOVE: st_dados-tknum     TO st_zlest0034-tknum,
            st_dados-fknum     TO st_zlest0034-fknum,
            st_dados-ebeln     TO st_zlest0034-ebeln,
            st_dados-ebelp     TO st_zlest0034-ebelp,
            st_dados-lblni     TO st_zlest0034-lblni,
            st_dados-tdlnr     TO st_zlest0034-tdlnr,
            st_dados-shtyp     TO st_zlest0034-shtyp,
            st_dados-nfenum    TO st_zlest0034-nfenum,
            st_dados-lifnr     TO st_zlest0034-lifnr,
            'TAXBRA'           TO st_zlest0034-kalsm,
            st_dados_tab-add03 TO st_dados-add03.

      PERFORM busca_info_custo   USING st_zlest0034-fknum CHANGING st_zlest0034-bukrs st_zlest0034-werks st_zlest0034-waers st_zlest0034-kurst st_zlest0034-kbetr.
      PERFORM busca_info_remessa USING st_zlest0034-tknum CHANGING st_zlest0034-zpeso_origem st_zlest0034-gewei st_zlest0034-matnr.
      PERFORM busca_ekpe         USING st_zlest0034.

      IF NOT st_notas IS INITIAL.
        st_zlest0034-nfenum = st_notas-nfenum.
        st_zlest0034-docnum = st_notas-docnum.
      ENDIF.

      "Verifica Tipo de Transporte
      CASE st_dados-vsart.
          "Aquaviario
        WHEN: '03'.

          IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.
            READ TABLE st_zlest0061 INTO wl_zlest0061 WITH KEY tknum = st_dados-tknum
                                                               fknum = st_dados-fknum.
            IF ( sy-subrc EQ 0 ).
              CASE wl_zlest0061-waerk.
                WHEN: 'BRL'.
                  MOVE wl_zlest0061-vlr_brl TO st_zlest0034-dmbtr.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-vlr_brl.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-dmbtr.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-dmbtr_doc.
                WHEN: 'USD'.
                  MOVE wl_zlest0061-vlr_usd TO st_zlest0034-dmbtr.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-vlr_brl.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-dmbtr.
                  MOVE wl_zlest0061-vlr_brl TO st_dados-dmbtr_doc.
              ENDCASE.
            ENDIF.
          ELSE.
            MOVE: st_zlest0034-dmbtr        TO st_dados-dmbtr,
                  st_zlest0034-dmbtr        TO st_dados-dmbtr_doc.
          ENDIF.

        WHEN OTHERS.
          MOVE: st_zlest0034-dmbtr        TO st_dados-dmbtr,
                st_zlest0034-dmbtr        TO st_dados-dmbtr_doc.

      ENDCASE.


      MODIFY zlest0034 FROM st_zlest0034.

      MOVE: st_zlest0034-bukrs        TO st_dados-bukrs,
            st_zlest0034-werks        TO st_dados-werks,
            st_zlest0034-waers        TO st_dados-waers,
            st_zlest0034-kbetr        TO st_dados-kbetr,
            st_zlest0034-kurst        TO st_dados-kurst,
            "ST_ZLEST0034-DMBTR        TO ST_DADOS-DMBTR,
            "ST_ZLEST0034-DMBTR        TO ST_DADOS-DMBTR_DOC,
            st_zlest0034-lfgja        TO st_dados-lfgja,
            st_zlest0034-zpeso_origem TO st_dados-zpeso_origem,
            st_zlest0034-gewei        TO st_dados-gewei,
            st_zlest0034-matnr        TO st_dados-matnr,
            st_zlest0034-nfenum       TO st_dados-nfenum,
            st_zlest0034-docnum       TO st_dados-docnum,
            st_dados_tab-add03        TO st_dados-add03,
            st_zlest0034-zdt_vencto   TO st_dados-zdt_vencto,
            st_zlest0034-zdt_conhec   TO st_dados-zdt_conhec,
            st_zlest0034-docnum       TO st_dados-docnum.


    ENDIF.
* Obter a descrição do material
    CLEAR: st_dados-maktx, st_dados-makts.
    SELECT SINGLE maktx INTO st_dados-maktx FROM makt WHERE matnr = st_dados-matnr.
    SELECT SINGLE maktx INTO st_dados-makts FROM makt WHERE matnr = st_dados-matns.

    PERFORM verificar_status CHANGING st_dados.

    LOOP AT it_imp_ret WHERE tknum EQ st_dados-tknum.
      CLEAR: wa_imp_rettidos.
      MOVE-CORRESPONDING it_imp_ret TO wa_imp_rettidos.
      APPEND wa_imp_rettidos TO st_dados-it_impostos_retidos.
    ENDLOOP.

    IF NOT ra1 IS INITIAL.
      IF st_dados-status NE icon_green_light.
        APPEND st_dados TO ti_dados.
      ENDIF.
    ELSE.
      IF st_dados-status NE icon_red_light.
        APPEND st_dados TO ti_dados.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF NOT ti_eventos IS INITIAL.
    MESSAGE 'Favor verificar advertências em Eventos de Processamento!' TYPE 'I'.
  ENDIF.

ENDFORM.                    " Z_AGRUPAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_APRESENTAR_TELA
*&---------------------------------------------------------------------*
* Apresentar tela para que o usuário complete os dados do relatório ALV
*----------------------------------------------------------------------*
FORM z_apresentar_tela .

  DATA: ti_dados_aux TYPE TABLE OF zftte_dados,
        st_dados_aux TYPE zftte_dados,
        vl_indice    TYPE sy-tabix,
        vl_check     TYPE char1,
        wa_imp_ret   TYPE zles0043_imp_retidos,
        wa_imp_ret_2 TYPE zles0043_imp_retidos,
        wl_zlest0061 TYPE zlest0061.

  ti_dados_aux[] = ti_dados.
  DELETE ti_dados_aux WHERE box IS INITIAL.

* Processar somente quando houver linhas selecionadas
  IF ti_dados_aux[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-022.
    CHECK ti_dados_aux[] IS NOT INITIAL.
  ENDIF.

* Identificar o número de registros.
  DESCRIBE TABLE ti_dados_aux LINES vg_ultimo.

* Ficar em looping até todos os registros marcados pelo usuário sejam
* processados.
  vg_erro_s_n = c_n.
  vg_indice = 1.
  DO.
    IF vg_erro_s_n = c_n.
      vl_indice = vg_indice.
      READ TABLE ti_dados_aux INTO st_dados INDEX vg_indice.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING st_dados TO st_tela.
      CLEAR: st_tela-it_impostos_retidos[].
      MOVE st_dados-it_impostos_retidos[] TO st_tela-it_impostos_retidos[].
* Retirar zeros a esquerda
      PERFORM z_converter_campo CHANGING st_tela-tdlnr.
      PERFORM z_converter_campo CHANGING st_tela-tknum.
      PERFORM z_converter_campo CHANGING st_tela-fknum.
      PERFORM z_converter_campo CHANGING st_tela-matnr.

      "Cópia de Impostos Retidos do Registro anterior agrupado. *********************************
      IF ( st_dados-it_impostos_retidos[] IS INITIAL ) AND ( vg_indice GT 1 ).
        vg_indice = vg_indice - 1.
        READ TABLE ti_dados_aux INTO st_dados_aux INDEX vg_indice.
        vg_indice = vg_indice + 1.
        IF NOT st_dados_aux-it_impostos_retidos[] IS INITIAL.
          MOVE st_dados_aux-it_impostos_retidos[] TO st_dados-it_impostos_retidos[].
          LOOP AT st_dados-it_impostos_retidos INTO wa_imp_ret.
            wa_imp_ret-tknum      = st_dados-tknum.
            wa_imp_ret-bukrs      = st_dados-bukrs.
            wa_imp_ret-lifnr      = st_dados-lifnr.
            wa_imp_ret-base       = CONV #( st_dados-dmbtr ).
            wa_imp_ret-taxval     = st_dados-dmbtr * ( wa_imp_ret-rate / 100 ).
            MODIFY st_dados-it_impostos_retidos INDEX sy-tabix FROM wa_imp_ret.
          ENDLOOP.
          MOVE st_dados-it_impostos_retidos[] TO st_tela-it_impostos_retidos[].
          vg_alterou = c_x.
        ENDIF.
      ENDIF.
      "******************************************************************************************
      "******************************************************************************************
    ENDIF.

    PERFORM carrega_parametros USING st_tela-matnr CHANGING pc_tolerancia.

* Apresentar tela de inclusãode dados.
    CLEAR vg_okcode.
    CALL SCREEN 100 STARTING AT 10 02.

* Encerrar o looping se o usuário cancelar
    IF vg_okcode = c_cancelar.
      EXIT.
    ENDIF.

    CLEAR: vl_check.

    "*** Verifica alterações de impostos retidos ******************************************************************
    IF ( st_tela-it_impostos_retidos[] IS INITIAL ) AND ( NOT st_dados-it_impostos_retidos[] IS INITIAL ).
      CLEAR: st_dados-it_impostos_retidos[].
      vg_alterou = c_x.
    ENDIF.

    LOOP AT st_dados-it_impostos_retidos INTO wa_imp_ret_2.
      CLEAR: wa_imp_ret_2-mark.
      MODIFY st_dados-it_impostos_retidos INDEX sy-tabix FROM wa_imp_ret_2 TRANSPORTING mark.
    ENDLOOP.

    LOOP AT st_tela-it_impostos_retidos INTO wa_imp_ret.
      READ TABLE st_dados-it_impostos_retidos INTO wa_imp_ret_2 WITH KEY witht     = wa_imp_ret-witht
                                                                         wt_withcd = wa_imp_ret-wt_withcd.
      IF sy-subrc IS INITIAL.
        IF ( wa_imp_ret_2-bukrs  NE wa_imp_ret-bukrs  ) OR
           ( wa_imp_ret_2-lifnr  NE wa_imp_ret-lifnr  ) OR
           ( wa_imp_ret_2-base   NE wa_imp_ret-base   ) OR
           ( wa_imp_ret_2-rate   NE wa_imp_ret-rate   ) OR
           ( wa_imp_ret_2-taxval NE wa_imp_ret-taxval ).
          CLEAR: wa_imp_ret_2.
          MOVE-CORRESPONDING wa_imp_ret TO wa_imp_ret_2.
          MODIFY st_dados-it_impostos_retidos INDEX sy-tabix FROM wa_imp_ret_2.
        ELSE.
          wa_imp_ret_2-mark = c_x.
          MODIFY st_dados-it_impostos_retidos INDEX sy-tabix FROM wa_imp_ret_2 TRANSPORTING mark.
        ENDIF.
      ELSE.
        wa_imp_ret-tknum = st_dados-tknum.
        wa_imp_ret-mark  = c_x.
        APPEND wa_imp_ret TO st_dados-it_impostos_retidos.
        vg_alterou = c_x.
      ENDIF.
    ENDLOOP.
    DELETE st_dados-it_impostos_retidos WHERE mark EQ space.
    MODIFY ti_dados_aux FROM st_dados INDEX vl_indice TRANSPORTING it_impostos_retidos.
    "**************************************************************************************************************
    "**************************************************************************************************************

* Mover os dados informados pelo usuário para tabela interna e atualizá-la.
    IF st_dados-zdt_mov       NE vg_data_mov           OR
       st_dados-zdt_vencto    NE vg_data_vencto        OR
       st_dados-nr_conhec     NE st_tela-nr_conhec     OR
       st_dados-series        NE st_tela-series        OR
       st_dados-zdt_conhec    NE vg_data_conhec        OR
       st_dados-zdt_chegada   NE vg_data_chegada       OR
       st_dados-zpeso_destino NE vg_peso_destino       OR
       st_dados-iva           NE vg_iva                OR
       st_dados-nfe           NE vg_nfe                OR
       st_dados-multimodal    NE vg_multimodal         OR
       st_dados-matns         NE zlest0034-matns       OR
       st_dados-bvtyp         NE info_forne-bvtyp      OR
       st_dados-dmbtr_doc     NE st_tela-dmbtr_doc     OR
       st_dados-valor_pedagio NE st_tela-valor_pedagio.

      st_tela-matns          = zlest0034-matns.
      st_dados-zdt_mov       = vg_data_mov.
      st_dados-zdt_vencto    = vg_data_vencto.
      st_dados-nr_conhec     = st_tela-nr_conhec.
      st_dados-series        = st_tela-series.
      st_dados-zdt_conhec    = vg_data_conhec.
      st_dados-zdt_chegada   = vg_data_chegada.
      st_dados-zpeso_destino = vg_peso_destino.
      st_dados-iva           = vg_iva.
      st_dados-nfe           = vg_nfe.
      st_dados-multimodal    = vg_multimodal.
      st_dados-matns         = st_tela-matns.
      st_dados-bvtyp         = info_forne-bvtyp.
      "ST_DADOS-DMBTR_DOC     = ST_TELA-DMBTR_DOC.
      st_dados-valor_pedagio = st_tela-valor_pedagio.

      CLEAR: wl_zlest0061.
      SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE tknum EQ st_dados-tknum
                                                         AND fknum EQ st_dados-fknum
                                                         AND ck_anulado EQ abap_false.

      IF ( sy-subrc EQ 0 ).

        CASE wl_zlest0061-waerk.
          WHEN: 'BRL'.
            MOVE wl_zlest0061-vlr_brl TO st_dados-dmbtr_doc.
          WHEN: 'USD'.
            MOVE wl_zlest0061-vlr_usd  TO st_dados-dmbtr_doc.
        ENDCASE.

      ELSE.
        st_dados-dmbtr_doc     = st_tela-dmbtr_doc.
      ENDIF.


      IF NOT st_tela-matns IS INITIAL.
        SELECT SINGLE maktg INTO st_dados-makts
          FROM makt
         WHERE matnr EQ st_tela-matns
           AND spras EQ sy-langu.
        IF sy-subrc IS  NOT INITIAL.
          SELECT SINGLE asktx INTO st_dados-makts
            FROM asmdt
           WHERE asnum EQ st_tela-matns      "#EC CI_FLDEXT_OK[2215424]
             AND spras EQ sy-langu.
        ENDIF.
      ENDIF.


      PERFORM calcular_movimento CHANGING st_dados.

      MODIFY ti_dados_aux FROM  st_dados INDEX vl_indice
                          TRANSPORTING zdt_mov
                                       zdt_vencto
                                       nr_conhec
                                       series
                                       zdt_conhec
                                       zpeso_destino
                                       zdt_chegada
                                       zpeso_diferenca
                                       zquebra
                                       zperda
                                       zvlr_quebra
                                       zvlr_perda
                                       zvlr_liq_pagar
                                       iva
                                       nfe
                                       matns
                                       bvtyp
                                       dmbtr_doc
                                       regio_emissor
                                       txjcd_emissor
                                       regio_receptor
                                       base_icms
                                       base_pis
                                       base_cofins
                                       rate_icms
                                       rate_pis
                                       rate_cofins
                                       valor_icms
                                       valor_pis
                                       valor_cofins
                                       valor_pedagio
                                       valor_mercadoria
                                       multimodal.
      vg_alterou = c_x.
    ENDIF.

* Encerrar o looping se o usuário confirmar
    IF vg_okcode = c_confirmar.
      EXIT.
    ENDIF.

  ENDDO.

* Atualizar o registro no ALV somente quando o usuário confirmar.
  CHECK vg_okcode = c_confirmar.

* Mover os dados informados pelo usuário para tabela utilizada pelo ALV
  LOOP AT ti_dados_aux INTO st_dados.
* Obter o ponteiro do registro na tabela
*    READ TABLE TI_DADOS
*      WITH KEY TDLNR = ST_DADOS-TDLNR
*               TKNUM = ST_DADOS-TKNUM
*      BINARY SEARCH
*      TRANSPORTING NO FIELDS.

    IF vl_check IS INITIAL.
*     Atualizar a tabela
      LOOP AT ti_dados INTO st_dados_aux WHERE tdlnr EQ st_dados-tdlnr AND tknum EQ st_dados-tknum.

        MODIFY ti_dados FROM st_dados INDEX sy-tabix
                        TRANSPORTING zdt_mov
                                     zdt_vencto
                                     nr_conhec
                                     series
                                     zdt_conhec
                                     zpeso_destino
                                     zdt_chegada
                                     zpeso_diferenca
                                     zquebra
                                     zperda
                                     zvlr_quebra
                                     zvlr_perda
                                     zvlr_liq_pagar
                                     iva
                                     nfe
                                     matns
                                     bvtyp
                                     dmbtr_doc
                                     regio_emissor
                                     txjcd_emissor
                                     regio_receptor
                                     base_icms
                                     base_pis
                                     base_cofins
                                     rate_icms
                                     rate_pis
                                     rate_cofins
                                     valor_icms
                                     valor_pis
                                     valor_cofins
                                     valor_pedagio
                                     valor_mercadoria
                                     multimodal
                                     it_impostos_retidos.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " Z_APRESENTAR_TELA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA ti_okcode TYPE TABLE OF sy-ucomm.
  CLEAR ti_okcode.

* Excluir os botões ANTERIOR e PROXIMO, pois so há um registro.
  IF vg_ultimo = 1.
    APPEND c_anterior TO ti_okcode.
    APPEND c_proximo  TO ti_okcode.
    SET PF-STATUS 'STATUS_0100' EXCLUDING ti_okcode.
  ELSEIF vg_indice = 1.
* Excluir o botão ANTERIOR quando for o primeiro registro.
    APPEND c_anterior TO ti_okcode.
    SET PF-STATUS 'STATUS_0100' EXCLUDING ti_okcode.
  ELSEIF vg_indice = vg_ultimo.
* Excluir o botão PROXIMO quando for o último registro.
    APPEND c_proximo TO ti_okcode.
    SET PF-STATUS 'STATUS_0100' EXCLUDING ti_okcode.
  ELSE.
* Apresentar todos os botões
    SET PF-STATUS 'STATUS_0100'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: wa_impostos_retidos TYPE zles0043_imp_retidos,
        vg_validado         TYPE sy-subrc.

  PERFORM verificar_multimodal USING vg_validado.

  CHECK vg_validado IS INITIAL.

  CASE vg_okcode.

    WHEN c_anterior.
      PERFORM z_validar_tela.
      IF vg_erro_s_n = c_n.
        SUBTRACT 1 FROM vg_indice.
      ENDIF.

    WHEN c_proximo.
      PERFORM z_validar_tela.
      IF vg_erro_s_n = c_n.
        ADD 1 TO vg_indice.
      ENDIF.

    WHEN c_confirmar.
      PERFORM z_validar_tela.

    WHEN c_impostos_r.

      MOVE-CORRESPONDING st_dados TO wa_impostos_retidos.
      wa_impostos_retidos-base = CONV #( st_tela-dmbtr_doc ).
      IF st_dados-re_belnr IS INITIAL.
        PERFORM z_impostos_retidos TABLES st_tela-it_impostos_retidos USING wa_impostos_retidos space.
      ELSE.
        PERFORM z_impostos_retidos TABLES st_tela-it_impostos_retidos USING wa_impostos_retidos c_x.
      ENDIF.
      EXIT.

    WHEN OTHERS.
      PERFORM z_validar_tela.

  ENDCASE.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_CONVERTER_CAMPO
*&---------------------------------------------------------------------*
*  Converter o valor a ser apresentado na tela.
*----------------------------------------------------------------------*
FORM z_converter_campo  CHANGING p_campo.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_campo
    IMPORTING
      output = p_campo.

ENDFORM.                    " Z_CONVERTER_CAMPO
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDAR_TELA
*&---------------------------------------------------------------------*
* Validar as datas da tela.
*----------------------------------------------------------------------*
FORM z_validar_tela .

  DATA vl_data TYPE sy-datum.
*-CS1084034-#RIMINI-05.02.2023-BEGIN
  IF vg_peso_destino IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF '.' IN vg_peso_destino WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN vg_peso_destino WITH '.'.
  ENDIF.
*-CS1084034-#RIMINI-05.02.2023-END
  CLEAR: vg_data_mov,
         vg_data_vencto,
         vg_data_conhec,
         vg_data_chegada.

  vg_erro_s_n = c_n.

  IF vg_dt_mov IS NOT INITIAL.
    PERFORM z_consistir_data USING vg_dt_mov TEXT-014
                             CHANGING vl_data.
    CHECK vg_erro_s_n = c_n.
    vg_data_mov = vl_data.
  ENDIF.

  IF vg_dt_vencto IS NOT INITIAL.
    PERFORM z_consistir_data USING vg_dt_vencto TEXT-015
                             CHANGING vl_data.
    CHECK vg_erro_s_n = c_n.
    vg_data_vencto = vl_data.
  ENDIF.

  IF vg_dt_conhec IS NOT INITIAL.
    PERFORM z_consistir_data USING vg_dt_conhec TEXT-016
                             CHANGING vl_data.
    CHECK vg_erro_s_n = c_n.
    vg_data_conhec = vl_data.
  ENDIF.

  IF vg_dt_chegada IS NOT INITIAL.
    PERFORM z_consistir_data USING vg_dt_chegada TEXT-017
                             CHANGING vl_data.
    CHECK vg_erro_s_n = c_n.
    vg_data_chegada = vl_data.
  ENDIF.
*-CS1084034-#RIMINI-05.02.2023-BEGIN
*  IF vg_peso_destino IS NOT INITIAL.
*    REPLACE ALL OCCURRENCES OF '.' IN vg_peso_destino WITH ''.
*    REPLACE ALL OCCURRENCES OF ',' IN vg_peso_destino WITH '.'.
*  ENDIF.
*-CS1084034-#RIMINI-05.02.2023-END
ENDFORM.                    " Z_VALIDAR_TELA
*&---------------------------------------------------------------------*
*&      Form  Z_CONSISTIR_DATA
*&---------------------------------------------------------------------*
* Executar a função que valida uma data.
*----------------------------------------------------------------------*
FORM z_consistir_data  USING    p_data
                                p_texto
                       CHANGING p_data_conv.

  DATA vl_data TYPE sy-datum.
  CLEAR vl_data.

* Retirar caracters não numéricos da data como . ou /
  REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_data  WITH ''.

* Colocar a dagta no formato AAAAMMDD.
  CONCATENATE p_data+4(4)
              p_data+2(2)
              p_data+0(2) INTO vl_data.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = vl_data
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    MESSAGE w000(zles) WITH p_texto TEXT-018.
    vg_erro_s_n = 'S'.
  ELSE.
    p_data_conv = vl_data.
  ENDIF.
*-CS1084034-#RIMINI-05.02.2023-BEGIN
  CHECK vg_erro_s_n NE 'S'.
*- Tratamento Data do Documento não deve ser maior quer data de lançamento
*  e ano inferior a 1970
  IF p_texto EQ TEXT-016.  "Data de conhecimento
    IF p_data_conv GT sy-datum OR p_data_conv(4) LT '1970'.
      MESSAGE w000(zles) WITH p_texto TEXT-044 TEXT-045.
      vg_erro_s_n = 'S'.
    ENDIF.
  ENDIF.
*-CS1084034-#RIMINI-05.02.2023-END
ENDFORM.                    " Z_CONSISTIR_DATA
*&---------------------------------------------------------------------*
*&      Module  CAMPOS_AUXILIARES_TELA  OUTPUT
*&---------------------------------------------------------------------*
* Preencher os campos auxiliares da tela
*----------------------------------------------------------------------*
MODULE campos_auxiliares_tela OUTPUT.

  DATA : vg_iva_aux   TYPE mwskz,
         vg_mul_aux   TYPE char01,
         vg_nfe_aux   TYPE j_1bnfe,
         vg_pro_aux   TYPE matnr,
         vg_venc_aux  LIKE zlest0034-zdt_vencto,
         wa_zlest0044 TYPE zlest0044.

  vg_iva_aux  = vg_iva.
  vg_nfe_aux  = vg_nfe.
  vg_mul_aux  = vg_multimodal.
  vg_pro_aux  = zlest0034-matns.
  vg_venc_aux = zlest0034-zdt_vencto.

  CLEAR: vg_dt_mov,
         vg_dt_vencto,
         st_tela-nr_conhec,
         vg_dt_conhec,
         vg_dt_chegada,
         vg_peso_destino,
         vg_iva,
         vg_nfe,
         vg_multimodal,
         zlest0034-matns,
         vg_produto_nome,
         vg_ferro_novo.

  IF ( st_dados-vsart EQ '02' ).

    SELECT SINGLE * INTO wa_zlest0044
      FROM zlest0044
     WHERE nr_trans EQ st_dados-tknum.

    IF sy-subrc IS INITIAL.
      IF  st_dados-nr_conhec IS INITIAL.
        st_tela-dmbtr_doc      = wa_zlest0044-vlr_rec.
        st_dados-zpeso_destino = wa_zlest0044-peso_bruto.
        CONCATENATE wa_zlest0044-data_emisao+6(2) '.' wa_zlest0044-data_emisao+4(2) '.' wa_zlest0044-data_emisao(4) INTO vg_dt_chegada.
        st_tela-nr_conhec      = wa_zlest0044-nr_cte.
        st_tela-series         = wa_zlest0044-serie.
        CONCATENATE wa_zlest0044-data_emisao+6(2) '.' wa_zlest0044-data_emisao+4(2) '.' wa_zlest0044-data_emisao(4) INTO vg_dt_conhec.
        CONCATENATE wa_zlest0044-dt_venc+6(2)     '.' wa_zlest0044-dt_venc+4(2)     '.' wa_zlest0044-dt_venc(4)     INTO vg_dt_vencto.
      ENDIF.
      vg_nfe        = 'X'.
      vg_ferro_novo = 'X'.
    ENDIF.

  ENDIF.

  IF NOT st_dados-bvtyp IS INITIAL.
    info_forne-bvtyp = st_dados-bvtyp.
  ENDIF.

  PERFORM busca_dados_forne USING st_dados-lifnr info_forne.

  IF st_dados-zdt_mov IS NOT INITIAL.
    WRITE st_dados-zdt_mov       TO vg_dt_mov.
  ELSE.
    WRITE sy-datum TO vg_dt_mov.
  ENDIF.

  IF st_dados-nr_conhec IS NOT INITIAL.
    WRITE st_dados-nr_conhec    TO st_tela-nr_conhec.
  ENDIF.

  IF st_dados-zdt_conhec IS NOT INITIAL.
    WRITE st_dados-zdt_conhec    TO vg_dt_conhec.
  ENDIF.

  IF st_dados-zdt_chegada IS NOT INITIAL.
    WRITE st_dados-zdt_chegada   TO vg_dt_chegada.
  ENDIF.

  IF st_dados-zpeso_origem IS NOT INITIAL.
    MOVE st_dados-zpeso_origem TO st_tela-zpeso_origem.
  ENDIF.

  IF st_dados-iva IS NOT INITIAL.
    WRITE st_dados-iva TO vg_iva.
  ELSE.
    WRITE vg_iva_aux   TO vg_iva.
  ENDIF.

  IF st_dados-zdt_vencto IS NOT INITIAL.
    WRITE st_dados-zdt_vencto TO vg_dt_vencto.
  ELSE.
    IF vg_venc_aux IS NOT INITIAL.
      WRITE vg_venc_aux TO vg_dt_vencto.
    ENDIF.
  ENDIF.

  IF st_dados-nfe IS NOT INITIAL.
    WRITE st_dados-nfe TO vg_nfe.
  ELSE.
    IF ( vg_nfe_aux IS INITIAL ) AND ( vg_ferro_novo IS INITIAL ).
      WRITE vg_nfe_aux   TO vg_nfe.
    ENDIF.
  ENDIF.

  IF st_dados-multimodal IS NOT INITIAL.
    WRITE st_dados-multimodal TO vg_multimodal.
  ELSE.
    IF ( vg_ferro_novo IS INITIAL ).
      WRITE vg_mul_aux   TO vg_multimodal.
    ENDIF.
  ENDIF.

  IF st_dados-matns IS NOT INITIAL.
    WRITE st_dados-matns TO zlest0034-matns.
  ELSE.
    WRITE vg_pro_aux   TO zlest0034-matns.
  ENDIF.

  IF zlest0034-matns IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zlest0034-matns
      IMPORTING
        output = zlest0034-matns.

    SELECT SINGLE maktg INTO vg_produto_nome
      FROM makt
     WHERE matnr EQ zlest0034-matns
       AND spras EQ sy-langu.

    "Caso não seja um material buscar serviço
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE asktx INTO vg_produto_nome
        FROM asmdt
       WHERE asnum EQ zlest0034-matns        "#EC CI_FLDEXT_OK[2215424]
         AND spras EQ sy-langu.
    ENDIF.

  ENDIF.

  WRITE st_dados-zpeso_destino TO vg_peso_destino.

  IF NOT vg_iva IS INITIAL.
    SELECT SINGLE text1 INTO tx_iva
      FROM t007s
     WHERE mwskz EQ vg_iva
       AND spras EQ sy-langu.
  ENDIF.

ENDMODULE.                 " CAMPOS_AUXILIARES_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  Z_INVERTER_DATA
*&---------------------------------------------------------------------*
* Inverter a data para o formato AAMMDD.
*----------------------------------------------------------------------*
FORM z_inverter_data USING p_data
                     CHANGING vg_data.


ENDFORM.                    " Z_INVERTER_DATA
*&---------------------------------------------------------------------*
*&      Form  Z_GRAVAR_DADOS
*&---------------------------------------------------------------------*
* Gravar os dados no banco de dados
*----------------------------------------------------------------------*
FORM z_gravar_dados .

  TYPES: BEGIN OF y_zlest0034,
           zdt_mov          TYPE zlest0034-zdt_mov,
           zdt_vencto       TYPE zlest0034-zdt_vencto,
           nr_conhec        TYPE zlest0034-nr_conhec,
           series           TYPE zlest0034-series,
           zdt_conhec       TYPE zlest0034-zdt_conhec,
           zpeso_destino    TYPE zlest0034-zpeso_destino,
           zdt_chegada      TYPE zlest0034-zdt_chegada,
           iva              TYPE zlest0034-iva,
           nfe              TYPE zlest0034-nfe,
           matns            TYPE zlest0034-matns,
           bvtyp            TYPE zlest0034-bvtyp,             "Banco Pareiro
           dmbtr_doc        TYPE zlest0034-dmbtr_doc,         "Valor Documento
           regio_emissor    TYPE zlest0034-regio_emissor,
           txjcd_emissor    TYPE zlest0034-txjcd_emissor,
           regio_receptor   TYPE zlest0034-regio_receptor,
           base_icms        TYPE zlest0034-base_icms,
           base_pis         TYPE zlest0034-base_pis,
           base_cofins      TYPE zlest0034-base_cofins,
           rate_icms        TYPE zlest0034-rate_icms,
           rate_pis         TYPE zlest0034-rate_pis,
           rate_cofins      TYPE zlest0034-rate_cofins,
           valor_icms       TYPE zlest0034-valor_icms,
           valor_pis        TYPE zlest0034-valor_pis,
           valor_cofins     TYPE zlest0034-valor_cofins,
           valor_pedagio    TYPE zlest0034-valor_pedagio,
           valor_mercadoria TYPE zlest0034-valor_mercadoria,
           multimodal       TYPE zlest0034-multimodal,
         END OF y_zlest0034.

  DATA: st_zlest0034        TYPE y_zlest0034,
        st_zlest0034_aux    TYPE zlest0034,
        ti_zlest0034        TYPE TABLE OF zlest0034,
        ti_dados_aux        TYPE TABLE OF zftte_dados,
        st_dados_aux        TYPE zftte_dados,
        vl_tabix            TYPE sy-tabix,
        ti_tab_imp          TYPE TABLE OF zles0043_imp_ret WITH HEADER LINE,
        wa_zles0043_imp_ret TYPE zles0043_imp_ret,
        st_tab_imp          TYPE zles0043_imp_retidos,
        vg_imp_ret          TYPE c LENGTH 1,
        vg_linha1           TYPE i,
        vg_linha2           TYPE i.

  FIELD-SYMBOLS: <fs_zlest0034> TYPE zlest0034.

  DATA: wl_zlest0061 TYPE zlest0061.

  ti_dados_aux[] = ti_dados[].
  DELETE ti_dados_aux WHERE box IS INITIAL.

* Processar somente quando houver linhas selecionadas
  IF ti_dados_aux[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-022.
    CHECK ti_dados_aux[] IS NOT INITIAL.
  ENDIF.

* Verificar se alguma linha selecionada foi modificada.
  LOOP AT ti_dados_aux INTO st_dados_aux.

    vl_tabix = sy-tabix.

    CLEAR: ti_tab_imp[], vg_imp_ret.

    SELECT SINGLE zdt_mov          zdt_vencto
                  nr_conhec        series
                  zdt_conhec       zpeso_destino
                  zdt_chegada      iva
                  nfe              matns
                  bvtyp            dmbtr_doc
                  regio_emissor    txjcd_emissor
                  regio_receptor
                  base_icms        base_pis
                  base_cofins      rate_icms
                  rate_pis         rate_cofins
                  valor_icms       valor_pis
                  valor_cofins     valor_pedagio
                  valor_mercadoria multimodal
            INTO st_zlest0034
            FROM zlest0034
           WHERE tknum = st_dados_aux-tknum.

    CHECK sy-subrc EQ 0.

    SELECT * INTO TABLE ti_tab_imp
      FROM zles0043_imp_ret
     WHERE tknum = st_dados_aux-tknum.

    DESCRIBE TABLE ti_tab_imp LINES vg_linha1.
    DESCRIBE TABLE st_dados_aux-it_impostos_retidos LINES vg_linha2.

    IF vg_linha1 NE vg_linha2.
      vg_imp_ret = c_x.
    ENDIF.

    IF vg_imp_ret IS INITIAL.
      LOOP AT st_dados_aux-it_impostos_retidos INTO st_tab_imp.
        READ TABLE ti_tab_imp WITH KEY witht     = ti_tab_imp-witht
                                       wt_withcd = ti_tab_imp-wt_withcd.
        IF NOT sy-subrc IS INITIAL.
          vg_imp_ret = c_x.
        ELSE.
          IF ( st_tab_imp-bukrs  NE ti_tab_imp-bukrs  ) OR
             ( st_tab_imp-lifnr  NE ti_tab_imp-lifnr  ) OR
             ( st_tab_imp-base   NE ti_tab_imp-base   ) OR
             ( st_tab_imp-rate   NE ti_tab_imp-rate   ) OR
             ( st_tab_imp-taxval NE ti_tab_imp-taxval ).
            vg_imp_ret = c_x.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF vg_imp_ret EQ c_x.
      CONTINUE.
    ENDIF.

    IF st_dados_aux-zdt_mov        EQ st_zlest0034-zdt_mov        AND
       st_dados_aux-zdt_vencto     EQ st_zlest0034-zdt_vencto     AND
       st_dados_aux-nr_conhec      EQ st_zlest0034-nr_conhec      AND
       st_dados_aux-series         EQ st_zlest0034-series         AND
       st_dados_aux-zdt_conhec     EQ st_zlest0034-zdt_conhec     AND
       st_dados_aux-zpeso_destino  EQ st_zlest0034-zpeso_destino  AND
       st_dados_aux-zdt_chegada    EQ st_zlest0034-zdt_chegada    AND
       st_dados_aux-iva            EQ st_zlest0034-iva            AND
       st_dados_aux-nfe            EQ st_zlest0034-nfe            AND
       st_dados_aux-matns          EQ st_zlest0034-matns          AND
       st_dados_aux-bvtyp          EQ st_zlest0034-bvtyp          AND
       st_dados_aux-dmbtr_doc      EQ st_zlest0034-dmbtr_doc      AND
       st_dados_aux-regio_emissor  EQ st_zlest0034-regio_emissor  AND
       st_dados_aux-txjcd_emissor  EQ st_zlest0034-txjcd_emissor  AND
       st_dados_aux-regio_receptor EQ st_zlest0034-regio_receptor AND
       st_dados_aux-base_icms      EQ st_zlest0034-base_icms      AND
       st_dados_aux-base_pis       EQ st_zlest0034-base_pis       AND
       st_dados_aux-base_cofins    EQ st_zlest0034-base_cofins    AND
       st_dados_aux-rate_icms      EQ st_zlest0034-rate_icms      AND
       st_dados_aux-rate_pis       EQ st_zlest0034-rate_pis       AND
       st_dados_aux-rate_cofins    EQ st_zlest0034-rate_cofins    AND
       st_dados_aux-valor_icms     EQ st_zlest0034-valor_icms     AND
       st_dados_aux-valor_pis      EQ st_zlest0034-valor_pis      AND
       st_dados_aux-valor_cofins   EQ st_zlest0034-valor_cofins   AND
       st_dados_aux-valor_pedagio  EQ st_zlest0034-valor_pedagio  AND
       st_dados_aux-valor_mercadoria EQ st_zlest0034-valor_mercadoria AND
       st_dados_aux-multimodal       EQ st_zlest0034-multimodal.
      DELETE ti_dados_aux INDEX vl_tabix.
    ENDIF.
  ENDLOOP.

* Apresentar mensagem de erro quando não houver nenhuma modificação
* realizda pelo usuário em nenhuma linha do ALV.
  IF ti_dados_aux[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-020 TEXT-021.
    CHECK 1 = 2.
  ENDIF.

* Gravar a tabela interna que conterá os registros que serão inseridos ou
* modificados.
  LOOP AT ti_dados_aux INTO st_dados_aux.
    MOVE-CORRESPONDING st_dados_aux TO st_zlest0034_aux.
    st_zlest0034_aux-mandt = sy-mandt.
    APPEND st_zlest0034_aux TO ti_zlest0034.
    DELETE FROM zles0043_imp_ret WHERE tknum EQ st_dados_aux-tknum.
    LOOP AT st_dados_aux-it_impostos_retidos INTO st_tab_imp.
      CLEAR wa_zles0043_imp_ret.
      MOVE-CORRESPONDING st_tab_imp TO wa_zles0043_imp_ret.
      INSERT zles0043_imp_ret FROM wa_zles0043_imp_ret.
    ENDLOOP.
  ENDLOOP.


  LOOP AT ti_zlest0034 ASSIGNING <fs_zlest0034>.
    SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE tknum EQ <fs_zlest0034>-tknum
                                                       AND fknum EQ <fs_zlest0034>-fknum
                                                       AND ck_anulado EQ abap_false.
    IF ( sy-subrc EQ 0 ).



      CASE wl_zlest0061-waerk.
        WHEN: 'BRL'.
          MOVE wl_zlest0061-vlr_brl TO <fs_zlest0034>-dmbtr.
          MOVE wl_zlest0061-vlr_brl TO <fs_zlest0034>-dmbtr_doc.
        WHEN: 'USD'.
          MOVE wl_zlest0061-vlr_usd TO <fs_zlest0034>-dmbtr.
          MOVE wl_zlest0061-vlr_usd  TO <fs_zlest0034>-dmbtr_doc.
      ENDCASE.


    ENDIF.
  ENDLOOP.
  UNASSIGN <fs_zlest0034>.

* Atualizar os dados que estavam no ALV no banco de dados
  MODIFY zlest0034 FROM TABLE ti_zlest0034.

  IF sy-subrc EQ 0.
    CLEAR: vg_alterou.
    COMMIT WORK.
    MESSAGE s000(zles) WITH TEXT-023.
  ENDIF.

* Desmarcar os registros processados.
  LOOP AT ti_dados_aux INTO st_dados_aux.
    vl_tabix = sy-tabix.
    READ TABLE ti_dados INTO st_dados WITH KEY tdlnr = st_dados_aux-tdlnr
                                               tknum = st_dados_aux-tknum
                                      BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    CLEAR st_dados-box.
    MODIFY ti_dados FROM st_dados INDEX vl_tabix TRANSPORTING box.
  ENDLOOP.

ENDFORM.                    " Z_GRAVAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  CARREGA_PARAMETROS
*&---------------------------------------------------------------------*
*       Busca parâmetros de tolerância de frete
*----------------------------------------------------------------------*
FORM carrega_parametros  USING vg_matnr TYPE matnr
                         CHANGING pr_que TYPE p.

  DATA: wa_a912   TYPE a912,
        wa_konp   TYPE konp,
        vg_matnr2 TYPE matnr.

  pr_que = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vg_matnr
    IMPORTING
      output = vg_matnr2.

  SELECT SINGLE * INTO wa_a912 FROM a912 WHERE kschl EQ c_zmrg AND matnr EQ vg_matnr2.

  IF sy-subrc EQ 0.

    SELECT SINGLE * INTO wa_konp FROM konp WHERE knumh EQ wa_a912-knumh.
    IF sy-subrc EQ 0.
      pr_que = wa_konp-kbetr / 10.
    ENDIF.

  ENDIF.

ENDFORM.                    " CARREGA_PARAMETROS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_INFO_REMESSA
*&---------------------------------------------------------------------*
*       Busca peso de origem e material da remessa
*----------------------------------------------------------------------*
FORM busca_info_remessa  USING    vg_tknum        TYPE tknum
                         CHANGING vg_zpeso_origem TYPE zpeso_origem
                                  vg_gewei        TYPE gewei
                                  vg_matnr        TYPE matnr.

  DATA: wa_vttp TYPE vttp,
        wa_lips TYPE lips,
        it_vttp TYPE TABLE OF vttp INITIAL SIZE 0 WITH HEADER LINE,
        it_lips TYPE TABLE OF lips INITIAL SIZE 0 WITH HEADER LINE.

  DATA: lt_zlest0060 TYPE TABLE OF zlest0060,
        wl_zlest0060 TYPE zlest0060,
        lt_zlest0061 TYPE TABLE OF zlest0061,
        wl_zlest0061 TYPE zlest0061.

  vg_zpeso_origem = 0.

  CASE st_dados-vsart.

    WHEN: '03'.

      IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.

        SELECT * FROM zlest0061
          INTO TABLE lt_zlest0061
        WHERE tknum EQ vg_tknum
          AND ck_anulado EQ abap_false.

        IF NOT ( lt_zlest0061[] IS INITIAL ).

          SELECT * FROM vttp
            INTO TABLE it_vttp
            FOR ALL ENTRIES IN lt_zlest0061
          WHERE tknum EQ lt_zlest0061-tknum.

          SELECT * FROM lips
            INTO TABLE it_lips
            FOR ALL ENTRIES IN it_vttp
          WHERE vbeln EQ it_vttp-vbeln.

          SELECT * FROM zlest0060
            INTO TABLE lt_zlest0060
            FOR ALL ENTRIES IN lt_zlest0061
          WHERE docnum EQ lt_zlest0061-docnum.

          IF ( sy-subrc EQ 0 ).

            CLEAR: vg_zpeso_origem.
            LOOP AT lt_zlest0060 INTO wl_zlest0060.
              READ TABLE lt_zlest0061 INTO wl_zlest0061 WITH KEY docnum = wl_zlest0060-docnum.

              IF ( sy-subrc EQ 0 ).
                READ TABLE it_vttp INTO wa_vttp WITH KEY tknum = wl_zlest0061-tknum.

                READ TABLE it_lips INTO wa_lips WITH KEY vbeln = wa_vttp-vbeln.

                vg_zpeso_origem = vg_zpeso_origem + wl_zlest0060-peso_fiscal.
                vg_gewei        = wa_lips-gewei.
                vg_matnr        = wa_lips-matnr.

              ELSE.
                CLEAR: wl_zlest0061, wl_zlest0060.
                CONTINUE.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT *
          INTO TABLE it_vttp
          FROM vttp
         WHERE tknum EQ vg_tknum.

        LOOP AT it_vttp INTO wa_vttp.

          SELECT *
            INTO TABLE it_lips
            FROM lips
           WHERE vbeln EQ wa_vttp-vbeln.

          LOOP AT it_lips INTO wa_lips.
            vg_zpeso_origem = vg_zpeso_origem + wa_lips-lfimg.
            vg_gewei        = wa_lips-gewei.
            vg_matnr        = wa_lips-matnr.
          ENDLOOP.

        ENDLOOP.
      ENDIF.

    WHEN OTHERS.

      SELECT *
        INTO TABLE it_vttp
        FROM vttp
       WHERE tknum EQ vg_tknum.

      LOOP AT it_vttp INTO wa_vttp.

        SELECT *
          INTO TABLE it_lips
          FROM lips
         WHERE vbeln EQ wa_vttp-vbeln.

        LOOP AT it_lips INTO wa_lips.
          vg_zpeso_origem = vg_zpeso_origem + wa_lips-lfimg.
          vg_gewei        = wa_lips-gewei.
          vg_matnr        = wa_lips-matnr.
        ENDLOOP.

      ENDLOOP.
  ENDCASE.





ENDFORM.                    " BUSCA_INFO_REMESSA

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_MOVIMENTO
*&---------------------------------------------------------------------*
*       Calcula QUEBRA/PERDA
*----------------------------------------------------------------------*
FORM calcular_movimento CHANGING wa_dados TYPE zftte_dados.

  DATA: pc_quebra   TYPE p DECIMALS 6,
        imp_retidos TYPE zles0043_imp_retidos.

  IF st_dados-vsart NE '02'.

    wa_dados-zpeso_diferenca = wa_dados-zpeso_origem - wa_dados-zpeso_destino.

    PERFORM busca_valor_transp_mercadoria USING wa_dados CHANGING p_kg_transp p_kg_mercad.

    "Se peso de chegado for menor que o peso de saida temos
    "quebra e se passar da tolerância temos perda
    IF wa_dados-zpeso_origem > wa_dados-zpeso_destino.
      pc_quebra = ( wa_dados-zpeso_diferenca * 100 ) / wa_dados-zpeso_origem.
      IF pc_quebra > pc_tolerancia.
        wa_dados-zperda      = ( ( pc_quebra - pc_tolerancia ) * wa_dados-zpeso_origem ) / 100.
        wa_dados-zvlr_perda  = wa_dados-zperda * p_kg_mercad.
        wa_dados-zquebra     = wa_dados-zpeso_diferenca.
        wa_dados-zvlr_quebra = wa_dados-zpeso_diferenca * p_kg_transp.
      ELSE.
        wa_dados-zperda      = 0.
        wa_dados-zvlr_perda  = 0.
        wa_dados-zquebra     = wa_dados-zpeso_diferenca.
        wa_dados-zvlr_quebra = wa_dados-zpeso_diferenca * p_kg_transp.
      ENDIF.
    ELSEIF wa_dados-zpeso_origem < wa_dados-zpeso_destino.
      wa_dados-zperda      = 0.
      wa_dados-zvlr_perda  = 0.
      wa_dados-zquebra     = wa_dados-zpeso_diferenca.
      wa_dados-zvlr_quebra = wa_dados-zpeso_diferenca * p_kg_transp.
    ELSE.
      CLEAR: wa_dados-zquebra, wa_dados-zvlr_quebra, wa_dados-zperda, wa_dados-zvlr_perda.
    ENDIF.

    IF wa_dados-zvlr_quebra LT 0.
      wa_dados-zvlr_liq_pagar  = wa_dados-dmbtr_doc - wa_dados-zvlr_perda.
    ELSE.
      wa_dados-zvlr_liq_pagar  = wa_dados-dmbtr_doc - wa_dados-zvlr_quebra - wa_dados-zvlr_perda.
    ENDIF.

  ELSE.
    wa_dados-zperda         = 0.
    wa_dados-zvlr_perda     = 0.
    wa_dados-zvlr_liq_pagar = CONV #( wa_dados-dmbtr_doc ).
  ENDIF.

  LOOP AT wa_dados-it_impostos_retidos INTO imp_retidos WHERE taxval GT 0.
    wa_dados-zvlr_liq_pagar = wa_dados-zvlr_liq_pagar - imp_retidos-taxval.
  ENDLOOP.

  PERFORM busca_taxa_icms_pis_cofins CHANGING wa_dados.

ENDFORM.                    " CALCULAR_MOVIMENTO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALOR_TRANSP_MERCADORIA
*&---------------------------------------------------------------------*
*       Busca valores em Kg do Frete e da Mercadoria
*----------------------------------------------------------------------*
FORM busca_valor_transp_mercadoria  USING    p_dados  TYPE zftte_dados
                                    CHANGING p_transp TYPE p
                                             p_mercad TYPE p.

  DATA: wa_j_1bnflin TYPE j_1bnflin.

  p_transp = 0.
  p_mercad = 0.

  "Valor do kilo transportado
  IF p_dados-zpeso_origem GT 0.
    p_transp = ( p_dados-dmbtr_doc - p_dados-valor_pedagio ) / p_dados-zpeso_origem.
  ENDIF.

  IF p_dados-docnum IS INITIAL.
    MESSAGE e062(zles).
  ENDIF.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ p_dados-docnum.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e062(zles).
  ENDIF.

  p_mercad                 = wa_j_1bnflin-netpr.
  p_dados-valor_mercadoria = wa_j_1bnflin-netwr.

ENDFORM.                    " BUSCA_VALOR_TRANSP_MERCADORIA
*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_STATUS
*&---------------------------------------------------------------------*
*       Verifica status documento de frete terceiro
*----------------------------------------------------------------------*
FORM verificar_status CHANGING wa_dados TYPE zftte_dados.

  IF ( v_rbkp  EQ 'X' ).

    wa_dados-status = icon_red_light.
  ELSE.

    IF ( wa_dados-re_belnr IS INITIAL ) AND ( wa_dados-en_docnum IS INITIAL ).
      wa_dados-status = icon_red_light.
    ELSE.
      IF ( ( NOT wa_dados-en_docnum IS INITIAL ) AND (     wa_dados-re_belnr IS INITIAL ) ) OR
         ( (     wa_dados-en_docnum IS INITIAL ) AND ( NOT wa_dados-re_belnr IS INITIAL ) ).
        wa_dados-status = icon_yellow_light.
      ELSE.
        wa_dados-status = icon_green_light.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " VERIFICAR_STATUS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_INFO_CUSTO
*&---------------------------------------------------------------------*
*       Informações do custo do frete
*----------------------------------------------------------------------*
FORM busca_info_custo  USING    vg_fknum TYPE fknum
                       CHANGING vg_bukrs TYPE bukrs
                                vg_werks TYPE werks_d
                                vg_moeda TYPE waers
                                vg_cotac TYPE kurst
                                vg_kbetr TYPE kbetr.

  DATA: wa_vfkp TYPE vfkp,
        wa_konv TYPE konv.

  CLEAR: vg_bukrs, vg_werks.

  vg_kbetr = 0.

  SELECT SINGLE * INTO wa_vfkp
    FROM vfkp
   WHERE fknum EQ vg_fknum.

  CHECK sy-subrc EQ 0.

  vg_bukrs = wa_vfkp-bukrs.
  vg_werks = wa_vfkp-werks.
  vg_moeda = wa_vfkp-waers.
  vg_cotac = wa_vfkp-kurst.

  TRY.

      cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
        EXPORTING it_selection_attribute = VALUE #(
       ( fieldname = 'KNUMV' value = wa_vfkp-knumv )
       ( fieldname = 'KSCHL' value = 'ZFRE' )
       )
        IMPORTING et_prc_element_classic_format = DATA(etl2579c2r8908) ).
      wa_konv = etl2579c2r8908[ 1 ].
    CATCH cx_prc_result cx_sy_itab_line_not_found .
      sy-subrc = 4.
  ENDTRY.

  CHECK sy-subrc EQ 0.

  vg_kbetr = wa_konv-kbetr.

ENDFORM.                    " BUSCA_INFO_CUSTO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_EKPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_ekpe  USING wa_zlest0034 TYPE zlest0034.

  DATA: wa_ekbe TYPE ekbe.

  CASE st_dados-vsart.
    WHEN '03'.

* Obter o valor da folha de serviço
      SELECT SINGLE * INTO wa_ekbe
         FROM ekbe
        WHERE ebeln = wa_zlest0034-ebeln
          AND ebelp = wa_zlest0034-ebelp
          AND vgabe = '1'
          AND xwsbr NE 'X'
          AND lfbnr = wa_zlest0034-lblni.


    WHEN OTHERS.
* Obter o valor da folha de serviço
      SELECT SINGLE * INTO wa_ekbe
        FROM ekbe
       WHERE ebeln = wa_zlest0034-ebeln
         AND ebelp = wa_zlest0034-ebelp
         AND vgabe = c_9
         AND lfbnr = wa_zlest0034-lblni.

      wa_zlest0034-dmbtr = wa_ekbe-dmbtr.

  ENDCASE.

  wa_zlest0034-lfgja = wa_ekbe-lfgja.

ENDFORM.                    " BUSCA_EKPE

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verificar_campos OUTPUT.

  IF st_dados-re_belnr IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'VG_DT_MOV'         OR
         screen-name EQ 'VG_DT_VENCTO'      OR
         screen-name EQ 'ST_TELA-NR_CONHEC' OR
         screen-name EQ 'ST_TELA-DMBTR_DOC' OR
         screen-name EQ 'ST_TELA-SERIES'    OR
         screen-name EQ 'VG_DT_CONHEC'      OR
         screen-name EQ 'VG_DT_CHEGADA'     OR
         screen-name EQ 'VG_PESO_DESTINO'   OR
         screen-name EQ 'VG_IVA'            OR
         screen-name EQ 'VG_NFE'            OR
         screen-name EQ 'VG_MULTIMODAL'     OR
         screen-name EQ 'ZLEST0034-MATNS'   OR
         screen-name EQ 'INFO_FORNE-BVTYP'  OR
         screen-name EQ 'ST_TELA-VALOR_PEDAGIO'.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    "Ferroviário
    IF ( st_dados-vsart EQ '02' ) AND ( NOT vg_ferro_novo IS INITIAL ).
      LOOP AT SCREEN.
        IF screen-name EQ 'ST_TELA-DMBTR_DOC' OR
           screen-name EQ 'ST_TELA-VALOR_PEDAGIO' OR
           screen-name EQ 'VG_PESO_DESTINO' OR
           screen-name EQ 'VG_DT_CHEGADA' OR
           screen-name EQ 'ST_TELA-NR_CONHEC' OR
           screen-name EQ 'ST_TELA-SERIES' OR
           screen-name EQ 'VG_DT_CONHEC' OR
           screen-name EQ 'VG_MULTIMODAL' OR
           screen-name EQ 'VG_NFE' .
          screen-output = '1'.
          screen-input  = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMODULE.                 " VERIFICAR_CAMPOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_MOSTRA_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_mostra_eventos.

  IF ti_eventos IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-026.
    CHECK ti_eventos IS INITIAL.
  ENDIF.

  CALL SCREEN 9999.

ENDFORM.                    " Z_MOSTRA_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  Z_MOSTRA_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_limpa_eventos.
  CLEAR: ti_eventos[].
ENDFORM.                    " Z_MOSTRA_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  Z_MOSTRA_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_add_eventos USING p_eventos TYPE zftte_eventos.
  APPEND p_eventos TO ti_eventos.
ENDFORM.                    " Z_MOSTRA_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_FORNE
*&---------------------------------------------------------------------*
*       Informações Bancárias
*----------------------------------------------------------------------*
FORM busca_dados_forne  USING vg_lifnr TYPE lifnr
                              informacoes TYPE ty_info_forne.

  DATA: wa_lfa1  TYPE lfa1,
        it_lfbk  TYPE TABLE OF lfbk INITIAL SIZE 0 WITH HEADER LINE,
        wa_lfbk  TYPE lfbk,
        wa_bnka  TYPE bnka,
        vg_bvtyp TYPE bvtyp.

  vg_bvtyp = informacoes-bvtyp.
  CLEAR: informacoes.

  SELECT SINGLE * INTO wa_lfa1
    FROM lfa1
   WHERE lifnr EQ vg_lifnr.

  CHECK sy-subrc IS INITIAL.

  CONCATENATE wa_lfa1-lifnr '-' wa_lfa1-name1 INTO informacoes-texto SEPARATED BY space.

  IF vg_bvtyp IS INITIAL.
    SELECT *
      INTO TABLE it_lfbk
      FROM lfbk
     WHERE lifnr EQ wa_lfa1-lifnr.
  ELSE.
    SELECT *
      INTO TABLE it_lfbk
      FROM lfbk
     WHERE lifnr EQ wa_lfa1-lifnr
       AND bvtyp EQ vg_bvtyp.
  ENDIF.

  DELETE it_lfbk WHERE bvtyp IS INITIAL.

  CHECK NOT it_lfbk[] IS INITIAL.

  SORT it_lfbk BY bvtyp.

  READ TABLE it_lfbk INTO wa_lfbk INDEX 1.

  SELECT SINGLE * INTO wa_bnka
    FROM bnka
   WHERE banks EQ wa_lfbk-banks
     AND bankl EQ wa_lfbk-bankl.

  CHECK sy-subrc IS INITIAL.

  informacoes-bvtyp = wa_lfbk-bvtyp.
  informacoes-bankl = wa_bnka-bankl(3).
  informacoes-banka = wa_bnka-banka.
  informacoes-bankn = wa_lfbk-bankn.

  IF NOT wa_lfbk-bkont IS INITIAL.
    CONCATENATE wa_lfbk-bankl+4(11) '-' wa_lfbk-bkont INTO informacoes-agenc.
  ELSE.
    informacoes-agenc = wa_lfbk-bankl+4(11).
  ENDIF.

ENDFORM.                    " BUSCA_DADOS_FORNE

*&---------------------------------------------------------------------*
*&      Module  HLP_BVTYP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hlp_bvtyp INPUT.

  CALL FUNCTION 'FI_F4_BVTYP'
    EXPORTING
      i_kunnr = space
      i_lifnr = st_dados-lifnr
      i_xshow = space
    IMPORTING
      e_bvtyp = info_forne-bvtyp.

ENDMODULE.                 " HLP_BVTYP  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_APLICATIVO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_aplicativo INPUT.

  IF vg_okcode EQ c_cancelar.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " EXIT_APLICATIVO  INPUT

*&---------------------------------------------------------------------*
*&      Module  HLP_MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE hlp_mwskz INPUT.

  TYPES:
    BEGIN OF ty_help,
      mwskz TYPE mwskz,
      text1 TYPE text1_007s,
    END OF ty_help.

  DATA: t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE,
        ti_help      TYPE STANDARD TABLE OF ty_help INITIAL SIZE 0 WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval,
        ti_t007a     TYPE TABLE OF t007a INITIAL SIZE 0 WITH HEADER LINE.

  DATA: st_ret TYPE ddshretval.

  SELECT * INTO TABLE ti_t007a
    FROM t007a
   WHERE kalsm EQ 'TAXBRA'.

  CHECK NOT ti_t007a[] IS INITIAL.

  SELECT mwskz text1
    INTO TABLE ti_help
    FROM t007s
    FOR ALL ENTRIES IN ti_t007a
   WHERE mwskz EQ ti_t007a-mwskz
     AND kalsm EQ ti_t007a-kalsm
     AND spras EQ sy-langu.

  CHECK NOT ti_help[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'MWSKZ'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help WITH KEY mwskz = st_ret-fieldval BINARY SEARCH.

  MOVE: 'VG_IVA'        TO t_dynpfields-fieldname,
        st_ret-fieldval TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'TX_IVA'        TO  t_dynpfields-fieldname,
        ti_help-text1   TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " HLP_MWSKZ  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNO_MIRO_FISCAL
*&---------------------------------------------------------------------*
*       Estorno de Miro / Fiscal
*----------------------------------------------------------------------*
FORM z_estorno_miro_fiscal .

  DATA: it_dados_aux             TYPE TABLE OF zftte_dados,
        wa_dados_aux             TYPE zftte_dados,
        it_return                TYPE TABLE OF bapiret2,
        vg_invoicedocnumber_miro TYPE bapi_incinv_fld-inv_doc_no,
        vg_ano_migo              TYPE bapi_incinv_fld-fisc_year,
        vg_tabix                 TYPE sy-tabix,
        wa_zlest0034             TYPE zlest0034,
        wa_zlest0032             TYPE zlest0032,
        vg_miro                  TYPE c LENGTH 1,
        vg_fiscal                TYPE c LENGTH 1,
        e_docnum                 LIKE bapi_j_1bnfdoc-docnum,
        vg_augdt                 TYPE augdt,
        vg_augbl                 TYPE augbl,
        wa_rbkp                  TYPE rbkp.

  DATA: en_docnum_aux TYPE zlest0034-en_docnum.

  CLEAR: v_rbkp.


  it_dados_aux[]    = ti_dados[].
  DELETE it_dados_aux WHERE box IS INITIAL.
  DELETE it_dados_aux WHERE re_belnr IS INITIAL.


  " Processar somente quando houver linhas selecionadas
  IF it_dados_aux[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-022.
    CHECK it_dados_aux[] IS NOT INITIAL.
  ENDIF.

  SORT it_dados_aux BY re_belnr re_gjahr en_docnum.
  "DELETE ADJACENT DUPLICATES FROM it_dados_aux COMPARING re_belnr re_gjahr en_docnum.

  LOOP AT it_dados_aux INTO wa_dados_aux.

    SELECT SINGLE * FROM rbkp INTO wa_rbkp WHERE belnr EQ wa_dados_aux-re_belnr
                                             AND gjahr EQ wa_dados_aux-re_gjahr.

    IF ( sy-subrc NE 0 ).
      v_rbkp = 'X'.



      wa_dados_aux-zdt_mov = sy-datum.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          doc_number               = wa_dados_aux-en_docnum
          ref_type                 = space
          ref_key                  = space
        IMPORTING
          doc_number               = e_docnum
        EXCEPTIONS
          document_not_found       = 1
          cancel_not_possible      = 2
          nf_cancel_type_not_found = 3
          database_problem         = 4
          docum_lock               = 5
          nfe_cancel_simulation    = 6
          OTHERS                   = 7.

      IF sy-subrc <> 0.
        CLEAR st_eventos.
        st_eventos-icone    = icon_led_red.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO st_eventos-msg_text.
        APPEND st_eventos TO ti_eventos.
      ELSEIF NOT e_docnum IS INITIAL.
        CLEAR st_eventos.
        st_eventos-icone    = icon_led_green.
        CONCATENATE '(Fiscal) Documento' wa_dados_aux-en_docnum 'estornado por' e_docnum '!' INTO st_eventos-msg_text SEPARATED BY space.
        APPEND st_eventos TO ti_eventos.
        CLEAR: vg_fiscal.
      ELSE.
        CLEAR st_eventos.
        st_eventos-icone    = icon_led_red.
        CONCATENATE '(Fiscal) Documento' wa_dados_aux-en_docnum 'não estornado!' INTO st_eventos-msg_text SEPARATED BY space.
        APPEND st_eventos TO ti_eventos.
      ENDIF.

      LOOP AT ti_dados INTO st_dados WHERE re_belnr  = wa_dados_aux-re_belnr
                                      AND re_gjahr  = wa_dados_aux-re_gjahr
                                      AND en_docnum = wa_dados_aux-en_docnum.
        vg_tabix = sy-tabix.
        CLEAR: st_dados-re_belnr, st_dados-re_gjahr.
        CLEAR: st_dados-en_docnum.

        "Atualizar ti_dados e salvar
        SELECT SINGLE * INTO wa_zlest0034
          FROM zlest0034
         WHERE tknum EQ st_dados-tknum.

        "Atualizar ti_dados e salvar
        SELECT SINGLE * INTO wa_zlest0032
          FROM zlest0032
         WHERE tknum EQ st_dados-tknum
           AND belnr EQ wa_zlest0034-re_belnr
           AND gjahr EQ wa_zlest0034-re_gjahr.


        CLEAR: wa_zlest0032-belnr, wa_zlest0032-gjahr.
        MODIFY zlest0032 FROM wa_zlest0032.

        en_docnum_aux = wa_zlest0034-en_docnum.
        CLEAR: wa_zlest0034-re_belnr, wa_zlest0034-re_gjahr, wa_zlest0034-en_docnum.
        CLEAR: wa_zlest0034-en_docnum.

        MODIFY zlest0034 FROM wa_zlest0034.
        COMMIT WORK.

        PERFORM verificar_status CHANGING st_dados.
        MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status re_belnr re_gjahr en_docnum.


        DELETE ti_dados WHERE en_docnum EQ en_docnum_aux.
        DELETE it_dados_aux WHERE en_docnum EQ en_docnum_aux.
        CLEAR: en_docnum_aux.

      ENDLOOP.
      CLEAR: v_rbkp.

    ELSE.

      IF NOT ( wa_rbkp-stblg IS INITIAL ).

        LOOP AT ti_dados INTO st_dados WHERE re_belnr  = wa_dados_aux-re_belnr
                                             AND re_gjahr  = wa_dados_aux-re_gjahr
                                             AND en_docnum = wa_dados_aux-en_docnum.
          vg_tabix = sy-tabix.
          CLEAR: st_dados-re_belnr, st_dados-re_gjahr.
          CLEAR: st_dados-en_docnum.

          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO wa_zlest0034
            FROM zlest0034
           WHERE tknum EQ st_dados-tknum.

          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO wa_zlest0032
            FROM zlest0032
           WHERE tknum EQ st_dados-tknum
             AND belnr EQ wa_zlest0034-re_belnr
             AND gjahr EQ wa_zlest0034-re_gjahr.


          CLEAR: wa_zlest0032-belnr, wa_zlest0032-gjahr.
          MODIFY zlest0032 FROM wa_zlest0032.

          en_docnum_aux = wa_zlest0034-en_docnum.
          CLEAR: wa_zlest0034-re_belnr, wa_zlest0034-re_gjahr, wa_zlest0034-en_docnum.
          CLEAR: wa_zlest0034-en_docnum.

          MODIFY zlest0034 FROM wa_zlest0034.
          COMMIT WORK.

          PERFORM verificar_status CHANGING st_dados.
          MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status re_belnr re_gjahr en_docnum.


          "DELETE ti_dados WHERE en_docnum EQ en_docnum_aux.
          DELETE it_dados_aux WHERE en_docnum EQ en_docnum_aux.
          CLEAR: en_docnum_aux.

        ENDLOOP.
        CLEAR: v_rbkp.


      ELSE.


        CLEAR: vg_miro, vg_fiscal.
        IF NOT wa_dados_aux-re_belnr IS INITIAL.

          CALL FUNCTION 'Z_VERIFICA_MIRO_PAGA'
            EXPORTING
              belnr = wa_dados_aux-re_belnr
              gjahr = wa_dados_aux-re_gjahr
            CHANGING
              augdt = vg_augdt
              augbl = vg_augbl.

          IF NOT vg_augdt IS INITIAL.
            CLEAR st_eventos.
            st_eventos-icone    = icon_led_red.
            CONCATENATE '(Miro) Doc.' wa_dados_aux-re_belnr 'Ano' wa_dados_aux-re_gjahr 'pago!'
                        'Doc. Compensação:' vg_augbl 'Ano: ' vg_augdt
                   INTO st_eventos-msg_text SEPARATED BY space.
            APPEND st_eventos TO ti_eventos.
            CONTINUE.
          ENDIF.

        ENDIF.
        IF NOT wa_dados_aux-en_docnum IS INITIAL.

          vg_fiscal = 'X'.

          wa_dados_aux-zdt_mov = sy-datum.
          CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
            EXPORTING
              doc_number               = wa_dados_aux-en_docnum
              ref_type                 = space
              ref_key                  = space
            IMPORTING
              doc_number               = e_docnum
            EXCEPTIONS
              document_not_found       = 1
              cancel_not_possible      = 2
              nf_cancel_type_not_found = 3
              database_problem         = 4
              docum_lock               = 5
              nfe_cancel_simulation    = 6
              OTHERS                   = 7.

          IF sy-subrc <> 0.
            CLEAR st_eventos.
            st_eventos-icone    = icon_led_red.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO st_eventos-msg_text.
            APPEND st_eventos TO ti_eventos.
          ELSEIF NOT e_docnum IS INITIAL.
            CLEAR st_eventos.
            st_eventos-icone    = icon_led_green.
            CONCATENATE '(Fiscal) Documento' wa_dados_aux-en_docnum 'estornado por' e_docnum '!' INTO st_eventos-msg_text SEPARATED BY space.
            APPEND st_eventos TO ti_eventos.
            CLEAR: vg_fiscal.
          ELSE.
            CLEAR st_eventos.
            st_eventos-icone    = icon_led_red.
            CONCATENATE '(Fiscal) Documento' wa_dados_aux-en_docnum 'não estornado!' INTO st_eventos-msg_text SEPARATED BY space.
            APPEND st_eventos TO ti_eventos.
          ENDIF.

        ENDIF.



        IF NOT wa_dados_aux-re_belnr IS INITIAL.

          vg_miro = c_x.

          wa_dados_aux-zdt_mov = sy-datum.

          CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
            EXPORTING
              invoicedocnumber          = wa_dados_aux-re_belnr
              fiscalyear                = wa_dados_aux-re_gjahr
              reasonreversal            = 'Z1'
            IMPORTING
              invoicedocnumber_reversal = vg_invoicedocnumber_miro
              fiscalyear_reversal       = vg_ano_migo
            TABLES
              return                    = it_return.

          IF it_return[] IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = c_x.
            CLEAR: vg_miro.


            LOOP AT ti_dados INTO st_dados WHERE re_belnr  = wa_dados_aux-re_belnr
                                                       AND re_gjahr  = wa_dados_aux-re_gjahr
                                                       AND en_docnum = wa_dados_aux-en_docnum.
              vg_tabix = sy-tabix.
              CLEAR: st_dados-re_belnr, st_dados-re_gjahr.
              CLEAR: st_dados-en_docnum.

              "Atualizar ti_dados e salvar
              SELECT SINGLE * INTO wa_zlest0034
                FROM zlest0034
               WHERE tknum EQ st_dados-tknum.

              "Atualizar ti_dados e salvar
              SELECT SINGLE * INTO wa_zlest0032
                FROM zlest0032
               WHERE tknum EQ st_dados-tknum
                 AND belnr EQ wa_zlest0034-re_belnr
                 AND gjahr EQ wa_zlest0034-re_gjahr.


              CLEAR: wa_zlest0032-belnr, wa_zlest0032-gjahr.
              MODIFY zlest0032 FROM wa_zlest0032.

              en_docnum_aux = wa_zlest0034-en_docnum.
              CLEAR: wa_zlest0034-re_belnr, wa_zlest0034-re_gjahr, wa_zlest0034-en_docnum.
              CLEAR: wa_zlest0034-en_docnum.

              MODIFY zlest0034 FROM wa_zlest0034.
              COMMIT WORK.

              PERFORM verificar_status CHANGING st_dados.
              MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status re_belnr re_gjahr en_docnum.


              "DELETE ti_dados WHERE en_docnum EQ en_docnum_aux.
              DELETE it_dados_aux WHERE en_docnum EQ en_docnum_aux.
              CLEAR: en_docnum_aux.

            ENDLOOP.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.

          PERFORM popula_log TABLES it_return.

          IF vg_miro IS INITIAL.
            CLEAR st_eventos.
            st_eventos-icone    = icon_led_green.
            CONCATENATE '(Miro) Documento' wa_dados_aux-re_belnr
                        'estornado por' vg_invoicedocnumber_miro 'ano' vg_ano_migo '!' INTO st_eventos-msg_text SEPARATED BY space.
            APPEND st_eventos TO ti_eventos.
          ENDIF.

        ENDIF.

        "Atualizar documento da miro.
        LOOP AT ti_dados INTO st_dados WHERE re_belnr  = wa_dados_aux-re_belnr
                                         AND re_gjahr  = wa_dados_aux-re_gjahr
                                         AND en_docnum = wa_dados_aux-en_docnum.
          vg_tabix = sy-tabix.
          IF vg_miro IS INITIAL.
            CLEAR: st_dados-re_belnr, st_dados-re_gjahr.
          ENDIF.

          IF vg_fiscal IS INITIAL.
            CLEAR: st_dados-en_docnum.
          ENDIF.

          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO wa_zlest0034
            FROM zlest0034
           WHERE tknum EQ st_dados-tknum.

          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO wa_zlest0032
            FROM zlest0032
           WHERE tknum EQ st_dados-tknum
             AND belnr EQ wa_zlest0034-re_belnr
             AND gjahr EQ wa_zlest0034-re_gjahr.

          IF sy-subrc IS INITIAL.
            CLEAR: wa_zlest0032-belnr, wa_zlest0032-gjahr.
            MODIFY zlest0032 FROM wa_zlest0032.
          ENDIF.

          IF vg_miro IS INITIAL.

            en_docnum_aux = wa_zlest0034-en_docnum.
            CLEAR: wa_zlest0034-re_belnr, wa_zlest0034-re_gjahr, wa_zlest0034-en_docnum.
          ENDIF.

          IF vg_fiscal IS INITIAL.
            CLEAR: wa_zlest0034-en_docnum.
          ENDIF.


          MODIFY zlest0034 FROM wa_zlest0034.
          COMMIT WORK.

          PERFORM verificar_status CHANGING st_dados.
          MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status re_belnr re_gjahr en_docnum.

          IF vg_miro IS INITIAL.
            DELETE ti_dados WHERE en_docnum EQ en_docnum_aux.
            DELETE it_dados_aux WHERE en_docnum EQ en_docnum_aux.
            CLEAR: en_docnum_aux.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.





  ENDLOOP.



ENDFORM.                    " Z_ESTORNO_MIRO_FISCAL


*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_MULTIMODAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verificar_multimodal USING vg_validado TYPE sy-subrc.

  vg_validado = 0.

  IF ( NOT vg_nfe IS INITIAL ) AND ( NOT vg_multimodal IS INITIAL ).
    MESSAGE s000 WITH 'Para documento multimodal' 'não deve ser marcado doc. Eletrônico!'.
    vg_validado = 1.
  ENDIF.

ENDFORM.                    " VERIFICAR_MULTIMODAL
*&---------------------------------------------------------------------*
*&      Form  DADOS_NAO_ENCONTRADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dados_nao_encontrado .

  MESSAGE 'Não foi encontrado registros!' TYPE 'S' DISPLAY LIKE 'W'.
  STOP.
ENDFORM.                    " DADOS_NAO_ENCONTRADO

INCLUDE zlesr00140101.

*&---------------------------------------------------------------------*
*&      Module  VERIFICAR_DATA_VENCIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verificar_data_vencimento INPUT.

  DATA: lc_data_vencimento TYPE datum.

  CHECK vg_dt_vencto IS NOT INITIAL.

  CONCATENATE vg_dt_vencto+6(4) vg_dt_vencto+3(2) vg_dt_vencto(2) INTO lc_data_vencimento.

  CALL METHOD zcl_cte_dist_g=>verifica_vencimento_fatura
    EXPORTING
      i_data_vencimento = lc_data_vencimento
    EXCEPTIONS
      nao_valida        = 1
      OTHERS            = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.
