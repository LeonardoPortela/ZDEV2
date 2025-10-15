*----------------------------------------------------------------------*
* Programa..: ZFIS41.                                                  *
* Tipo......: R - Report                                               *
* Transação.: ZFIS60                                                   *
* Descrição.: Movimento: Subcontratados Fretes - Cooperativas          *
* Autor.....: CBRAND                                                   *
* Data......: 05.08.2021                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 05.08.2021   |  |CBRAND     | Codificação Inicial                    *
*----------------------------------------------------------------------*
REPORT zfis41.
TABLES: t001, vttk, ekbe.

*---------------------------------------------------------------------*
* Declaração de Types
*---------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida_cad,
         bukrs       TYPE lfb1-bukrs,
         lifnr       TYPE lfb1-lifnr,
         name1       TYPE lfa1-name1,
         stcd1       TYPE lfa1-stcd1,
         stcd3       TYPE lfa1-stcd3,
         ort01       TYPE lfa1-ort01,
         regio       TYPE lfa1-regio,
         ktokk       TYPE lfa1-ktokk,
         txt30       TYPE t077y-txt30,
         indtyp      TYPE lfa1-indtyp,
         witht       TYPE lfbw-witht,
         wt_withcd   TYPE lfbw-wt_withcd,
         dt_registro TYPE zlest0209-dt_registro,
         hr_registro TYPE zlest0209-hr_registro,
         us_registro TYPE zlest0209-us_registro,
         mark(1)     TYPE c,
       END OF ty_saida_cad,

       BEGIN OF ty_saida_mov,
         status_ro(5)    TYPE c,
         status_dc(5)    TYPE c,
         bukrs           TYPE j_1bbranch-bukrs,
         tdlnr           TYPE vttk-tdlnr,
         werks           TYPE lips-werks,
         name            TYPE j_1bbranch-name,
         lifnr           TYPE vtpa-lifnr,  "parceiro ‘PC’
         name1           TYPE lfa1-name1,  "parceiro ‘PC’
         regio           TYPE lfa1-regio,  "parceiro ‘PC’
         kunnr           TYPE vtpa-kunnr,  "parceiro ‘LR’
         name1_lr        TYPE kna1-name1,  "parceiro ‘LR’
         regio_lr        TYPE kna1-regio,   "parceiro ‘LR’
         lifnr_pv        TYPE vtpa-lifnr,  "parceiro ‘PV’
         name1_pv        TYPE lfa1-name1,  "parceiro ‘PV’
         stcd1_pv        TYPE lfa1-stcd1,  "parceiro ‘PV’
         ktokk_pv        TYPE lfa1-ktokk,  "parceiro ‘PV’
         indtyp_pv       TYPE lfa1-indtyp, "parceiro ‘PV’
         docdat          TYPE j_1bnfdoc-docdat,
         nfenum          TYPE j_1bnfdoc-nfenum,
         docnum          TYPE j_1bnflin-docnum,
         gsber           TYPE j_1bnflin-werks,
         tknum           TYPE vttk-tknum,
         fknum           TYPE vfkp-fknum,
         vbeln           TYPE vbak-vbeln,
         vbeln_fa        TYPE vbfa-vbeln,
         nftot           TYPE j_1bnfdoc-nftot,
         budat           TYPE ekbe-budat,
         belnr           TYPE zlest0032-belnr,
         wrbtr           TYPE ekbe-wrbtr,
         docnum_32       TYPE zlest0032-docnum,
         bukrs_bk        TYPE bkpf-bukrs, "doc ZLEST0032-OBJ_KEY_SUB
         obj_key_sub     TYPE zlest0032-obj_key_sub,
         dmbtr           TYPE bsas-dmbtr,
         kawrt           TYPE konv-kawrt,
         wt_qssh2_mir    TYPE with_item-wt_qssh2, "do doc contábil da miro
         wt_qssh2_sub    TYPE with_item-wt_qssh2, "do doc contábil subcontratado
         nr_lote         TYPE zfit0171-nr_lote,
         doc_lcto_ajuste TYPE zfit0171-doc_lcto_ajuste,
         dt_lcto_ajuste  TYPE zfit0171-dt_lcto_ajuste,
         belnr_zib       TYPE zib_contabil_chv-belnr,
         qproz           TYPE t059z-qproz, " (Base de cálculo x T059Z-QSATZ ) dividido por 100
       END OF ty_saida_mov,

       BEGIN OF ty_ekbe,
         belnr TYPE ekbe-belnr,
         wrbtr TYPE ekbe-wrbtr,
         budat TYPE ekbe-budat,
         gjahr TYPE ekbe-gjahr,
         awkey TYPE bkpf-awkey,
       END OF ty_ekbe,

       BEGIN OF ty_ekbe_miro,
         bukrs  TYPE ekpo-bukrs,
         belnr  TYPE ekbe-belnr,
         ebeln  TYPE ekbe-ebeln,
         ebelp  TYPE ekbe-ebelp,
         gjahr  TYPE ekbe-gjahr,
         buzei  TYPE ekbe-buzei,
         dmbtr  TYPE ekbe-dmbtr,
         xblnr  TYPE ekbe-xblnr,
         lfgja  TYPE ekbe-lfgja,
         lfbnr  TYPE ekbe-lfbnr,
         werks  TYPE ekbe-werks,
         del(1),
         tknum  TYPE vttk-tknum,
       END OF ty_ekbe_miro,

       BEGIN OF ty_vbfa,
         vbelv  TYPE vbfa-vbelv,
         vbeln  TYPE vbfa-vbeln,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbfa,

       BEGIN OF ty_vttk,
         tknum TYPE  vttk-tknum,
         tdlnr TYPE  vttk-tdlnr,
         tplst TYPE  vttk-tplst,
         bukrs TYPE j_1bbranch-bukrs,
       END OF ty_vttk,

       BEGIN OF ty_zlest0032,
         tknum         TYPE zlest0032-tknum,
         fknum         TYPE zlest0032-fknum,
         belnr         TYPE zlest0032-belnr,
         obj_key_sub   TYPE zlest0032-obj_key_sub,
         docnum        TYPE zlest0032-docnum,
         obj_key_sub_b TYPE bkpf-belnr,
         bukrs         TYPE bkpf-bukrs,
         gjahr         TYPE bkpf-gjahr,
       END OF ty_zlest0032.
*----------------------------------------------------------------------*
* Declaração de Tabelas
*----------------------------------------------------------------------*
DATA: git_lfa1           TYPE TABLE OF lfa1,
      git_lfb1           TYPE TABLE OF lfb1,
      git_lfbw           TYPE TABLE OF lfbw,
      git_t077y          TYPE TABLE OF t077y,
      git_zlest0209      TYPE TABLE OF zlest0209,
      git_saida_cad      TYPE TABLE OF ty_saida_cad,
      git_saida_mov      TYPE TABLE OF ty_saida_mov,
      git_vttk           TYPE TABLE OF ty_vttk,
      git_vttk_aux       TYPE TABLE OF vttk,
      git_vtpa_lr        TYPE TABLE OF vtpa,
      git_vttp           TYPE TABLE OF vttp,
      git_lips           TYPE TABLE OF lips,
      git_j_1bbranch     TYPE TABLE OF j_1bbranch,
      git_j_1bbranch_aux TYPE TABLE OF j_1bbranch,
      git_vtpa           TYPE TABLE OF vtpa,
      git_vtpa_par       TYPE TABLE OF vtpa,
      git_lfa1_par       TYPE TABLE OF lfa1,
      git_kna1           TYPE TABLE OF kna1,
      git_vfkp           TYPE TABLE OF vfkp,
      git_vbak           TYPE TABLE OF vbak,
      git_vbfa           TYPE TABLE OF ty_vbfa,
      git_vbfa_aux       TYPE TABLE OF vbfa,
      git_j_1bnflin      TYPE TABLE OF j_1bnflin,
      git_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
      git_j_1bbranch_vt  TYPE TABLE OF j_1bbranch,
      git_zlest0032_aux  TYPE TABLE OF zlest0032,
      git_vttk_0032      TYPE TABLE OF vttk,
      git_zlest0032      TYPE TABLE OF ty_zlest0032,
      git_ekbe_aux       TYPE TABLE OF ekbe,
      git_ekbe           TYPE TABLE OF ty_ekbe,
      git_ekbe_miro      TYPE TABLE OF ty_ekbe_miro,
      git_ekko           TYPE TABLE OF ekko,
      git_bkpf           TYPE TABLE OF bkpf,
      git_with_item      TYPE TABLE OF with_item,
      git_bkpf_sub       TYPE TABLE OF bkpf,
      git_with_item_sub  TYPE TABLE OF with_item,
      git_bsik           TYPE TABLE OF bsik,
      git_bsak           TYPE TABLE OF bsak,
      git_vfkp_vi        TYPE TABLE OF vfkp,
      git_konv_vi        TYPE TABLE OF konv,
      git_filtro         TYPE zif_screen_linha_filtro_t.

DATA: gwa_saida_cad LIKE LINE OF git_saida_cad,
      gwa_saida_mov LIKE LINE OF git_saida_mov,
      gwa_lfbw      LIKE LINE OF git_lfbw,
      gwa_t077y     LIKE LINE OF git_t077y,
      gwa_lfb1      LIKE LINE OF git_lfb1,
      gwa_lfa1      LIKE LINE OF git_lfa1,
      gwa_zlest0209 LIKE LINE OF git_zlest0209,
      gwa_vbfa      LIKE LINE OF git_vbfa,
      gwa_ekbe      LIKE LINE OF git_ekbe,
      gwa_zlest0032 LIKE LINE OF git_zlest0032,
      gwa_vttk      LIKE LINE OF git_vttk.

*----------------------------------------------------------------------*
* Objetos
*----------------------------------------------------------------------*
DATA: git_fieldcat     TYPE lvc_t_fcat,
      gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
      gs_layout        TYPE lvc_s_layo.

DATA: gs_variant   TYPE disvariant,
      variante     LIKE disvariant,
      gs_variant_c TYPE disvariant.

DATA: chck_ucomm TYPE sy-ucomm.
*----------------------------------------------------------------------*
* Parâmetros de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN COMMENT 20(30) TEXT-002.
  PARAMETERS: p_mov RADIOBUTTON GROUP g1 USER-COMMAND fm_modifica_tela DEFAULT 'X',
              p_cad RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: p_bukrs  FOR t001-bukrs MODIF ID t1 NO-EXTENSION NO INTERVALS,
                  p_erdat  FOR vttk-erdat MODIF ID t1,
                  p_budat  FOR ekbe-budat MODIF ID t1,
                  p_tdlnr  FOR vttk-tdlnr MODIF ID t1,
                  p_shtyp  FOR vttk-shtyp MODIF ID t1,
                  p_tknum  FOR vttk-tknum MODIF ID t1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3  WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_forn AS CHECKBOX USER-COMMAND check1 MODIF ID t2,
              p_imp  AS CHECKBOX USER-COMMAND check2 MODIF ID t2,
              p_conf AS CHECKBOX USER-COMMAND check3 MODIF ID t2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
  PARAMETER: p_varia TYPE disvariant-variant MODIF ID t3.
SELECTION-SCREEN: END OF BLOCK b4.

DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

IF variante IS INITIAL.
  variante-report  = sy-repid.
  variante-variant = p_varia.
ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  vg_variant-report = vg_repid.

  IF ( p_varia IS NOT INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  variante-report  = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.

INITIALIZATION.

*---------------------------------------------------------------------*
* SELECTION-SCREEN                                                    *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  chck_ucomm = sy-ucomm.

*---------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT.                                            *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_modifica_tela.

*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fm_start_of_selection.

*---------------------------------------------------------------------*
* END-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM fm_end_of_selection.


CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  FM_MODIFICA_TELA
*&---------------------------------------------------------------------*
FORM fm_modifica_tela .
  LOOP AT SCREEN.
    IF  p_mov = 'X'.
      IF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        IF screen-group1 = 'T2'.
          screen-invisible = 1.
          screen-input     = 0.
          screen-active    = 0.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ELSE.
        IF screen-group1 = 'T2'.
          screen-invisible = 0.
          screen-input     = 1.
          screen-active    = 1.
          MODIFY SCREEN.
          CONTINUE.
        ELSE.
          IF screen-group1 = 'T3'.
            screen-invisible = 1.
            screen-input     = 0.
            screen-active    = 0.
            MODIFY SCREEN.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CASE chck_ucomm.
    WHEN 'CHECK3'. "p_conf
      CLEAR: p_forn,
             p_imp.
    WHEN 'CHECK2'.
      CLEAR: p_conf.
    WHEN 'CHECK1'.
      CLEAR: p_conf.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
FORM fm_dados_seleciona .

  DATA: lva_tabix TYPE sy-tabix.

  IF p_mov = 'X'.
    IF p_bukrs IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-100 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF p_budat IS INITIAL AND p_erdat IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-102 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF p_budat IS NOT INITIAL AND p_erdat IS NOT INITIAL.
      MESSAGE s000(z01) WITH TEXT-103 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA: lwa_j_1bbranch TYPE j_1bbranch.

    IF p_erdat IS NOT INITIAL AND p_budat IS INITIAL.

      SELECT vttk~tknum vttk~tdlnr vttk~tplst j_1bbranch~bukrs
            FROM vttk
            INNER JOIN j_1bbranch
            ON j_1bbranch~branch  = vttk~tplst
            INTO TABLE git_vttk
       WHERE vttk~erdat   IN p_erdat
          AND vttk~vsart  EQ '01'
          AND vttk~add03  EQ '0000000001'
          AND vttk~tknum  IN p_tknum
          AND vttk~tdlnr  IN p_tdlnr
          AND vttk~shtyp  IN p_shtyp
          AND j_1bbranch~bukrs IN p_bukrs.

    ELSE.

      IF p_erdat IS INITIAL AND p_budat IS NOT INITIAL.
        SELECT ekpo~bukrs ekbe~belnr ekbe~ebeln ekbe~ebelp ekbe~gjahr ekbe~buzei ekbe~dmbtr ekbe~xblnr ekbe~lfgja ekbe~lfbnr ekbe~werks
           FROM ekbe
         INNER JOIN ekpo
          ON ekbe~ebeln  = ekpo~ebeln
           INTO TABLE git_ekbe_miro
           WHERE budat IN p_budat
            AND ekbe~bewtp = 'Q'
            AND ekbe~vgabe = '2'
            AND ekbe~zekkn = '01'
            AND ekpo~bukrs IN p_bukrs.

        IF git_ekbe_miro IS NOT INITIAL.
          SELECT  *
          FROM ekko INTO TABLE git_ekko
           FOR ALL ENTRIES IN git_ekbe_miro
          WHERE ebeln  = git_ekbe_miro-ebeln
           AND  bsart = 'NB'.

          LOOP AT git_ekbe_miro INTO DATA(lwa_ekbe_miro).
            lva_tabix = sy-tabix.
            READ TABLE git_ekko INTO DATA(lwa_ekko) WITH KEY ebeln  = lwa_ekbe_miro-ebeln.
            IF lwa_ekko IS INITIAL.
              lwa_ekbe_miro-del = 'X'.
            ELSE.
              lwa_ekbe_miro-del = ''.
              lwa_ekbe_miro-tknum = lwa_ekbe_miro-xblnr.
            ENDIF.
            MODIFY git_ekbe_miro FROM lwa_ekbe_miro INDEX lva_tabix TRANSPORTING del tknum.
            CLEAR: lwa_ekbe_miro, lwa_ekko.
          ENDLOOP.

          DELETE git_ekbe_miro WHERE del = 'X'.

*– Seleção dados de transporte
          SELECT vttk~tknum vttk~tdlnr vttk~tplst
          FROM vttk INTO TABLE git_vttk
           FOR ALL ENTRIES IN git_ekbe_miro
          WHERE tknum EQ git_ekbe_miro-tknum
           AND vsart  EQ '01'
           AND add03  EQ '0000000001'
           AND tknum  IN p_tknum
           AND tdlnr  IN p_tdlnr
           AND shtyp  IN p_shtyp.

          LOOP AT git_vttk INTO DATA(lwa_vttk).
            lva_tabix = sy-tabix.
            READ TABLE git_ekbe_miro INTO lwa_ekbe_miro WITH KEY tknum = lwa_vttk-tknum.
            IF lwa_ekbe_miro IS NOT INITIAL.
              lwa_vttk-bukrs = lwa_ekbe_miro-bukrs.
              MODIFY git_vttk FROM lwa_vttk INDEX lva_tabix TRANSPORTING bukrs.
              CLEAR: lwa_vttk, lwa_ekbe_miro.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDIF.

    IF git_vttk IS NOT INITIAL.

      SELECT  *
        FROM vtpa INTO TABLE git_vtpa
         FOR ALL ENTRIES IN git_vttk
        WHERE vbeln EQ git_vttk-tknum
         AND  parvw EQ 'PV'.

      SELECT  *
        FROM lfa1 INTO TABLE git_lfa1
         FOR ALL ENTRIES IN git_vtpa
        WHERE lifnr EQ git_vtpa-lifnr.
      "AND  stkzn EQ ''.


      "Descartar VTs onde PV seja pessoa fisica
      LOOP AT git_lfa1 INTO DATA(lwa_lfa1_pv) WHERE stkzn IS NOT INITIAL.
        LOOP AT git_vtpa INTO DATA(lwa_vtpa) WHERE lifnr EQ lwa_lfa1_pv-lifnr
                                               AND parvw EQ 'PV'.
          DELETE git_vttk WHERE tknum EQ lwa_vtpa-vbeln.
        ENDLOOP.
      ENDLOOP.

*  - Seleção para dados da remessa
      SELECT  *
        FROM vttp INTO TABLE git_vttp
         FOR ALL ENTRIES IN git_vttk
        WHERE tknum EQ git_vttk-tknum
         AND  tpnum EQ '0001'.

      SELECT  *
        FROM lips INTO TABLE git_lips
         FOR ALL ENTRIES IN git_vttp
        WHERE vbeln EQ git_vttp-vbeln.

*  - Seleção para nome filial remetente
      SELECT  *
        FROM j_1bbranch INTO TABLE git_j_1bbranch
         FOR ALL ENTRIES IN git_lips
        WHERE branch EQ git_lips-werks.

* - Busca parceiros
      SELECT  *
      FROM vtpa INTO TABLE git_vtpa_par
       FOR ALL ENTRIES IN git_vttk
      WHERE vbeln EQ git_vttk-tknum
       AND  parvw IN ('PC','PV').

      SELECT  *
        FROM lfa1 INTO TABLE git_lfa1_par
         FOR ALL ENTRIES IN git_vtpa_par
        WHERE lifnr EQ git_vtpa_par-lifnr
         AND  land1 EQ 'BR'.

      SELECT  *
       FROM vtpa INTO TABLE git_vtpa_lr
        FOR ALL ENTRIES IN git_vttk
       WHERE vbeln EQ git_vttk-tknum
        AND  parvw EQ 'LR'.

      SELECT  *
        FROM kna1 INTO TABLE git_kna1
         FOR ALL ENTRIES IN git_vtpa_lr
        WHERE kunnr EQ git_vtpa_lr-kunnr
         AND  land1 EQ 'BR'.

* - Seleção para documento de custo
      SELECT  *
        FROM vfkp INTO TABLE git_vfkp
      FOR ALL ENTRIES IN git_vttk
        WHERE rebel EQ git_vttk-tknum
        AND fkpos = '000001'.

* -  Seleção Ov. Serviço, Fatura serviço, Docnum e CT-e
      SELECT  *
        FROM vbak INTO TABLE git_vbak
      FOR ALL ENTRIES IN git_vttk
         WHERE tknum EQ git_vttk-tknum.

      SELECT *
        FROM vbfa INTO TABLE git_vbfa_aux
        FOR ALL ENTRIES IN git_vbak
        WHERE vbelv EQ git_vbak-vbeln
          AND vbtyp_n EQ 'M'
          AND vbtyp_v EQ 'C'.

      LOOP AT git_vbfa_aux INTO DATA(gwa_vbfa_aux).

        gwa_vbfa-vbelv = gwa_vbfa_aux-vbelv.
        gwa_vbfa-vbeln = gwa_vbfa_aux-vbeln.
        gwa_vbfa-refkey = gwa_vbfa-vbeln.

        APPEND gwa_vbfa TO git_vbfa.
        CLEAR: gwa_vbfa.
      ENDLOOP.

      IF git_vbfa IS NOT INITIAL.
        SELECT  *
          FROM j_1bnflin INTO TABLE git_j_1bnflin
        FOR ALL ENTRIES IN git_vbfa
          WHERE refkey EQ git_vbfa-refkey.
      ENDIF.

      IF git_j_1bnflin IS NOT INITIAL.
        SELECT  *
          FROM j_1bnfdoc INTO TABLE git_j_1bnfdoc
       FOR ALL ENTRIES IN git_j_1bnflin
          WHERE docnum EQ git_j_1bnflin-docnum.
      ENDIF.

* - Seleção para empresa da VT
      SELECT  *
        FROM j_1bbranch INTO TABLE git_j_1bbranch_vt
      FOR ALL ENTRIES IN git_vttk
        WHERE branch EQ git_vttk-tplst.

* - Seleção para Miro, doc subcontratação, docnum entrada
      SELECT  *
        FROM zlest0032 INTO TABLE git_zlest0032_aux
      FOR ALL ENTRIES IN git_vfkp
        WHERE tknum EQ git_vfkp-rebel
          AND fknum EQ git_vfkp-fknum
          AND add03 =  '0000000001'.

      SELECT  *
          FROM vttk INTO TABLE git_vttk_0032
        FOR ALL ENTRIES IN git_zlest0032_aux
          WHERE tknum EQ git_zlest0032_aux-tknum.

      LOOP AT git_zlest0032_aux INTO DATA(gwa_zlest0032_aux).

        READ TABLE git_vttk_0032 INTO DATA(gwa_vttk_0032) WITH KEY tknum = gwa_zlest0032_aux-tknum.

        IF gwa_vttk_0032-tdlnr IS NOT INITIAL.
          CLEAR: lwa_j_1bbranch.
          SELECT SINGLE * FROM j_1bbranch INTO lwa_j_1bbranch
           WHERE branch EQ gwa_vttk_0032-tdlnr+6(4).

          gwa_zlest0032-bukrs =  lwa_j_1bbranch-bukrs.
          gwa_zlest0032-gjahr =  gwa_zlest0032_aux-data+0(4).
        ENDIF.

        gwa_zlest0032-tknum          = gwa_zlest0032_aux-tknum.
        gwa_zlest0032-fknum          = gwa_zlest0032_aux-fknum.
        gwa_zlest0032-belnr          = gwa_zlest0032_aux-belnr.
        gwa_zlest0032-obj_key_sub    = gwa_zlest0032_aux-obj_key_sub.
        gwa_zlest0032-docnum         = gwa_zlest0032_aux-docnum.
        gwa_zlest0032-obj_key_sub_b  = gwa_zlest0032_aux-obj_key_sub.
        MOVE gwa_zlest0032_aux-obj_key_sub TO gwa_zlest0032-obj_key_sub_b.
        APPEND gwa_zlest0032 TO git_zlest0032.
        CLEAR: gwa_zlest0032.
      ENDLOOP.

* - Dados da Miro
      IF git_zlest0032 IS NOT INITIAL.

        SELECT  *
          FROM ekbe INTO TABLE git_ekbe_aux
        FOR ALL ENTRIES IN git_zlest0032
          WHERE belnr EQ git_zlest0032-belnr.

        LOOP AT git_ekbe_aux INTO DATA(gwa_ekbe_aux).
          gwa_ekbe-belnr = gwa_ekbe_aux-belnr.
          gwa_ekbe-wrbtr = gwa_ekbe_aux-wrbtr.
          gwa_ekbe-budat = gwa_ekbe_aux-budat.
          gwa_ekbe-gjahr = gwa_ekbe_aux-gjahr.
          CONCATENATE gwa_ekbe-belnr gwa_ekbe-gjahr INTO gwa_ekbe-awkey.
          APPEND gwa_ekbe TO git_ekbe.
          CLEAR: gwa_ekbe, gwa_ekbe_aux.
        ENDLOOP.

* - Documento contábil da miro
        SELECT  *
          FROM bkpf INTO TABLE git_bkpf
        FOR ALL ENTRIES IN git_ekbe
          WHERE awkey EQ git_ekbe-awkey.


        IF git_bkpf IS NOT INITIAL.
* - Impostos retidos
          SELECT  *
            FROM with_item INTO TABLE git_with_item
          FOR ALL ENTRIES IN git_bkpf
            WHERE bukrs EQ git_bkpf-bukrs
             AND belnr  EQ git_bkpf-belnr
             AND gjahr  EQ git_bkpf-gjahr
             AND witht  EQ 'IW'.
        ENDIF.

* - Dados do Documento de Subcontratação
        DATA(git_zlest0032_sub)  = git_zlest0032[].
        DELETE git_zlest0032_sub WHERE obj_key_sub_b IS INITIAL.

        DATA: s_bktxt(14).
        s_bktxt = 'SUBCONTRATADO%'.

        IF git_zlest0032_sub[] IS NOT INITIAL.
          SELECT  *
            FROM bkpf INTO TABLE git_bkpf_sub
          FOR ALL ENTRIES IN git_zlest0032_sub
            WHERE belnr EQ git_zlest0032_sub-obj_key_sub_b
              AND blart EQ 'FR'
              AND bukrs EQ git_zlest0032_sub-bukrs
              AND gjahr EQ git_zlest0032_sub-gjahr
              AND bktxt LIKE s_bktxt.
        ENDIF.

* - Impostos no documento de subcontratação
        IF git_bkpf_sub IS NOT INITIAL.
          SELECT  *
            FROM with_item INTO TABLE git_with_item_sub
          FOR ALL ENTRIES IN git_bkpf_sub
            WHERE bukrs EQ git_bkpf_sub-bukrs
             AND belnr  EQ git_bkpf_sub-belnr
             AND gjahr  EQ git_bkpf_sub-gjahr
             AND witht  EQ 'IW'.

          SELECT  *
            FROM bsik INTO TABLE git_bsik
         FOR ALL ENTRIES IN git_bkpf_sub
            WHERE bukrs EQ git_bkpf_sub-bukrs
              AND belnr EQ git_bkpf_sub-belnr
              AND gjahr EQ git_bkpf_sub-gjahr.

          SELECT  *
            FROM bsak INTO TABLE git_bsak
         FOR ALL ENTRIES IN git_bkpf_sub
            WHERE bukrs EQ git_bkpf_sub-bukrs
              AND belnr EQ git_bkpf_sub-belnr
              AND gjahr EQ git_bkpf_sub-gjahr.

        ENDIF.
      ENDIF.

* - Valor IRF na VI
      IF git_vfkp[] IS NOT INITIAL.
        SELECT  *
          FROM vfkp INTO TABLE git_vfkp_vi
        FOR ALL ENTRIES IN git_vfkp
          WHERE fknum  EQ git_vfkp-fknum.

        IF git_vfkp_vi[] IS NOT INITIAL.
          SELECT FROM v_konv FIELDS * FOR ALL ENTRIES IN @git_vfkp_vi WHERE knumv EQ @git_vfkp_vi-knumv AND kappl EQ 'F' AND kschl EQ 'ZIRF' INTO CORRESPONDING FIELDS OF TABLE @git_konv_vi .
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE s000(z01) WITH TEXT-101 DISPLAY LIKE 'W'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  IF p_cad = 'X'.
* 1 - Opção não seleciona nada na tela.

    IF p_forn IS INITIAL AND
       p_imp  IS INITIAL OR
       p_conf IS NOT INITIAL.

      RANGES: lrg_lifnr FOR lfa1-lifnr.

      DATA: s_coop(5).
      s_coop = 'COOP%'.

      SELECT  *
        FROM lfa1 INTO TABLE git_lfa1
          WHERE land1  EQ 'BR'
            AND ktokk IN ('ZFFJ','ZFNJ', 'ZPRJ')
            AND loevm NE 'X'
            AND name1 LIKE s_coop
            AND indtyp NE 'Z3'.

      IF  p_conf IS  INITIAL.

        SELECT  *
          FROM zlest0209 INTO TABLE git_zlest0209
           FOR ALL ENTRIES IN git_lfa1
          WHERE lifnr EQ git_lfa1-lifnr
          AND eliminado = ''.

        IF git_zlest0209 IS NOT INITIAL.

*---> 04/07/2023 - Migração S4 - WS
          SORT git_zlest0209 BY lifnr.
*<--- 04/07/2023 - Migração S4 - WS

          DELETE ADJACENT DUPLICATES FROM git_zlest0209 COMPARING lifnr.

          LOOP AT git_zlest0209 INTO DATA(lwa_zlest0209).
            lrg_lifnr-sign = 'I'.
            lrg_lifnr-option = 'EQ'.
            lrg_lifnr-low = lwa_zlest0209-lifnr.
            APPEND lrg_lifnr.
          ENDLOOP.
          CLEAR: lwa_zlest0209.

          DELETE git_lfa1 WHERE lifnr IN lrg_lifnr.

        ENDIF.
      ELSE.
        SELECT  *
          FROM zlest0209 INTO TABLE git_zlest0209
           FOR ALL ENTRIES IN git_lfa1
          WHERE lifnr EQ git_lfa1-lifnr.

        IF git_zlest0209 IS INITIAL.
          MESSAGE s000(z01) WITH TEXT-101 DISPLAY LIKE 'W'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.

    ELSE.
* 2 -  Fornecedores Z3 marcado e sem imposto 'IW'
* 4 -  Fornecedores com "Z3" marcado e com imposto "IW"
      IF  p_forn IS NOT INITIAL OR
          p_imp  IS NOT INITIAL AND
          p_conf IS INITIAL.

        SELECT  *
      FROM lfa1 INTO TABLE git_lfa1
        WHERE land1  EQ 'BR'
          AND ktokk  IN ('ZFFJ','ZFNJ', 'ZPRJ')
          AND loevm  NE 'X'
          AND indtyp EQ 'Z3'.

      ELSE.
* 3 - Fornecedores sem "Z3" marcado e com imposto "IW"
        IF  p_forn IS INITIAL AND
            p_imp  IS NOT INITIAL AND
            p_conf IS INITIAL.

          SELECT  *
        FROM lfa1 INTO TABLE git_lfa1
          WHERE land1  EQ 'BR'
            AND ktokk IN ('ZFFJ','ZFNJ', 'ZPRJ')
            AND loevm NE 'X'
            AND indtyp NE 'Z3'.

        ENDIF.
      ENDIF.
    ENDIF.

    IF git_lfa1 IS NOT INITIAL.

      SELECT  *
        FROM lfb1 INTO TABLE git_lfb1
         FOR ALL ENTRIES IN git_lfa1
        WHERE lifnr    EQ git_lfa1-lifnr
        AND loevm NE 'X'.

      IF  p_imp IS NOT INITIAL.
        SELECT  *
        FROM lfbw INTO TABLE git_lfbw
         FOR ALL ENTRIES IN git_lfb1
        WHERE lifnr  EQ git_lfb1-lifnr
           AND bukrs EQ git_lfb1-bukrs
           AND witht EQ 'IW'.
      ELSE.
        SELECT  *
        FROM lfbw INTO TABLE git_lfbw
        FOR ALL ENTRIES IN git_lfb1
        WHERE lifnr EQ git_lfb1-lifnr
         AND bukrs  EQ git_lfb1-bukrs
         AND witht  NE 'IW'.
      ENDIF.

      IF  p_conf IS INITIAL.
        SELECT  *
         FROM t077y INTO TABLE git_t077y
          FOR ALL ENTRIES IN git_lfa1
         WHERE ktokk EQ git_lfa1-ktokk.

      ENDIF.

    ELSE.
      MESSAGE s000(z01) WITH TEXT-101 DISPLAY LIKE 'W'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
FORM fm_dados_processa .

  IF p_mov = 'X'.

    DATA: lwa_lfbw      TYPE lfbw,
          lwa_zfit0171  TYPE zfit0171,
          lwa_t059z     TYPE t059z,
          lva_base_calc TYPE zglt036-vlr_moeda_doc,
          lva_imp       TYPE zglt036-vlr_moeda_doc,
          lva_obj_key   TYPE zib_contabil_chv-obj_key.


    LOOP AT git_vttk INTO gwa_vttk.

      CLEAR: gwa_saida_mov.

      gwa_saida_mov-tdlnr  =  gwa_vttk-tdlnr.


      READ TABLE git_vtpa INTO DATA(gwa_vtpa) WITH KEY vbeln   = gwa_vttk-tknum.
      READ TABLE git_lfa1 INTO DATA(gwa_lfa1) WITH KEY lifnr =  gwa_vtpa-lifnr.

      gwa_saida_mov-lifnr_pv  = gwa_vtpa-lifnr.
      gwa_saida_mov-name1_pv  = gwa_lfa1-name1.
      gwa_saida_mov-stcd1_pv  = gwa_lfa1-stcd1.
      gwa_saida_mov-ktokk_pv  = gwa_lfa1-ktokk.
      gwa_saida_mov-indtyp_pv  = gwa_lfa1-indtyp.

* dados da remessa
      READ TABLE git_vttp INTO DATA(gwa_vttp) WITH KEY tknum  = gwa_vttk-tknum.
      READ TABLE git_lips INTO DATA(gwa_lips) WITH KEY vbeln = gwa_vttp-vbeln.
      gwa_saida_mov-werks  = gwa_lips-werks.

      READ TABLE git_j_1bbranch INTO DATA(gwa_j_1bbranch) WITH KEY branch = gwa_lips-werks.
      gwa_saida_mov-bukrs  = gwa_j_1bbranch-bukrs.
      gwa_saida_mov-name  = gwa_j_1bbranch-name.

* dados parceiros
      READ TABLE git_vtpa_par INTO DATA(gwa_vtpa_par) WITH KEY vbeln   = gwa_vttk-tknum.
      READ TABLE git_lfa1_par INTO DATA(gwa_lfa1_par) WITH KEY lifnr = gwa_vtpa_par-lifnr.

      gwa_saida_mov-lifnr  = gwa_vtpa_par-lifnr.
      gwa_saida_mov-name1  = gwa_lfa1_par-name1.
      gwa_saida_mov-regio  = gwa_lfa1_par-regio.

      READ TABLE git_vtpa_lr INTO DATA(gwa_vtpa_lr) WITH KEY vbeln   = gwa_vttk-tknum.
      READ TABLE git_kna1 INTO DATA(gwa_kna1) WITH KEY kunnr = gwa_vtpa_lr-kunnr.

      gwa_saida_mov-kunnr  = gwa_vtpa_lr-kunnr.
      gwa_saida_mov-name1_lr  = gwa_kna1-name1.
      gwa_saida_mov-regio_lr  = gwa_kna1-regio.


* documento de custo
      READ TABLE git_vfkp INTO DATA(gwa_vfkp) WITH KEY rebel   = gwa_vttk-tknum.

      gwa_saida_mov-tknum  = gwa_vttk-tknum.
      gwa_saida_mov-fknum  = gwa_vfkp-fknum.


* Ov. Serviço, Fatura serviço, Docnum e CT-e
      READ TABLE git_vbak INTO DATA(gwa_vbak) WITH KEY tknum = gwa_vttk-tknum.

      gwa_saida_mov-vbeln  = gwa_vbak-vbeln.


      READ TABLE git_vbfa INTO gwa_vbfa WITH KEY vbelv  = gwa_vbak-vbeln.

      gwa_saida_mov-vbeln_fa  =  gwa_vbfa-vbeln.

      READ TABLE git_j_1bnflin INTO DATA(gwa_j_1bnflin) WITH KEY refkey = gwa_vbfa-refkey.

      gwa_saida_mov-docnum  = gwa_j_1bnflin-docnum.

      READ TABLE git_j_1bnfdoc INTO DATA(gwa_j_1bnfdoc) WITH KEY docnum = gwa_j_1bnflin-docnum.

      gwa_saida_mov-docdat  = gwa_j_1bnfdoc-docdat.
      gwa_saida_mov-nfenum  = gwa_j_1bnfdoc-nfenum.
      gwa_saida_mov-nftot   = gwa_j_1bnfdoc-nftot.


* empresa da VT
      READ TABLE git_j_1bbranch_vt INTO DATA(gwa_j_1bbranch_vt) WITH KEY branch    = gwa_vttk-tplst.

* Miro, doc subcontratação, docnum entrada

      READ TABLE git_zlest0032 INTO DATA(gwa_zlest0032) WITH KEY tknum = gwa_vfkp-rebel
                                                                 fknum = gwa_vfkp-fknum.
      IF sy-subrc EQ 0.

        gwa_saida_mov-belnr      = gwa_zlest0032-belnr.
        gwa_saida_mov-docnum_32  = gwa_zlest0032-docnum.

*       Para WL_ZGLT036-GSBER
        SELECT SINGLE * FROM j_1bnflin INTO @DATA(lwa_j_1bnflin)
           WHERE docnum  EQ @gwa_zlest0032-docnum
            AND  itmnum EQ '000010'.
        IF sy-subrc EQ 0.
          gwa_saida_mov-gsber   = lwa_j_1bnflin-werks.
        ENDIF.

* Documento de Subcontratação
        READ TABLE git_bkpf_sub INTO DATA(gwa_bkpf_sub) WITH KEY belnr = gwa_zlest0032-obj_key_sub_b.
        IF ( sy-subrc EQ 0 ) AND ( gwa_zlest0032-obj_key_sub_b IS NOT INITIAL ).
          gwa_saida_mov-bukrs_bk     = gwa_bkpf_sub-bukrs.
          gwa_saida_mov-obj_key_sub  = gwa_zlest0032-obj_key_sub_b.

* Impostos no documento de subcontratação
          READ TABLE git_with_item_sub INTO DATA(gwa_with_item_sub) WITH KEY bukrs  = gwa_bkpf_sub-bukrs
                                                                             belnr  = gwa_bkpf_sub-belnr
                                                                             gjahr  = gwa_bkpf_sub-gjahr.
          IF sy-subrc EQ 0.
            gwa_saida_mov-wt_qssh2_sub  = gwa_with_item_sub-wt_qssh2.
          ENDIF.

          READ TABLE git_bsik INTO DATA(gwa_bsik) WITH KEY bukrs  = gwa_bkpf_sub-bukrs
                                                           belnr  = gwa_bkpf_sub-belnr
                                                           gjahr  = gwa_bkpf_sub-gjahr.

          IF sy-subrc EQ 0.
            gwa_saida_mov-dmbtr  = gwa_bsik-dmbtr.
          ELSE.
            READ TABLE git_bsak INTO DATA(gwa_bsak) WITH KEY bukrs  = gwa_bkpf_sub-bukrs
                                                             belnr  = gwa_bkpf_sub-belnr
                                                             gjahr  = gwa_bkpf_sub-gjahr.
            IF sy-subrc EQ 0.
              gwa_saida_mov-dmbtr  = gwa_bsak-dmbtr.
            ENDIF.

          ENDIF.

        ENDIF.

        READ TABLE git_ekbe INTO DATA(gwa_ekbe) WITH KEY belnr = gwa_zlest0032-belnr.
        IF sy-subrc EQ 0.
          gwa_saida_mov-budat  = gwa_ekbe-budat.
          gwa_saida_mov-wrbtr  = gwa_ekbe-wrbtr.

*         Documento contábil da miro
          READ TABLE git_bkpf INTO DATA(gwa_bkpf) WITH KEY awkey = gwa_ekbe-awkey.

          IF sy-subrc EQ 0.
            READ TABLE git_with_item INTO DATA(gwa_with_item) WITH KEY bukrs  = gwa_bkpf-bukrs
                                                                       belnr  = gwa_bkpf-belnr
                                                                       gjahr  = gwa_bkpf-gjahr.

            IF sy-subrc EQ 0.
              gwa_saida_mov-wt_qssh2_mir  = gwa_with_item-wt_qssh2.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

* Valor IRF na VI
      READ TABLE git_vfkp_vi INTO DATA(gwa_vfkp_vi) WITH KEY fknum   = gwa_vfkp-fknum.
      IF sy-subrc EQ 0.
        READ TABLE git_konv_vi INTO DATA(gwa_konv_vi) WITH KEY knumv  = gwa_vfkp_vi-knumv.
        IF sy-subrc EQ 0.
          gwa_saida_mov-kawrt  =  gwa_konv_vi-kwert.
        ENDIF.
      ENDIF.

      CLEAR:  lwa_lfbw,
              lwa_t059z,
              lva_base_calc,
              lva_imp.

      SELECT SINGLE * FROM lfbw INTO lwa_lfbw
       WHERE lifnr  = gwa_saida_mov-lifnr_pv
            AND  bukrs     = gwa_saida_mov-bukrs_bk
            AND  witht     = 'IW'
            AND  wt_withcd = 'R5'.

      CLEAR:lwa_t059z.
      SELECT SINGLE * FROM t059z INTO lwa_t059z
       WHERE land1 = 'BR'
         AND witht EQ lwa_lfbw-witht
         AND wt_withcd  =  lwa_lfbw-wt_withcd.

      lva_base_calc =  ( gwa_saida_mov-dmbtr * lwa_t059z-qproz ) / 100.
      lva_imp       =  ( lva_base_calc * lwa_t059z-qsatz ) / 100.

      gwa_saida_mov-qproz  = lva_imp.


      CLEAR: lwa_zfit0171.
      SELECT SINGLE * FROM zfit0171 INTO lwa_zfit0171
       WHERE tknum           = gwa_saida_mov-tknum
         AND fknum           = gwa_saida_mov-fknum
         AND lfa1_pv         = gwa_saida_mov-lifnr_pv
         AND lfa1_indtyp     = gwa_saida_mov-indtyp_pv.

      IF sy-subrc EQ 0.
        gwa_saida_mov-nr_lote          = lwa_zfit0171-nr_lote.
        gwa_saida_mov-doc_lcto_ajuste  = lwa_zfit0171-doc_lcto_ajuste.
        gwa_saida_mov-dt_lcto_ajuste   = lwa_zfit0171-dt_lcto_ajuste.
        CONCATENATE 'ZGL17' gwa_saida_mov-doc_lcto_ajuste lwa_zfit0171-dt_lcto_ajuste+0(4) INTO lva_obj_key.
      ENDIF.

********************************Status documento contábil*****************************************
      IF gwa_saida_mov-doc_lcto_ajuste IS NOT INITIAL.

        SELECT SINGLE * FROM zib_contabil_chv INTO @DATA(lwa_zib_contabil_chv)
          WHERE obj_key EQ @lva_obj_key.

        IF sy-subrc EQ 0.
* Retenção avulsa gerada com sucesso
          MOVE icon_green_light TO gwa_saida_mov-status_dc.
          gwa_saida_mov-belnr_zib  = lwa_zib_contabil_chv-belnr.
        ELSE.

          SELECT SINGLE * FROM zib_contabil_err INTO @DATA(lwa_zib_contabil_err)
           WHERE obj_key EQ @lva_obj_key.

          IF  sy-subrc EQ 0.
* Erro na geração da retenção avulsa -  Mostrar o erro no hotspot  ( ZIB_CONTABIL_ERR)
            MOVE icon_red_light TO gwa_saida_mov-status_dc.
          ELSE.
* Retenção avulsa em processamento
            MOVE icon_yellow_light TO gwa_saida_mov-status_dc.
          ENDIF.

        ENDIF.
      ELSE.
* Retenção avulsa não gerada
        MOVE icon_light_out  TO gwa_saida_mov-status_dc.
      ENDIF.

************************* Status Retenção Origem ******************************

      IF gwa_saida_mov-indtyp_pv = 'Z3' AND gwa_saida_mov-kawrt IS NOT INITIAL.
        MOVE icon_green_light TO gwa_saida_mov-status_ro.
      ENDIF.

      IF gwa_saida_mov-indtyp_pv = 'Z3' AND gwa_saida_mov-kawrt IS INITIAL.
        MOVE icon_yellow_light TO gwa_saida_mov-status_ro.
      ENDIF.

      IF gwa_saida_mov-indtyp_pv <> 'Z3' .

        SEARCH  gwa_saida_mov-name1  FOR 'COOP'.
        IF sy-subrc = 0.

          SELECT SINGLE * FROM zlest0209 INTO @DATA(lwa_zlest0209)
         WHERE lifnr EQ @gwa_saida_mov-lifnr.

          IF lwa_zlest0209 IS NOT INITIAL.

            IF lwa_zlest0209-eliminado IS INITIAL..
* Fornecedor a analisar
              MOVE icon_red_light TO gwa_saida_mov-status_ro.
            ELSE.
* Fornecedor sem retenção
              MOVE icon_light_out TO gwa_saida_mov-status_ro.
            ENDIF.
          ENDIF.

          MOVE icon_red_light TO gwa_saida_mov-status_ro.
        ELSE.
          MOVE icon_light_out TO gwa_saida_mov-status_ro.
        ENDIF.
      ENDIF.

      APPEND gwa_saida_mov TO git_saida_mov.
      CLEAR: gwa_saida_mov,
             gwa_lfa1,
             gwa_vtpa,
             gwa_vttp,
             gwa_lips,
             gwa_j_1bbranch,
             gwa_vtpa_par,
             gwa_lfa1_par,
             gwa_vtpa_lr,
             gwa_kna1,
             gwa_vfkp,
             gwa_vbak,
             gwa_vbfa,
             gwa_j_1bnflin,
             gwa_j_1bnfdoc,
             gwa_j_1bbranch_vt,
             gwa_zlest0032,
             gwa_ekbe,
             lwa_j_1bnflin,
             gwa_bkpf,
             gwa_with_item,
             gwa_bkpf_sub,
             gwa_with_item_sub,
             gwa_bsik,
             gwa_bsak,
             gwa_vfkp_vi,
             gwa_konv_vi,
             lwa_zib_contabil_chv,
             lwa_zib_contabil_err,
             lwa_zlest0209.
    ENDLOOP.

  ELSE.
    LOOP AT git_lfa1 INTO gwa_lfa1.

      IF  p_conf IS INITIAL.

        READ TABLE git_lfb1 INTO gwa_lfb1 WITH KEY lifnr = gwa_lfa1-lifnr.

        gwa_saida_cad-bukrs =  gwa_lfb1-bukrs.
        gwa_saida_cad-lifnr =  gwa_lfb1-lifnr.
        gwa_saida_cad-name1 =  gwa_lfa1-name1.
        gwa_saida_cad-stcd1 =  gwa_lfa1-stcd1.
        gwa_saida_cad-stcd3 =  gwa_lfa1-stcd3.
        gwa_saida_cad-ort01 =  gwa_lfa1-ort01.
        gwa_saida_cad-regio =  gwa_lfa1-regio.
        gwa_saida_cad-ktokk =  gwa_lfa1-ktokk.

        READ TABLE git_t077y INTO gwa_t077y  WITH KEY ktokk = gwa_lfa1-ktokk.
        gwa_saida_cad-txt30 = gwa_t077y-txt30.

        gwa_saida_cad-indtyp =   gwa_lfa1-indtyp.
        READ TABLE git_lfbw INTO gwa_lfbw WITH KEY  lifnr = gwa_lfb1-lifnr
                                                    bukrs = gwa_lfb1-bukrs.
        gwa_saida_cad-witht     =  gwa_lfbw-witht.
        gwa_saida_cad-wt_withcd =  gwa_lfbw-wt_withcd.

        APPEND gwa_saida_cad TO git_saida_cad.

        CLEAR:  gwa_saida_cad,
                gwa_lfbw,
                gwa_t077y,
                gwa_lfb1,
                gwa_lfa1.
      ELSE.

        LOOP AT git_zlest0209 INTO gwa_zlest0209 WHERE lifnr = gwa_lfa1-lifnr.

          IF sy-subrc = 0 AND gwa_zlest0209-eliminado IS INITIAL.

            gwa_saida_cad-lifnr       =  gwa_lfa1-lifnr.
            gwa_saida_cad-name1       =  gwa_lfa1-name1.
            gwa_saida_cad-stcd1       =  gwa_lfa1-stcd1.
            gwa_saida_cad-stcd3       =  gwa_lfa1-stcd3.
            gwa_saida_cad-ort01       =  gwa_lfa1-ort01.
            gwa_saida_cad-regio       =  gwa_lfa1-regio.
            gwa_saida_cad-ktokk       =  gwa_lfa1-ktokk.
            gwa_saida_cad-dt_registro = gwa_zlest0209-dt_registro.
            gwa_saida_cad-hr_registro = gwa_zlest0209-hr_registro.
            gwa_saida_cad-us_registro = gwa_zlest0209-us_registro.

            APPEND gwa_saida_cad TO git_saida_cad.

            CLEAR:  gwa_saida_cad,
                    gwa_zlest0209,
                    gwa_lfa1.

          ENDIF.
        ENDLOOP.

      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*--------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  IF p_cad = 'X'.
    APPEND 'PROCESSAR'   TO fcode.
    APPEND 'ESTORNAR'    TO fcode.
    APPEND 'ATUALIZAR'     TO fcode.
    IF p_conf = 'X'.
      APPEND 'CONFERIDO'   TO fcode.
    ENDIF.

    IF p_forn = 'X' OR
       p_imp = 'X'.
      APPEND 'CONFERIDO'   TO fcode.
      APPEND 'DESF_CONF'   TO fcode.
    ENDIF.

    IF p_forn IS INITIAL AND
       p_imp  IS INITIAL AND
       p_conf IS INITIAL.
      APPEND 'DESF_CONF'   TO fcode.
    ENDIF.

  ENDIF.

  IF p_mov = 'X'.
    APPEND 'CONFERIDO'   TO fcode.
    APPEND 'DESF_CONF'   TO fcode.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR 'TB0100' WITH 'Subcontratos Fretes - Cooperativa'.
  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
FORM fm_criar_objetos .

  PERFORM fm_cria_fieldcat.
  PERFORM fm_fill_gs_variant.

  DATA: i_filtros	TYPE zif_screen_linha_filtro_t,
        p_text    TYPE sdydo_text_element.

  IF p_cad = 'X'.
    p_text = 'Cadastro - Subcontratos Fretes - Cooperativa'.

    IF p_forn IS NOT INITIAL AND p_imp IS INITIAL AND  p_conf IS INITIAL.
      APPEND VALUE #( parametro = 'Seleção para opção:' valor = 'Fornecedores com "Z3" marcado e sem imposto "IW" ' ) TO i_filtros.
    ENDIF.

    IF p_forn IS NOT INITIAL AND p_imp IS NOT INITIAL AND  p_conf IS INITIAL.
      APPEND VALUE #( parametro = 'Seleção para opção:' valor = 'Fornecedores com "Z3" marcado e com imposto "IW" ' ) TO i_filtros.
    ENDIF.

    IF p_forn IS INITIAL AND p_imp IS INITIAL AND  p_conf IS INITIAL.
      APPEND VALUE #( parametro = 'Seleção para opção:' valor = 'Fornecedores Cooperativas sem "Z3" ' ) TO i_filtros.
    ENDIF.

    IF p_forn IS INITIAL AND p_imp IS NOT INITIAL AND  p_conf IS INITIAL.
      APPEND VALUE #( parametro = 'Seleção para opção:' valor = 'Fornecedores sem "Z3" marcado e com imposto "IW" ' ) TO i_filtros.
    ENDIF.

    IF p_forn IS INITIAL AND p_imp IS INITIAL AND  p_conf IS NOT INITIAL.
      APPEND VALUE #( parametro = 'Seleção para opção:' valor = 'Conferidos                                       ' ) TO i_filtros.
    ENDIF.
  ENDIF.

  IF p_mov = 'X'.
    p_text = 'Movimento - Subcontratos Fretes - Cooperativa'.
  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = CONV #( p_text )
       i_filtros = i_filtros
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.

    gs_layout-sel_mode   = 'A'.
    gs_layout-stylefname = 'CELLSTYLES'.

    IF p_mov = 'X'.

      SET HANDLER lcl_event_receiver=>zm_handle_hotspot_report FOR gob_gui_alv_grid.

      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
          i_save                        = 'A'
          is_variant                    = variante
        CHANGING
          it_outtab                     = git_saida_mov
          it_fieldcatalog               = git_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ELSE.
      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
          i_save                        = 'A'
          is_variant                    = variante
        CHANGING
          it_outtab                     = git_saida_cad
          it_fieldcatalog               = git_fieldcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
    ENDIF.

  ELSE.
    gob_gui_alv_grid->refresh_table_display( ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES:  lit_fieldcat  TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  CLEAR: git_fieldcat.
  IF p_mov = 'X'.
    git_fieldcat = VALUE lit_fieldcat(
     ( fieldname = 'STATUS_RO'       coltext = 'Status Retenção Origem'      col_opt = 'X' just = 'C' )
     ( fieldname = 'STATUS_DC'       coltext = 'Status Documento Contábil'   col_opt = 'X' hotspot = 'X' just = 'C')
     ( fieldname = 'BUKRS'           coltext = 'Empresa VT'                  col_opt = 'X' )
     ( fieldname = 'TDLNR'           coltext = 'Agente Frete'                col_opt = 'X' )
     ( fieldname = 'WERKS'           coltext = 'Filial Remet.'               col_opt = 'X' )
     ( fieldname = 'NAME'            coltext = 'Nome Remet.'                 col_opt = 'X' )
     ( fieldname = 'LIFNR'           coltext = 'Cód. Coleta'                 col_opt = 'X' )
     ( fieldname = 'NAME1'           coltext = 'Nome Coleta'                 col_opt = 'X' )
     ( fieldname = 'REGIO'           coltext = 'UF Coleta'                   col_opt = 'X' )
     ( fieldname = 'KUNNR'           coltext = 'Cód.Destinat.'               col_opt = 'X' )
     ( fieldname = 'NAME1_LR'        coltext = 'Nome Destinat.'              col_opt = 'X' )
     ( fieldname = 'REGIO_LR'        coltext = 'UF Dest.'                    col_opt = 'X' )
     ( fieldname = 'LIFNR_PV'        coltext = 'Cód. Subcon.'                col_opt = 'X' )
     ( fieldname = 'NAME1_PV'        coltext = 'Nome Subcontratado'          col_opt = 'X' )
     ( fieldname = 'STCD1_PV'        coltext = 'CNPJ/CPF Subcon'             col_opt = 'X' )
     ( fieldname = 'KTOKK_PV'        coltext = 'Tipo'                        col_opt = 'X' )
     ( fieldname = 'INDTYP_PV'       coltext = 'Atividade'                   col_opt = 'X' )
     ( fieldname = 'DOCDAT'          coltext = 'Data CT-e'                   col_opt = 'X' )
     ( fieldname = 'NFENUM'          coltext = 'Nr. CT-e'                    col_opt = 'X' )
     ( fieldname = 'DOCNUM'          coltext = 'Docnum CT-e'                 col_opt = 'X' )
     ( fieldname = 'TKNUM'           coltext = 'Doc.Transp'                  col_opt = 'X' )
     ( fieldname = 'FKNUM'           coltext = 'Doc.Custo'                   col_opt = 'X' )
     ( fieldname = 'VBELN'           coltext = 'Ov. Serv.'                   col_opt = 'X' )
     ( fieldname = 'VBELN_FA'        coltext = 'Fatura'                      col_opt = 'X' )
     ( fieldname = 'NFTOT'           coltext = 'Vlr. CT-e'                   col_opt = 'X' )
     ( fieldname = 'BUDAT'           coltext = 'Data_Miro'                   col_opt = 'X' )
     ( fieldname = 'BELNR'           coltext = 'Nr. Miro'                    col_opt = 'X' )
     ( fieldname = 'WRBTR'           coltext = 'Vlr. Miro'                   col_opt = 'X' )
     ( fieldname = 'DOCNUM_32'       coltext = 'Docnum.Entrada'              col_opt = 'X' )
     ( fieldname = 'BUKRS_BK'        coltext = 'Emp Doc. Sub'                col_opt = 'X' )
     ( fieldname = 'OBJ_KEY_SUB'     coltext = 'Doc.Subcontratado'           col_opt = 'X' )
     ( fieldname = 'DMBTR'           coltext = 'Vlr.Subcont'                 col_opt = 'X' )
     ( fieldname = 'KAWRT'           coltext = 'IRF. VI'                     col_opt = 'X' )
     ( fieldname = 'WT_QSSH2_MIR'    coltext = 'IRF Miro'                    col_opt = 'X' )
     ( fieldname = 'WT_QSSH2_SUB'    coltext = 'IRF Subcon.'                 col_opt = 'X' )
     ( fieldname = 'NR_LOTE'         coltext = 'Lote Ajuste'                 col_opt = 'X' )
     ( fieldname = 'DOC_LCTO_AJUSTE' coltext = 'Doc. Ajuste'                 col_opt = 'X' hotspot = 'X' )
     ( fieldname = 'BELNR_ZIB'       coltext = 'Doc. Contabil Ajuste'        col_opt = 'X' hotspot = 'X' )
     ( fieldname = 'QPROZ'           coltext = 'Vlr. Ajuste'                 col_opt = 'X' ) ).

  ELSE.
    IF  p_conf IS INITIAL.
      git_fieldcat = VALUE lit_fieldcat(
         ( fieldname = 'BUKRS'      coltext = 'Empresa'           col_opt = 'X' )
         ( fieldname = 'LIFNR'      coltext = 'Cód.Fornec.'       col_opt = 'X' )
         ( fieldname = 'NAME1'      coltext = 'Nome Fornec.'      col_opt = 'X' )
         ( fieldname = 'STCD1'      coltext = 'CNPJ Fornec.'      col_opt = 'X' )
         ( fieldname = 'STCD3'      coltext = 'Insc.Est.Fornec.'  col_opt = 'X' )
         ( fieldname = 'ORT01'      coltext = 'Cidade Fornec.'    col_opt = 'X' )
         ( fieldname = 'REGIO'      coltext = 'UF Fornec.'        col_opt = 'X' )
         ( fieldname = 'KTOKK'      coltext = 'Grupo'             col_opt = 'X' )
         ( fieldname = 'TXT30'      coltext = 'Descr. Grupo'      col_opt = 'X' )
         ( fieldname = 'INDTYP'     coltext = 'Ativ.'             col_opt = 'X' )
         ( fieldname = 'WITHT'      coltext = 'Categoria IRF'     col_opt = 'X' )
         ( fieldname = 'WT_WITHCD'  coltext = 'Cód. IRF'          col_opt = 'X' ) ).
    ELSE.
      git_fieldcat = VALUE lit_fieldcat(
        ( fieldname = 'LIFNR'        coltext = 'Cód.Fornec.'       col_opt = 'X' )
        ( fieldname = 'NAME1'        coltext = 'Nome Fornec.'      col_opt = 'X' )
        ( fieldname = 'STCD1'        coltext = 'CNPJ Fornec.'      col_opt = 'X' )
        ( fieldname = 'STCD3'        coltext = 'Insc.Est.Fornec.'  col_opt = 'X' )
        ( fieldname = 'ORT01'        coltext = 'Cidade Fornec.'    col_opt = 'X' )
        ( fieldname = 'REGIO'        coltext = 'UF Fornec.'        col_opt = 'X' )
        ( fieldname = 'KTOKK'        coltext = 'Grupo'             col_opt = 'X' )
        ( fieldname = 'DT_REGISTRO'  coltext = 'Dta Registro'      col_opt = 'X' )
        ( fieldname = 'HR_REGISTRO'  coltext = 'Hora Registro'     col_opt = 'X' )
        ( fieldname = 'US_REGISTRO'  coltext = 'Usuário Registro'  col_opt = 'X' ) ).
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
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
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'CONFERIDO'.
      PERFORM fm_set_conferido.
    WHEN 'DESF_CONF'.
      PERFORM fm_desfaz_conf.
    WHEN 'PROCESSAR'.
      PERFORM fm_processar_mov.
    WHEN 'ESTORNAR'.
      PERFORM fm_etornar_mov.
    WHEN 'ATUALIZAR'.
      PERFORM fm_refresh.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
FORM fm_filtros .
  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

  LOOP AT SCREEN.
    git_filtro = VALUE #(
      ( parametro = '' valor = p_bukrs )
    ).
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DESFAZ_CONF
*&---------------------------------------------------------------------*
FORM fm_set_conferido.

  DATA: lwa_zlest0209 TYPE zlest0209,
        lit_zlest0209 TYPE TABLE OF zlest0209.


  gob_gui_alv_grid->get_selected_rows(
  IMPORTING
    et_index_rows = DATA(lit_rows)
).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE git_saida_cad[] INTO DATA(lwa_saida) INDEX lwa_rows-index.

    lwa_zlest0209-lifnr       = lwa_saida-lifnr.
    lwa_zlest0209-dt_registro = sy-datum.
    lwa_zlest0209-hr_registro = sy-uzeit.
    lwa_zlest0209-us_registro = sy-uname.

    APPEND lwa_zlest0209 TO lit_zlest0209.
    MODIFY zlest0209 FROM TABLE lit_zlest0209.
    COMMIT WORK.

    lwa_saida-mark = 'C'.
    MODIFY git_saida_cad INDEX lwa_rows-index  FROM lwa_saida  TRANSPORTING mark.

    CLEAR: lit_zlest0209,  lwa_zlest0209, lwa_saida.

  ENDLOOP.

  DELETE git_saida_cad WHERE mark = 'C'.

  gob_gui_alv_grid->refresh_table_display( ).

  MESSAGE 'Conferência Efetivada' TYPE 'S'.
  EXIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DESFAZ_CONF
*&---------------------------------------------------------------------*
FORM fm_desfaz_conf .

  gob_gui_alv_grid->get_selected_rows(
  IMPORTING
    et_index_rows = DATA(lit_rows) ).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE git_saida_cad[] INTO DATA(lwa_saida) INDEX lwa_rows-index.

    UPDATE zlest0209
       SET eliminado = 'X'
       WHERE  lifnr       = lwa_saida-lifnr
          AND dt_registro = lwa_saida-dt_registro
          AND hr_registro = lwa_saida-hr_registro
          AND us_registro = lwa_saida-us_registro.

    COMMIT WORK.

    lwa_saida-mark = 'D'.
    MODIFY git_saida_cad INDEX lwa_rows-index FROM lwa_saida  TRANSPORTING mark.
    CLEAR: lwa_saida.

  ENDLOOP.

  DELETE git_saida_cad WHERE mark = 'D'.

  gob_gui_alv_grid->refresh_table_display( ).

  MESSAGE 'Conferência Desfeita' TYPE 'S'.
  EXIT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fm_fill_gs_variant .
  gs_variant-report    = sy-repid.
  gs_variant-handle     = '0100'.
  gs_variant-log_group  = abap_false.
  gs_variant-username   = abap_false.
  gs_variant-text       = abap_false.
  gs_variant-dependvars = abap_false.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_PROCESSAR_MOV
*&---------------------------------------------------------------------*
FORM fm_processar_mov .

  DATA: lwa_zglt035     TYPE zglt035,
        lwa_zglt036     TYPE zglt036,
        lwa_tcurr       TYPE tcurr,
        lwa_zfit0171    TYPE zfit0171,
        lwa_j_1bnflin   TYPE j_1bnflin,
        lwa_lfbk        TYPE lfbk,
        lit_zfit0171    TYPE TABLE OF zfit0171,
        lit_zglt036     TYPE TABLE OF zglt036,
        lva_hbkid       TYPE zglt036-hbkid,
        lva_zlsch       TYPE zglt036-zlsch,
        lva_liberado    TYPE char01,
        lva_data        TYPE c LENGTH 10,
        lva_gdatu       TYPE tcurr-gdatu,
        lva_nova_data_i TYPE sy-datum,
        lva_answer      TYPE c,
        lv_dt_lcto_ctb  TYPE zglt035-dt_lcto,
        lva_nova_data_f TYPE sy-datum.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd,
        e_ukurs  	      TYPE ukurs_curr,
        i_data_ctb      TYPE gdatu_inv.

  gob_gui_alv_grid->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lit_rows) ).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  PERFORM f_get_dt_lcto_ctb CHANGING lv_dt_lcto_ctb.
  IF lv_dt_lcto_ctb IS INITIAL.
    MESSAGE 'Data geração Doc. Contabil não informada!' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar o registro contábil?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = lva_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lva_answer EQ '1'.

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    CLEAR: lit_zglt036[], lwa_zglt035, lwa_zfit0171.

    READ TABLE git_saida_mov[] INTO DATA(lwa_saida_mov) INDEX lwa_rows-index.

    CHECK sy-subrc EQ 0.

    IF lwa_saida_mov-status_ro = icon_yellow_light AND lwa_saida_mov-doc_lcto_ajuste IS INITIAL
      AND lwa_saida_mov-qproz > 0.

      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs      = lwa_saida_mov-bukrs_bk
          i_descr_lote = 'IRF Subcontratado'
          i_dep_resp   = '84'
          i_user_resp  = sy-uname
        IMPORTING
          e_num_lote   = lwa_zglt035-lote.

* Para  banco empresa e forma de pagamento  :  V_HBKID e V_ZLSCH
      CLEAR: lva_hbkid,
             lva_zlsch.

      TRY.

          zcl_miro=>get_formapag_banco_empresa( EXPORTING i_bukrs = lwa_saida_mov-bukrs_bk
                                                          i_lifnr = lwa_saida_mov-lifnr_pv
                                                          i_bvtyp = '0001'
                                                 IMPORTING e_banco_empresa   = lva_hbkid
                                                           e_forma_pagamento = lva_zlsch ).
          IF lva_hbkid IS INITIAL.
            MESSAGE s836(sd) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'S'.
            EXIT.
          ENDIF.
        CATCH zcx_miro_exception INTO DATA(ex_miro).  "
          ex_miro->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
          RETURN.
      ENDTRY.

      IF 1 = 2.
        IF lva_hbkid = 'BBD'.
          lva_zlsch = 'D'.
        ELSEIF lva_hbkid = 'BBRA'.
          CLEAR: lwa_lfbk.
          SELECT SINGLE *
            FROM lfbk INTO lwa_lfbk
           WHERE lifnr = lwa_saida_mov-lifnr_pv
             AND bvtyp = '0001'.

          IF sy-subrc = 0.
            IF lwa_lfbk-bankl(3) = '001'.
              lva_zlsch = 'U'.
            ELSE.
              lva_zlsch = 'S'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.



*      CLEAR: lva_nova_data_i, lva_nova_data_f.
*
*      CONCATENATE  lwa_saida_mov-docdat+0(4) lwa_saida_mov-docdat+4(2) '01' INTO lva_nova_data_i.
*
*      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*        EXPORTING
*          day_in            = lva_nova_data_i
*        IMPORTING
*          last_day_of_month = lva_nova_data_f.
*
*      DO.
*
*        IF lwa_tcurr IS INITIAL.
*
*          CLEAR: lva_data, lva_gdatu.
*
*          CONCATENATE lva_nova_data_i+6(2)  '.' lva_nova_data_i+4(2) '.' lva_nova_data_i(4) INTO lva_data.
*
*          CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*            EXPORTING
*              input  = lva_data
*            IMPORTING
*              output = lva_gdatu.
*
*          SELECT SINGLE *
*            FROM tcurr
*            INTO lwa_tcurr
*            WHERE kurst EQ 'B'
*                  AND fcurr EQ 'BRL'
*                  AND tcurr EQ 'USD'
*                  AND gdatu EQ lva_gdatu .
*
*          lva_nova_data_i = lva_nova_data_i + 1.
*
*          IF lva_nova_data_i  >= lva_nova_data_f.
*            EXIT.
*          ENDIF.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDDO.

      CLEAR: lwa_j_1bnflin.
      SELECT SINGLE * FROM j_1bnflin INTO lwa_j_1bnflin
          WHERE docnum  EQ lwa_saida_mov-docnum
            AND itmnum  EQ '000010'.

      lwa_zglt035-bukrs        =  lwa_saida_mov-bukrs_bk.
      lwa_zglt035-dpto_resp    =  '84'.
      lwa_zglt035-moeda_doc    =  'BRL'.
      lwa_zglt035-moeda_forte  =  'USD'.
      lwa_zglt035-blart        =  'LM'.
      lwa_zglt035-bktxt        =  'IRF Subcontratado'.
      lwa_zglt035-xblnr        =  lwa_saida_mov-nfenum.
      lwa_zglt035-budat        =  lv_dt_lcto_ctb.
      lwa_zglt035-bldat        =  lv_dt_lcto_ctb.
      lwa_zglt035-dt_lcto      =  lv_dt_lcto_ctb.
      lwa_zglt035-monat        =  lwa_zglt035-dt_lcto+4(2).
      lwa_zglt035-gjahr        =  lwa_zglt035-dt_lcto+0(4).
      lwa_zglt035-usnam        =  sy-uname.
      lwa_zglt035-dt_entrada   =  sy-datum.
      lwa_zglt035-hr_entrada   =  sy-uzeit.

      CREATE OBJECT obj_zcl_util_sd.

      i_data_ctb = lwa_zglt035-dt_lcto.
      obj_zcl_util_sd->set_data(  EXPORTING i_data = i_data_ctb ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = lwa_zglt035-moeda_doc ).




      lwa_zglt036-seqitem  = 1.
      lwa_zglt036-hkont    = lwa_saida_mov-lifnr_pv.
      CONCATENATE 'IRF Subcontratado CTR' lwa_saida_mov-nfenum INTO lwa_zglt036-sgtxt SEPARATED BY space.
      lwa_zglt036-bschl    = '21'.
      lwa_zglt036-hbkid    = lva_hbkid.
      lwa_zglt036-zlsch    = lva_zlsch.
      lwa_zglt036-bvtyp    = '0001'.

      lwa_zglt036-dt_vct   = sy-datum + 3.

      PERFORM fm_valida_data USING lwa_zglt036-dt_vct.

      lwa_zglt036-gsber    = lwa_j_1bnflin-werks.
      lwa_zglt036-vlr_moeda_doc = lwa_saida_mov-qproz.

      IF lwa_zglt035-moeda_forte EQ lwa_zglt035-moeda_doc.
        lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = lwa_zglt035-moeda_forte ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
          lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc / abs( e_ukurs ).
        ELSE.
          lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc * abs( e_ukurs ).
        ENDIF.
      ENDIF.

      IF ( lwa_zglt036-vlr_moeda_forte = 0 ) AND
         ( lwa_zglt036-vlr_moeda_doc   > 0 ) AND
         ( lwa_zglt036-vlr_moeda_doc   < 1 ).
        lwa_zglt036-vlr_moeda_forte = '0.01'.
      ENDIF.

      APPEND lwa_zglt036 TO lit_zglt036.

      CLEAR: lwa_zglt036-hbkid,
             lwa_zglt036-zlsch,
             lwa_zglt036-bvtyp.

      lwa_zglt036-seqitem  = 2.
      lwa_zglt036-hkont    = '0000213700'.
      CONCATENATE 'IRF Subcontratado CTR' lwa_saida_mov-nfenum INTO lwa_zglt036-sgtxt SEPARATED BY space.
      lwa_zglt036-bschl    = '50'.
      lwa_zglt036-gsber         = lwa_j_1bnflin-werks.
      lwa_zglt036-vlr_moeda_doc = lwa_saida_mov-qproz.

      IF lwa_zglt035-moeda_forte EQ lwa_zglt035-moeda_doc.
        lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc.
      ELSE.
        obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = lwa_zglt035-moeda_forte ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = e_ukurs ).
        IF e_ukurs LT 0.
          lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc / abs( e_ukurs ).
        ELSE.
          lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_doc * abs( e_ukurs ).
        ENDIF.
      ENDIF.

      IF ( lwa_zglt036-vlr_moeda_forte = 0 ) AND
         ( lwa_zglt036-vlr_moeda_doc   > 0 ) AND
         ( lwa_zglt036-vlr_moeda_doc   < 1 ).
        lwa_zglt036-vlr_moeda_forte = '0.01'.
      ENDIF.

      APPEND lwa_zglt036 TO lit_zglt036.

      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        EXPORTING
          i_arredonda = abap_true
        CHANGING
          i_zglt036   = lit_zglt036
          i_zglt035   = lwa_zglt035 ).

      CLEAR: lva_liberado.
      CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
        EXPORTING
          p_num_lote = lwa_zglt035-lote
        IMPORTING
          p_liberado = lva_liberado.

      CHECK lva_liberado EQ abap_true.

      MOVE: lwa_zglt035-lote     TO lwa_saida_mov-nr_lote,
            lwa_zglt035-doc_lcto TO lwa_saida_mov-doc_lcto_ajuste.

      MODIFY git_saida_mov INDEX lwa_rows-index FROM lwa_saida_mov
      TRANSPORTING nr_lote doc_lcto_ajuste.

      lwa_zfit0171-tknum           = lwa_saida_mov-tknum.
      lwa_zfit0171-fknum           = lwa_saida_mov-fknum.
      lwa_zfit0171-lfa1_pv         = lwa_saida_mov-lifnr_pv.
      lwa_zfit0171-lfa1_indtyp     = lwa_saida_mov-indtyp_pv.
      lwa_zfit0171-nr_lote         = lwa_saida_mov-nr_lote.
      lwa_zfit0171-doc_lcto_ajuste = lwa_saida_mov-doc_lcto_ajuste.
      lwa_zfit0171-dt_lcto_ajuste  = lv_dt_lcto_ctb.

      APPEND lwa_zfit0171 TO lit_zfit0171.
      MODIFY zfit0171  FROM TABLE lit_zfit0171.
      COMMIT WORK.

      CLEAR: lwa_saida_mov,
             lwa_zglt035,
             lwa_zglt036,
             lwa_zfit0171.


    ENDIF.
  ENDLOOP.

  gob_gui_alv_grid->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ETORNAR_MOV
*&---------------------------------------------------------------------*
FORM fm_etornar_mov .

  DATA: lva_obj_key          TYPE zib_contabil_chv-obj_key,
        lva_stblg            TYPE bkpf-stblg,
        lwa_zib_contabil_chv TYPE zib_contabil_chv,
        lwa_bsak             TYPE bsak,
        lwa_zfit0171         TYPE zfit0171,
        lwa_zglt035          TYPE zglt035,
        lit_zfit0171         TYPE TABLE OF zfit0171,
        lwa_mensagem(50).

  gob_gui_alv_grid->get_selected_rows(
  IMPORTING
    et_index_rows = DATA(lit_rows) ).

  CHECK ( lit_rows[] IS NOT INITIAL ).

  LOOP AT lit_rows[] INTO DATA(lwa_rows).

    READ TABLE git_saida_mov[] ASSIGNING FIELD-SYMBOL(<fs_saida_mov>) INDEX lwa_rows-index.

    CHECK sy-subrc EQ 0.

    CLEAR: lwa_zfit0171, lit_zfit0171[] .
    SELECT SINGLE * FROM zfit0171 INTO lwa_zfit0171
     WHERE tknum           = <fs_saida_mov>-tknum
       AND fknum           = <fs_saida_mov>-fknum
       AND lfa1_pv         = <fs_saida_mov>-lifnr_pv
       AND lfa1_indtyp     = <fs_saida_mov>-indtyp_pv.

    IF sy-subrc NE 0.
      MESSAGE s000(z01) WITH 'Documento não foi processado' DISPLAY LIKE 'E'.
      CONTINUE.
    ENDIF.

    IF lwa_zfit0171-doc_lcto_ajuste IS NOT INITIAL.

      CLEAR: lwa_zglt035.
      SELECT SINGLE *
       FROM zglt035
       INTO lwa_zglt035
     WHERE doc_lcto EQ lwa_zfit0171-doc_lcto_ajuste.

      IF sy-subrc NE 0 OR ( sy-subrc EQ 0 AND lwa_zglt035-loekz EQ abap_true ).
        CLEAR: lwa_zfit0171-doc_lcto_ajuste,
               lwa_zfit0171-dt_lcto_ajuste,
               lwa_zfit0171-nr_lote.


        CLEAR: <fs_saida_mov>-doc_lcto_ajuste,
               <fs_saida_mov>-dt_lcto_ajuste,
               <fs_saida_mov>-nr_lote,
               <fs_saida_mov>-belnr_zib.

        APPEND lwa_zfit0171 TO lit_zfit0171.

        MODIFY zfit0171  FROM TABLE lit_zfit0171.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    IF <fs_saida_mov>-belnr_zib IS NOT INITIAL.

      CLEAR: lva_obj_key.
      CONCATENATE 'ZGL17' lwa_zfit0171-doc_lcto_ajuste lwa_zfit0171-dt_lcto_ajuste+0(4) INTO lva_obj_key.

* - Para buscar doc contábil
      CLEAR: lwa_zib_contabil_chv.
      SELECT SINGLE * FROM zib_contabil_chv INTO lwa_zib_contabil_chv
          WHERE obj_key EQ lva_obj_key.

* - Para buscar doc contábil com erro
      CLEAR: lwa_bsak.
      SELECT SINGLE * FROM bsak INTO lwa_bsak
       WHERE bukrs       = lwa_zib_contabil_chv-bukrs
            AND gjahr    = lwa_zib_contabil_chv-gjahr
            AND belnr    = lwa_zib_contabil_chv-belnr.

      IF lwa_bsak IS INITIAL. " Estorno Permitido.

* - Estorno documento contábil
        SUBMIT z_fb08_zgl042 WITH p_obj = lva_obj_key
        AND RETURN.

        CLEAR:lva_stblg.
        SELECT SINGLE stblg
          FROM zib_contabil_chv
        INNER JOIN bkpf
          ON  bkpf~bukrs = zib_contabil_chv~bukrs
            AND bkpf~belnr = zib_contabil_chv~belnr
            AND bkpf~gjahr = zib_contabil_chv~gjahr
            INTO lva_stblg
        WHERE zib_contabil_chv~obj_key = lva_obj_key.

        IF lva_stblg IS INITIAL. "Não estornou
          EXIT.
        ELSE.
* - Estorno do documento 'Doc_Lcto'
          CLEAR: lwa_zglt035.
          SELECT SINGLE *
           FROM zglt035
           INTO lwa_zglt035
         WHERE doc_lcto EQ <fs_saida_mov>-nr_lote.

          lwa_zglt035-loekz =  'X'.

          MODIFY zglt035 FROM lwa_zglt035.
          COMMIT WORK.

* - Limpa 0171.

          CLEAR: lwa_zfit0171-doc_lcto_ajuste,
                 lwa_zfit0171-dt_lcto_ajuste,
                 lwa_zfit0171-nr_lote.

          CLEAR: <fs_saida_mov>-doc_lcto_ajuste,
                 <fs_saida_mov>-dt_lcto_ajuste,
                 <fs_saida_mov>-nr_lote,
                 <fs_saida_mov>-belnr_zib.

          APPEND lwa_zfit0171 TO lit_zfit0171.

          MODIFY zfit0171  FROM TABLE lit_zfit0171.
          COMMIT WORK.

        ENDIF.

      ELSE.
        CONCATENATE 'Documento' <fs_saida_mov>-belnr_zib 'esta compensado. Impossível estornar' INTO lwa_mensagem SEPARATED BY space.
        MESSAGE lwa_mensagem TYPE 'I'.
      ENDIF.

    ELSE.

      MESSAGE s000(z01) WITH 'Documento não foi processado' DISPLAY LIKE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0100
*&---------------------------------------------------------------------*
FORM user_command_0100 USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE lvc_s_col
                             p_es_eow_no TYPE lvc_s_roid.

  DATA: lva_chave_zib TYPE zib_contabil_chv-obj_key,
        lva_gjahr     TYPE zib_contabil_chv-gjahr,
        lva_lote      TYPE zglt034-lote.

  READ TABLE git_saida_mov[] INTO DATA(lwa_saida_mov) INDEX e_row_id-index.

  CHECK ( sy-subrc = 0 ).

  CASE p_e_column_id-fieldname.
    WHEN 'STATUS_DC'.
      IF lwa_saida_mov-status_ro = icon_red_light.
        CLEAR: lva_chave_zib.
        CONCATENATE 'ZGL17' gwa_saida_mov-doc_lcto_ajuste gwa_saida_mov-doc_lcto_ajuste+0(4) INTO lva_chave_zib.

        PERFORM fm_exibe_erro_zib USING lva_chave_zib.
      ENDIF.

    WHEN  'DOC_LCTO_AJUSTE'  .
      CHECK lwa_saida_mov-doc_lcto_ajuste IS NOT INITIAL.

      SET PARAMETER ID 'BLN' FIELD lwa_saida_mov-doc_lcto_ajuste.
      SET PARAMETER ID 'LOT' FIELD lva_lote.
      CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

    WHEN   'BELNR_ZIB'.
      IF lwa_saida_mov-belnr_zib IS NOT INITIAL.

        lva_gjahr = lwa_saida_mov-doc_lcto_ajuste+0(4).

        SET PARAMETER ID 'BLN' FIELD lwa_saida_mov-belnr_zib.
        SET PARAMETER ID 'BUK' FIELD lwa_saida_mov-bukrs_bk.
        SET PARAMETER ID 'GJR' FIELD lva_gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ERRO_ZIB
*&---------------------------------------------------------------------*
FORM fm_exibe_erro_zib  USING    p_lva_chave_zib.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  CHECK ( p_lva_chave_zib IS NOT INITIAL ).

  SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
    WHERE obj_key = @p_lva_chave_zib.

  IF ( sy-subrc = 0 ).
    cl_demo_output=>new(
      )->begin_section( `ZIB_CONTABIL_ERR:`
      )->write_text( |Erros encontrados na crição do documento: \n|
      ")->WRITE_DATA( SY-DATUM
      )->write_data( lit_zib_err[]
      )->end_section(
      )->display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_REFRESH
*&---------------------------------------------------------------------*

FORM fm_refresh .
  CLEAR: git_saida_mov[].
  PERFORM fm_start_of_selection.
  PERFORM fm_criar_objetos.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_VALIDA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_ZGLT036_DT_VCT  text
*      <--P_VL_VALID_DT  text
*----------------------------------------------------------------------*
FORM fm_valida_data  USING    p_data  TYPE sy-datum.

  DATA: lva_sydatum  TYPE sy-datum,
        lva_n_uteis  TYPE sy-index,
        lva_data_val TYPE sy-datum.

  DATA: lit_sab_dom_fer TYPE TABLE OF iscal_day WITH HEADER LINE.

  lva_sydatum = sy-datum.
  ADD 3 TO lva_sydatum.

  CLEAR: lit_sab_dom_fer[], lva_n_uteis.
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      factory_calendar           = 'ZF'
      date_from                  = sy-datum
      date_to                    = p_data
    TABLES
      holidays                   = lit_sab_dom_fer
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.

  DESCRIBE TABLE lit_sab_dom_fer LINES lva_n_uteis.

  IF lva_n_uteis > 0.
    ADD lva_n_uteis TO p_data.
  ENDIF.


ENDFORM.

FORM f_get_dt_lcto_ctb  CHANGING p_dt_lcto_ctb TYPE zglt035-dt_lcto.

  DATA: lt_fields  TYPE ty_sval.
  DATA: ls_fields  LIKE LINE OF lt_fields.
  DATA: lv_return  TYPE c.

  CLEAR: p_dt_lcto_ctb.

  ls_fields-tabname   = 'ZGLT035'.
  ls_fields-fieldtext = 'Data Lcto Contabil'.
  ls_fields-field_obl = abap_true.
  ls_fields-fieldname = 'DT_LCTO'.
  ls_fields-value     = sy-datum.
  APPEND ls_fields TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Informe a data para geração do documento contábil'
    IMPORTING
      returncode      = lv_return
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.


  READ TABLE lt_fields INTO DATA(lwa_field_dt_lcto) WITH KEY fieldname = 'DT_LCTO'.
  IF sy-subrc EQ 0.
    p_dt_lcto_ctb =  lwa_field_dt_lcto-value.
  ENDIF.


ENDFORM.
