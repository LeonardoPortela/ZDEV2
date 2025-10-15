*&---------------------------------------------------------------------*
*& Report  ZSDR0118
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0118.

TABLES: vbrk.

TYPES: BEGIN OF ty_vbrk,
         vbeln  TYPE vbrk-vbeln,
         vkorg  TYPE vbrk-vkorg,
         fkart  TYPE vbrk-fkart,
         fkdat  TYPE vbrk-fkdat,
         refkey TYPE j_1bnflin-refkey,
         vbelv  TYPE vbfa-vbelv,
         ernam  TYPE vbrk-ernam,
         erzet  TYPE vbrk-erzet,
         erdat  TYPE vbrk-erdat,
       END OF ty_vbrk,

       BEGIN OF ty_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         ntgew TYPE vbrp-ntgew,
         werks TYPE vbrp-werks,
         gsber TYPE vbrp-gsber,
         vkbur TYPE vbrp-vkbur,
         matnr TYPE vbrp-matnr,
         matkl TYPE vbrp-matkl,
         vgbel TYPE vbrp-vgbel,
       END OF ty_vbrp,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         vbtyp_n TYPE vbfa-vbtyp_n,
       END OF ty_vbfa,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
         charg  TYPE j_1bnflin-charg,
         meins  TYPE j_1bnflin-meins,
         menge  TYPE j_1bnflin-menge,
       END OF ty_j_1bnflin,

       BEGIN OF ty_j_1bnfe_active,
         docnum TYPE j_1bnfe_active-docnum,
       END OF ty_j_1bnfe_active,

       BEGIN OF ty_j_1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
         parid  TYPE j_1bnfdoc-parid,
         partyp TYPE j_1bnfdoc-partyp,
       END OF ty_j_1bnfdoc,

       BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         vstel TYPE likp-vstel,
         fkarv TYPE likp-fkarv,
       END OF ty_likp,

       BEGIN OF ty_lips,
         vbeln TYPE lips-vbeln,
         charg TYPE lips-charg,
       END OF ty_lips.

DATA: tg_vbrk           TYPE TABLE OF ty_vbrk           WITH HEADER LINE,
      tg_vbrp           TYPE TABLE OF ty_vbrp           WITH HEADER LINE,
      tg_likp           TYPE TABLE OF ty_likp           WITH HEADER LINE,
      tg_zsdt0023       TYPE TABLE OF zsdt0023          WITH HEADER LINE,
      tg_vbrk_estorno   TYPE TABLE OF ty_vbrk           WITH HEADER LINE,
      tg_j_1bnflin      TYPE TABLE OF ty_j_1bnflin      WITH HEADER LINE,
      tg_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active WITH HEADER LINE,
      tg_j_1bnfdoc      TYPE TABLE OF ty_j_1bnfdoc      WITH HEADER LINE,
      tg_vbfa_estorno   TYPE TABLE OF ty_vbfa           WITH HEADER LINE,
      tg_0251           TYPE TABLE OF zsdt0251          WITH HEADER LINE,
      tg_zsdt0249       TYPE TABLE OF zsdt0249          WITH HEADER LINE.

DATA: wl_0251 TYPE zsdt0251.

DATA: vg_fkdat_start         TYPE vbrk-fkdat,
      vg_fkdat_estorno_start TYPE erdat,
      vg_job                 TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK a1.
SELECT-OPTIONS:
    p_vkorg  FOR vbrk-vkorg  NO INTERVALS NO-EXTENSION NO-DISPLAY,
    p_fkdat  FOR vbrk-fkdat NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END  OF BLOCK a1.

START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1 AND ( p_fkdat-low IS INITIAL ).
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  PERFORM: f_selecionar_dados,
           f_processar_dados.


FORM f_selecionar_dados.

  PERFORM: f_iniciar_variaveis.

  vg_fkdat_start = p_fkdat-low.

  IF vg_fkdat_start IS INITIAL.
    vg_fkdat_start = sy-datum - 30.
  ENDIF.

  CHECK vg_fkdat_start IS NOT INITIAL.

*-----------------------------------------------------------------------------------------------------*
* Carrega Faturas Estornadas
*-----------------------------------------------------------------------------------------------------*
  vg_fkdat_estorno_start = vg_fkdat_start - 10.

  SELECT vb~vbelv ftn~fkdat INTO CORRESPONDING FIELDS OF TABLE tg_vbrk_estorno
    FROM vbfa AS vb INNER JOIN vbrk AS ftn ON vb~vbeln = ftn~vbeln
   WHERE vb~vbtyp_n EQ 'N'
     AND ftn~erdat  GE  vg_fkdat_estorno_start
     AND EXISTS ( SELECT *
                    FROM zsdt0251 AS c
                   WHERE c~vbeln_vf EQ vb~vbelv
                     AND c~loekz    EQ abap_false ) AND FTN~DRAFT = SPACE .

*-----------------------------------------------------------------------------------------------------*
* Carrega Faturas
*-----------------------------------------------------------------------------------------------------*

  "Cabeçalho Faturas
  SELECT *
    FROM vbrk APPENDING CORRESPONDING FIELDS OF TABLE tg_vbrk
   WHERE erdat   GE vg_fkdat_start
     AND fkart   EQ 'ZIND' AND DRAFT = SPACE .

  CHECK tg_vbrk[] IS NOT INITIAL.

  LOOP AT tg_vbrk.
    tg_vbrk-refkey = tg_vbrk-vbeln.
    MODIFY tg_vbrk.
  ENDLOOP.

  "Itens Doc. Fiscal
  SELECT *
    FROM j_1bnflin APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnflin
     FOR ALL ENTRIES IN tg_vbrk
   WHERE refkey EQ tg_vbrk-refkey.

  CHECK tg_j_1bnflin[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum     EQ tg_j_1bnflin-docnum
     AND cancel     EQ abap_false
     AND docsta     EQ '1'
     AND scssta     NE '2'.

  CHECK tg_j_1bnfe_active[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfdoc INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
     FOR ALL ENTRIES IN tg_j_1bnfe_active
   WHERE docnum EQ tg_j_1bnfe_active-docnum.

*-CS2021000386 - 28.04.2021 - JT - inicio
* IF tg_j_1bnfdoc[] IS NOT INITIAL.
*   SELECT *
*     FROM zsdt0249 INTO TABLE tg_zsdt0249
*      FOR ALL ENTRIES IN tg_j_1bnflin
*    WHERE docnum EQ tg_j_1bnflin-docnum.
* ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

  "Itens Faturas
  SELECT *
    FROM vbrp APPENDING CORRESPONDING FIELDS OF TABLE tg_vbrp
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbeln EQ tg_vbrk-vbeln.

  CHECK tg_vbrp[] IS NOT INITIAL.

  "Cabeçalho Remessas
  SELECT *
    FROM likp APPENDING CORRESPONDING FIELDS OF TABLE tg_likp
     FOR ALL ENTRIES IN tg_vbrp
   WHERE vbeln EQ tg_vbrp-vgbel.

  CHECK tg_likp[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0023 INTO TABLE tg_zsdt0023
     FOR ALL ENTRIES IN tg_likp
   WHERE vbeln EQ tg_likp-vbeln.

  CHECK tg_zsdt0023[] IS NOT INITIAL.

  "Fluxo de Documentos Estornados
  SELECT *
    FROM vbfa APPENDING CORRESPONDING FIELDS OF TABLE tg_vbfa_estorno
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbelv   EQ tg_vbrk-vbeln
     AND vbtyp_n EQ 'N'. "Estorno Fatura

ENDFORM.

FORM f_processar_dados.

*-------------------------------------------------------------------------------------------------------------*
* Marcar como Eliminado Faturas Estornadas e já gravadas na tabela
*-------------------------------------------------------------------------------------------------------------*
  LOOP AT tg_vbrk_estorno.

    SELECT SINGLE *
      FROM zsdt0251 INTO @DATA(wl_0251)
     WHERE vbeln_vf EQ @tg_vbrk_estorno-vbelv.

    CHECK sy-subrc EQ 0.

    wl_0251-loekz = 'X'.
    MODIFY zsdt0251 FROM wl_0251.

    COMMIT WORK.
  ENDLOOP.

*-------------------------------------------------------------------------------------------------------------*
* Incluão de Registros
*-------------------------------------------------------------------------------------------------------------*

  LOOP AT tg_vbrk. " Fatura Cabeçalho

    READ TABLE tg_vbfa_estorno WITH KEY vbelv = tg_vbrk-vbeln. "Check se fatura foi estornada
    CHECK sy-subrc NE 0.

    READ TABLE tg_vbrp WITH KEY vbeln = tg_vbrk-vbeln. "Item Fatura
    CHECK sy-subrc EQ 0.

    READ TABLE tg_likp WITH KEY vbeln = tg_vbrp-vgbel. "Remessa
    CHECK sy-subrc EQ 0.

    READ TABLE tg_zsdt0023 WITH KEY vbeln = tg_likp-vbeln.
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnflin WITH KEY refkey = tg_vbrk-refkey. "Item Doc. Fiscal
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnflin-docnum. "Dados NF-e
    CHECK sy-subrc EQ 0.

    READ TABLE tg_j_1bnfdoc WITH KEY docnum = tg_j_1bnflin-docnum. "Dados NF-e
    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zsdt0251 INTO @DATA(wl_0251_ck)
     WHERE docnum EQ @tg_j_1bnfe_active-docnum.

    CHECK sy-subrc NE 0.

    CLEAR: wl_0251.

    wl_0251-docnum          = tg_j_1bnfe_active-docnum.
    wl_0251-vbeln_vl        = tg_likp-vbeln.
    wl_0251-vbeln_vf        = tg_vbrk-vbeln.
    wl_0251-fkarv           = tg_likp-fkarv.
    wl_0251-fkart           = tg_vbrk-fkart.
    wl_0251-branch          = tg_likp-vstel.
    wl_0251-werks_v         = tg_zsdt0023-werks_v.
*-CS2021000386 - 28.04.2021 - JT - inicio
    wl_0251-lgort_v         = tg_zsdt0023-lgort_v. "'PO58'. "TG_ZSDT0023-LGORT_V. "Regra temporaria
*-CS2021000386 - 28.04.2021 - JT - fim
    wl_0251-branch_destino  = tg_j_1bnfdoc-parid+6(4).
    wl_0251-charg           = tg_j_1bnflin-charg.

    CASE tg_j_1bnflin-meins.
      WHEN 'KG'.
        wl_0251-qtde_total  = tg_j_1bnflin-menge.
        wl_0251-qtde_saldo  = wl_0251-qtde_total.
      WHEN 'TO'.
        wl_0251-qtde_total  = tg_j_1bnflin-menge * 1000.
        wl_0251-qtde_saldo  = wl_0251-qtde_total.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    wl_0251-ernam           = tg_vbrk-ernam.
    wl_0251-erzet           = tg_vbrk-erzet.
    wl_0251-erdat           = tg_vbrk-erdat.
    wl_0251-dt_registro     = sy-datum.
    wl_0251-hr_registro     = sy-uzeit.
    wl_0251-us_registro     = sy-uname.

    INSERT zsdt0251 FROM wl_0251.

  ENDLOOP.

ENDFORM.

FORM f_iniciar_variaveis .

  CLEAR: tg_vbrk[],
         tg_vbrp[],
         tg_likp[],
         tg_j_1bnflin[],
         tg_j_1bnfe_active[],
         tg_j_1bnfdoc[],
         tg_vbfa_estorno[],
         tg_vbrk_estorno[],
         tg_zsdt0023[].

  CLEAR: vg_fkdat_start,
         vg_fkdat_estorno_start.

ENDFORM.
