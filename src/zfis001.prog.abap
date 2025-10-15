************************************************************************
*         P R O J E T O  C R E S C E R   -   M A G G I                 *
*                                                                      *
************************************************************************
* Consultoria ...:                                                     *
* Responsável ...: Eduardo Ruttkowski Tavares       - Consultor ABAP   *
* Data desenv ...: 22.02.2008                                          *
* Tipo de prg ...:                                                     *
* Objetivo    ...:                                                     *
*                                                                      *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
*                                                                      *
*                                                                      *
************************************************************************

REPORT  zfis001.
*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: with_item, bkpf, bsak, v_t059z, rseg.

TYPES: BEGIN OF ty_saida_rseg.
TYPES: belnr TYPE belnr_d,
       gjahr TYPE gjahr.
TYPES: END OF ty_saida_rseg.

TYPES: BEGIN OF ty_saida_j1bnfdoc,
         docnum TYPE j_1bnfdoc-docnum,
       END OF ty_saida_j1bnfdoc.

*----------------------------------------------------------------------*
* Tabelas internas
*----------------------------------------------------------------------*
DATA: BEGIN OF t_with_item OCCURS 0,
        bukrs     LIKE with_item-bukrs,
        belnr     LIKE with_item-belnr,
        gjahr     LIKE with_item-gjahr,
        buzei     LIKE with_item-buzei,
        witht     LIKE with_item-witht,
        wt_acco   LIKE with_item-wt_acco,
        wt_qsshh  LIKE with_item-wt_qsshh,
        wt_qbshh  LIKE with_item-wt_qbshh,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
        wt_qbsh2  LIKE with_item-wt_qbsh2,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM
        wt_withcd LIKE with_item-wt_withcd, "MODIFICAÇÂO 13/09/2016
*        WITHT    LIKE T059U-WITHT,
        text40    LIKE t059u-text40,
      END OF t_with_item.

DATA: BEGIN OF t_bkpf OCCURS 0,
        bukrs LIKE bkpf-bukrs,
        belnr LIKE bkpf-belnr,
        gjahr LIKE bkpf-gjahr,
        budat LIKE bkpf-budat,
        bldat LIKE bkpf-bldat,
        brnch LIKE bkpf-brnch,
        xblnr LIKE bkpf-xblnr,
        awkey LIKE bkpf-awkey,
        usnam TYPE bkpf-usnam,
        awk10 TYPE c LENGTH 10,
      END OF t_bkpf.

DATA: BEGIN OF t_saida OCCURS 0,
        mark,
        bukrs           LIKE with_item-bukrs,
        gsber           LIKE bsak-gsber,
        name            LIKE j_1bbranch-name,
        budat           LIKE bkpf-budat,
        augdt           LIKE bsak-augdt,
        belnr           LIKE with_item-belnr,
        stcd2           LIKE lfa1-stcd1,
        lifnr           LIKE lfa1-lifnr,
        wt_acco         LIKE with_item-wt_acco,
        name1           LIKE lfa1-name1,
        stenr           LIKE lfa1-stenr,
        regio           LIKE lfa1-regio,
        qscod           LIKE v_t059z-qscod,
        rmwwr           LIKE rbkp-rmwwr,
        base_imposto    LIKE with_item-wt_qsshh, "Base do Imposto
        wt_qsshh        LIKE with_item-wt_qsshh,
        wt_qbshh        LIKE with_item-wt_qbshh,
        bldat           LIKE bkpf-bldat,
        text40          LIKE t059u-text40,
        nfnum           LIKE bkpf-xblnr,
        witht           LIKE with_item-witht,
        ktext1          LIKE esll-ktext1,
        gjahr           LIKE bkpf-gjahr,
        ebeln           TYPE ebeln,
        ort01           TYPE lfa1-ort01,
        usnam           TYPE bkpf-usnam,
        perimp          TYPE rbkp-rmwwr,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
        wt_qbsh2        TYPE with_item-wt_qbsh2,
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM
        j_1bnftype      TYPE rbkp-j_1bnftype, "MODIFICAÇÂO 13/09/2016
        stblg           TYPE rbkp-stblg, "MODIFICAÇÂO 13/09/2016
        in_rbkp         TYPE char1, "MODIFICAÇÂO 13/09/2016
        dia_vencimento  TYPE zfit0140-dia_vencimento,
        nbm             TYPE j_1bnflin-nbm,
        "#108338 - Alteração Guilherme Rabelo - Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
        ebeln_p         TYPE rseg-ebeln, "pedido
        awkey           TYPE bkpf-awkey, "miro
        anexo           TYPE icon_d,
        hkont           TYPE bsak-hkont, "conta razao
        xblnr           TYPE bkpf-xblnr, "fiscal
        obs(100)        TYPE c, "obs
        imp_gerado(200) TYPE c, "icon_d, "Contab Imp Gerado
        imp_correto     TYPE bseg-dmbtr, "Perc. Imp. Correto
        vl_correto      TYPE bseg-dmbtr, "Vlr Imp. Correto
        deb_cre         TYPE bseg-dmbtr, "DEB/CRED
        doc_lanc        TYPE zglt035-doc_lcto,
        "#108338 - Alteração Guilherme Rabelo - Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
      END OF t_saida.

DATA: BEGIN OF t_j_1bbranch OCCURS 0,
        bukrs  LIKE j_1bbranch-bukrs,
        branch LIKE j_1bbranch-branch, "Local de negócios
        name   LIKE j_1bbranch-name,
      END OF t_j_1bbranch.

DATA: BEGIN OF t_lfa1 OCCURS 0,
        lifnr LIKE lfa1-lifnr,
        stcd2 LIKE lfa1-stcd2,
        stcd1 LIKE lfa1-stcd1,
        name1 LIKE lfa1-name1,
        stenr LIKE lfa1-stenr,
        stkzn LIKE lfa1-stkzn,
        ort01 LIKE lfa1-ort01,
        regio LIKE lfa1-regio,
        land1 LIKE lfa1-land1,
      END OF t_lfa1.

DATA: BEGIN OF t_bsak OCCURS 0,
        bukrs LIKE bsak-bukrs,
        lifnr LIKE bsak-lifnr,
        belnr LIKE bsak-belnr,
        gsber LIKE bsak-gsber,
        augdt LIKE bsak-augdt,
        hkont LIKE bsak-hkont,  "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
      END OF t_bsak.

DATA: BEGIN OF t_bsak2 OCCURS 0,
        bukrs LIKE bsak-bukrs,
        lifnr LIKE bsak-lifnr,
        belnr LIKE bsak-belnr,
        gsber LIKE bsak-gsber,
        augdt LIKE bsak-augdt,
      END OF t_bsak2.

DATA: BEGIN OF t_t059z OCCURS 0,
        witht LIKE t059z-witht,
        qscod LIKE t059z-qscod,
      END OF t_t059z.

DATA: BEGIN OF t_doc OCCURS 0,
        docnum LIKE j_1bnfdoc-docnum,
        nfnum  LIKE j_1bnfdoc-nfnum,
        pstdat LIKE j_1bnfdoc-pstdat,
        belnr  LIKE j_1bnfdoc-belnr,
        bukrs  LIKE j_1bnfdoc-bukrs,
        gjahr  LIKE j_1bnfdoc-gjahr,
      END OF t_doc.

DATA: BEGIN OF t_rseg OCCURS 0,
        bukrs  LIKE rseg-bukrs,
        belnr  LIKE rseg-belnr,
        gjahr  LIKE rseg-gjahr,
        ebeln  LIKE rseg-ebeln,
        ebelp  LIKE rseg-ebelp,
        mwskz  LIKE rseg-mwskz,
        refkey LIKE j_1bnflin-refkey,
      END OF t_rseg.

DATA: BEGIN OF t_rbkp OCCURS 0,
        belnr      LIKE rbkp-belnr,
        gjahr      LIKE rbkp-gjahr,
        rmwwr      LIKE rbkp-rmwwr,
        budat      LIKE rbkp-budat,
        bldat      LIKE rbkp-bldat,
        j_1bnftype LIKE rbkp-j_1bnftype, "MODIFICAÇÂO 13/09/20
        stblg      LIKE rbkp-stblg, "MODIFICAÇÂO 13/09/20
      END OF t_rbkp.

DATA: BEGIN OF t_ekpo OCCURS 0,
        ebeln  LIKE ekpo-ebeln,
        ebelp  LIKE ekpo-ebelp,
        navnw  LIKE ekpo-navnw,
        packno LIKE ekpo-packno,
      END OF t_ekpo.

DATA: BEGIN OF t_esll OCCURS 0,
        packno     LIKE esll-packno,
        sub_packno LIKE esll-sub_packno,
      END OF t_esll.

DATA: BEGIN OF t_esllk OCCURS 0,
        packno LIKE esll-packno,
        ktext1 LIKE esll-ktext1,
      END OF t_esllk.

DATA: BEGIN OF t_t007s OCCURS 0,
        kalsm LIKE t007s-kalsm,
        mwskz LIKE t007s-mwskz,
        text1 LIKE t007s-text1,
      END OF t_t007s.

DATA: BEGIN OF t_zib_chv OCCURS 0,
        obj_key  LIKE zib_contabil_chv-obj_key,
        belnr    LIKE zib_contabil_chv-belnr,
        bukrs    LIKE zib_contabil_chv-bukrs,
        gjahr    LIKE zib_contabil_chv-gjahr,
        doc_lcto TYPE zglt081-doc_lcto,
      END OF t_zib_chv.

DATA: BEGIN OF t_zglt081 OCCURS 0,
        obj_key LIKE zib_contabil_chv-obj_key,
        lifnr   TYPE zglt080-lifnr.
        INCLUDE  STRUCTURE zglt081.
DATA: END OF t_zglt081.

DATA: BEGIN OF t_zglt082 OCCURS 0.
        INCLUDE STRUCTURE zglt082.
DATA: END OF t_zglt082.

DATA: BEGIN OF t_j_1bnfdoc OCCURS 0,
        docnum TYPE j_1bnfdoc-docnum,
        nftype TYPE j_1bnfdoc-nftype,
        nftot  TYPE j_1bnfdoc-nftot,
        maktx  TYPE j_1bnflin-maktx,
        pstdat TYPE j_1bnfdoc-pstdat,
        docdat TYPE j_1bnfdoc-docdat,
        nbm    TYPE j_1bnflin-nbm,
        refkey TYPE j_1bnflin-refkey,
      END OF t_j_1bnfdoc.
*----------------------------------------------------------------------*
*-- Variáveis
*----------------------------------------------------------------------*
DATA: v_bukrs_low  LIKE with_item-bukrs,
      v_bukrs_high LIKE with_item-bukrs,
      v_bukrs(250),
      v_budat_low  LIKE bkpf-budat,
      v_budat_high LIKE bkpf-budat,
      v_budat(250).

DATA aux_matnr    TYPE matnr18.

DATA: z_lfbw  TYPE lfbw,
      z_t059z TYPE t059z.

DATA: ls_zmm_controle TYPE zmm_controle.


*----------------------------------------------------------------------*
*-- Estrutura    ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
TYPES: BEGIN OF gy_alv,
         layout     TYPE slis_layout_alv,
         fieldcat_t TYPE slis_t_fieldcat_alv,
         variant    TYPE disvariant,
         events_t   TYPE slis_t_event,
         print      TYPE slis_print_alv,
       END OF gy_alv.
DATA: fieldcat            TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: layout              TYPE slis_layout_alv.
DATA: variant             TYPE disvariant. " 31.07.2024 - ramon - teste para puxar variante no alv ( pode apagar se necessario )
DATA: print               TYPE slis_print_alv.
DATA: it_layout           TYPE lvc_s_layo.
DATA: it_sort TYPE slis_t_sortinfo_alv,
      wa_sort TYPE slis_sortinfo_alv.
DATA: t_header            TYPE slis_t_listheader.
DATA: e_header         TYPE slis_listheader,
      st_grid_settings TYPE lvc_s_glay.

DATA: w_repid             LIKE sy-repid,
      w_titulo(70)        TYPE c,
      w_data(220)         TYPE c,
      w_datafull(220)     TYPE c,
      w_datafull1(220)    TYPE c,
      w_logicsinal(2)     TYPE c,
      w_posicaoinicial(3) TYPE n,
      w_zglt035           TYPE zglt035,  "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
      w_pos               LIKE fieldcat-col_pos,
      w_saida             LIKE t_saida.

DATA: t_saida_aux LIKE TABLE OF t_saida WITH HEADER LINE.
DATA:ls_lote TYPE zmm_controle_lt. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*
RANGES: r_gjahr FOR with_item-gjahr.

DATA : o_alv TYPE REF TO cl_gui_alv_grid.
*RANGES: R_STBLG FOR RBKP-STBLG.
*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  SELECT-OPTIONS s_bukrs FOR with_item-bukrs OBLIGATORY.
  SELECT-OPTIONS s_gsber FOR bsak-gsber.
  SELECT-OPTIONS s_budat FOR bkpf-budat OBLIGATORY.
  SELECT-OPTIONS s_belnr FOR with_item-belnr.
  SELECT-OPTIONS s_witht FOR v_t059z-witht   MATCHCODE OBJECT z_witht_br.
  SELECT-OPTIONS s_augdt FOR bsak-augdt .
  SELECT-OPTIONS s_bldat FOR bkpf-bldat .
  SELECT-OPTIONS s_lifnr FOR bsak-lifnr .
  SELECT-OPTIONS s_usnam FOR bkpf-usnam .
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  SELECT-OPTIONS s_hkont FOR bsak-hkont.
  SELECT-OPTIONS s_xblnr FOR bkpf-xblnr.
  SELECT-OPTIONS s_awkey FOR bkpf-awkey.
  SELECT-OPTIONS s_ebeln FOR rseg-ebeln.
*PARAMETER: sem_imp AS CHECKBOX DEFAULT 'X'.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
    PARAMETERS: s_miroa AS CHECKBOX,
                s_miroe AS CHECKBOX,
                s_mirom AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK b2.

  "PARAMETER: S_ZGL59 AS CHECKBOX.

SELECTION-SCREEN END   OF BLOCK b1.
*----------------------------------------------------------------------*
* Start-OF-Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* #108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*  IF sem_imp IS INITIAL.
  DATA:t_zglt035           TYPE TABLE OF zglt035.
  PERFORM f_seleciona_dados.
*  ELSE.
*    PERFORM f_seleciona_dados_sem.
*    PERFORM f_sel_dados_sem_zgl059.
*  ENDIF.
*#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  IF s_miroa = 'X' AND s_miroe IS INITIAL AND s_mirom IS INITIAL.
    PERFORM f_seleciona_miro_ativa.
  ELSEIF s_miroa IS INITIAL AND s_miroe ='X' AND s_mirom IS INITIAL.
    PERFORM f_seleciona_miro_estornada.
  ELSEIF ( s_miroa IS INITIAL AND s_miroe IS INITIAL AND s_mirom = 'X' ) OR ( s_miroa = 'X' AND s_miroe = 'X' AND s_mirom IS INITIAL ).
    PERFORM f_seleciona_miro_total.
  ELSEIF ( s_miroa = 'X' AND s_miroe IS INITIAL AND s_mirom = 'X' ) OR ( s_miroa IS INITIAL AND s_miroe = 'X' AND s_mirom = 'X').
    MESSAGE i000(z01) WITH 'Seleção de Relório MIRO Inválida.'.
    STOP.
  ENDIF.

  PERFORM f_atrib_dia_vencimento.

* #108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*  IF sem_imp IS INITIAL.


  IF s_ebeln-low <> ' '.
    DELETE t_saida WHERE ebeln NOT IN s_ebeln.
  ENDIF.

  IF s_augdt-low <> ' '.
    DELETE t_saida WHERE augdt NOT IN s_augdt.
  ENDIF.

  IF s_hkont-low <> ' '.
    DELETE t_saida WHERE hkont NOT IN s_hkont.
  ENDIF.

  IF s_awkey-low <> ' '.
    DELETE t_saida WHERE xblnr NOT IN s_awkey.
  ENDIF.
  PERFORM alv_imprime.
*  ELSE.
*    PERFORM alv_imprime_sem.
*  ENDIF.
* #108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

*    IF S_MIROA = 'X'.
*      PERFORM F_SELECIONA_MIRO_ATIVA.
*    ELSEIF S_MIROA = 'X'.
*      PERFORM F_SELECIONA_MIRO_ESTORNADA.
*    ELSE.
*      PERFORM F_SELECIONA_MIRO_TOTAL.
*    ENDIF.
*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .



  DATA: it_saida_rseg     TYPE TABLE OF ty_saida_rseg INITIAL SIZE 0 WITH HEADER LINE,
        wa_saida_rseg     TYPE ty_saida_rseg,
        it_saida_j1bnfdoc TYPE TABLE OF ty_saida_j1bnfdoc WITH HEADER LINE,
        vperimp           TYPE rbkp-rmwwr,
        vtotal_impostos   TYPE ekpo-navnw.

** Cria o range das datas
  CLEAR r_gjahr. REFRESH r_gjahr.
  LOOP AT s_budat.
    MOVE-CORRESPONDING s_budat TO r_gjahr.
    MOVE: s_budat-low(4) TO r_gjahr-low.
    MOVE: s_budat-high(4) TO r_gjahr-high.
    APPEND r_gjahr.
    CLEAR r_gjahr.
  ENDLOOP.
  CLEAR: t_saida[], t_with_item[], t_t059z, t_bkpf, t_rseg, t_zib_chv,  t_zglt081, t_j_1bnfdoc, t_zglt082,
  t_rbkp, t_ekpo, t_esll,  t_esllk, t_t007s, t_bsak2, t_bsak, t_lfa1, t_j_1bbranch, z_lfbw.
** Se o Nro do Documento do Parâmetro não estiver preenchido
  IF s_belnr[] IS INITIAL.
    SELECT with_item~bukrs with_item~belnr with_item~gjahr
           with_item~buzei with_item~witht with_item~wt_acco
           with_item~wt_qsshh with_item~wt_qbshh

      with_item~wt_qbsh2
     with_item~wt_withcd "MODIFICAÇÂO 13/09/2016


           t059u~text40
      FROM with_item INNER JOIN t059u ON
           ( with_item~witht EQ t059u~witht )


            INTO TABLE t_with_item
      WHERE with_item~bukrs   IN s_bukrs AND
            with_item~witht   IN s_witht AND
            with_item~gjahr   IN s_budat AND
            with_item~wt_acco IN s_lifnr AND
            t059u~spras       EQ 'P'    AND
            t059u~land1       EQ 'BR'
        AND EXISTS ( SELECT belnr
                       FROM bkpf
                      WHERE bukrs EQ with_item~bukrs
                        AND belnr EQ with_item~belnr
                        AND gjahr EQ with_item~gjahr
                        AND budat IN s_budat
                        AND xblnr IN s_xblnr ). "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*                        AND awkey IN s_awkey ).  "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  ELSE.
** Se o Nro do Documento do Parâmetros estiver preenchido
    SELECT bukrs belnr gjahr buzei witht wt_acco wt_qsshh wt_qbshh
     wt_withcd "MODIFICAÇÂO 13/09/2016
      FROM with_item INTO CORRESPONDING FIELDS OF TABLE t_with_item
      WHERE bukrs IN s_bukrs AND
            witht IN s_witht AND
            gjahr IN r_gjahr AND
            belnr IN s_belnr.

  ENDIF.

  SORT t_with_item BY bukrs belnr gjahr buzei witht wt_acco wt_qsshh.

** Seleciona os documentos contábies
  IF NOT t_with_item[] IS INITIAL.


*    select docnum nfnum pstdat belnr bukrs gjahr
*      from j_1bnfdoc into table t_doc
*      for all entries in t_with_item
*      where bukrs = t_with_item-bukrs and
*            belnr = t_with_item-belnr and
*            gjahr = t_with_item-gjahr.

    SORT t_doc BY belnr bukrs gjahr.

* V_T059Z
    SELECT witht qscod
      FROM t059z INTO TABLE t_t059z
      FOR ALL ENTRIES IN t_with_item
      WHERE witht EQ t_with_item-witht
      AND wt_withcd EQ t_with_item-wt_withcd "MODIFICAÇÂO 13/09/2016
      AND land1 = 'BR'.

* BKPF
    SELECT bukrs belnr gjahr budat bldat brnch xblnr awkey usnam
      FROM bkpf INTO TABLE t_bkpf
      FOR ALL ENTRIES IN t_with_item
      WHERE bukrs EQ t_with_item-bukrs AND
            belnr EQ t_with_item-belnr AND
            gjahr EQ t_with_item-gjahr AND
            budat IN s_budat
           AND xblnr IN s_xblnr."#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*          AND awkey IN s_awkey. "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

    SORT t_bkpf BY bukrs belnr gjahr budat bldat brnch.

    LOOP AT t_bkpf.
      t_bkpf-awk10 = t_bkpf-awkey(10).
      MODIFY t_bkpf FROM t_bkpf INDEX sy-tabix TRANSPORTING awk10.
    ENDLOOP.

** RSEG
    IF t_bkpf[] IS NOT INITIAL.
      SELECT bukrs belnr gjahr ebeln ebelp mwskz
        INTO CORRESPONDING FIELDS OF TABLE t_rseg
        FROM rseg
       FOR ALL ENTRIES IN t_bkpf
       WHERE bukrs EQ t_bkpf-bukrs
         AND belnr EQ t_bkpf-awk10
         AND gjahr EQ t_bkpf-gjahr
         AND ebeln IN s_ebeln. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
    ENDIF.

    SORT t_rseg BY bukrs belnr gjahr.

** ZIB_CONTABIL_CHV
    IF t_bkpf[] IS NOT INITIAL.
      SELECT *
        FROM zib_contabil_chv INTO CORRESPONDING FIELDS OF TABLE t_zib_chv
        FOR ALL ENTRIES IN t_bkpf
      WHERE obj_key EQ t_bkpf-awkey
        AND belnr   EQ t_bkpf-belnr
        AND gjahr   EQ t_bkpf-gjahr.
    ENDIF.

    LOOP AT t_zib_chv.
      IF ( strlen( t_zib_chv-obj_key ) = 19      ) AND
         ( t_zib_chv-obj_key(5)        = 'ZGL17' ).
        t_zib_chv-doc_lcto = t_zib_chv-obj_key+5(10).
        MODIFY t_zib_chv.
      ENDIF.
    ENDLOOP.

    DELETE t_zib_chv WHERE doc_lcto IS INITIAL.

    IF t_zib_chv[] IS NOT INITIAL.

      SORT t_zib_chv BY obj_key belnr gjahr.

      "Lançamentos Pagamentos Contratos Corporativos(ZGL059)
      SELECT  *
        FROM zglt081 INTO CORRESPONDING FIELDS OF TABLE t_zglt081
         FOR ALL ENTRIES IN t_zib_chv
       WHERE bukrs    = t_zib_chv-bukrs
         AND doc_lcto = t_zib_chv-doc_lcto.

      "DELETE T_ZGLT081 WHERE ( DOCNUM IS INITIAL ) OR ( NFENUM IS INITIAL ).
      DELETE t_zglt081 WHERE ( nfenum IS INITIAL ).

      IF t_zglt081[] IS NOT INITIAL.

        SORT t_zglt081 BY bukrs doc_lcto.

        SELECT a~docnum a~nftype a~nftot b~maktx a~pstdat a~docdat b~nbm b~refkey
          INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
          FROM j_1bnfdoc AS a INNER JOIN j_1bnflin AS b ON a~docnum = b~docnum
           FOR ALL ENTRIES IN t_zglt081
         WHERE a~docnum EQ t_zglt081-docnum.

        SORT t_j_1bnfdoc BY docnum.
        DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc COMPARING docnum.

        SELECT  *
          FROM zglt082 INTO CORRESPONDING FIELDS OF TABLE t_zglt082
           FOR ALL ENTRIES IN t_zglt081
         WHERE operacao = t_zglt081-operacao.

        SORT t_zglt082 BY operacao.
      ENDIF.

    ENDIF.

    IF t_rseg[] IS NOT INITIAL.

** RBKP

      SELECT belnr gjahr rmwwr budat bldat
        j_1bnftype stblg "MODIFICAÇÂO 13/09/2016
        INTO CORRESPONDING FIELDS OF TABLE t_rbkp
        FROM rbkp
        FOR ALL ENTRIES IN t_rseg
       WHERE belnr EQ t_rseg-belnr
         AND gjahr EQ t_rseg-gjahr.

      SORT t_rbkp BY belnr gjahr.

*  * EKPO
      SELECT ebeln ebelp navnw packno
        INTO CORRESPONDING FIELDS OF TABLE t_ekpo
        FROM ekpo
       FOR ALL ENTRIES IN t_rseg
      WHERE ebeln EQ t_rseg-ebeln
        AND ebelp EQ t_rseg-ebelp.

      SORT t_ekpo BY ebeln ebelp.

*  * ESLL
      SELECT packno sub_packno
        INTO CORRESPONDING FIELDS OF TABLE t_esll
        FROM esll
        FOR ALL ENTRIES IN t_ekpo
      WHERE packno EQ t_ekpo-packno.

      SORT t_esll BY packno.

      LOOP AT t_rseg.
        t_rseg-refkey = t_rseg-belnr && t_rseg-gjahr.
        MODIFY t_rseg.
      ENDLOOP.

      SELECT a~docnum a~nftype a~nftot b~maktx a~pstdat a~docdat b~nbm b~refkey
        APPENDING CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
        FROM j_1bnfdoc AS a INNER JOIN j_1bnflin AS b ON a~docnum = b~docnum
         FOR ALL ENTRIES IN t_rseg
       WHERE b~refkey EQ t_rseg-refkey.

    ENDIF.

** ESLL

    IF NOT ( t_esll[] IS INITIAL ).

      SELECT packno ktext1
        INTO CORRESPONDING FIELDS OF TABLE t_esllk
        FROM esll
        FOR ALL ENTRIES IN t_esll
      WHERE packno EQ t_esll-sub_packno.

      SORT t_esllk BY packno.
    ENDIF.

** T007A
    IF t_rseg[] IS NOT INITIAL.
      SELECT kalsm mwskz text1
        INTO CORRESPONDING FIELDS OF TABLE t_t007s
        FROM t007s
         FOR ALL ENTRIES IN t_rseg
       WHERE kalsm EQ	'TAXBRA'
         AND mwskz EQ t_rseg-mwskz
         AND spras EQ sy-langu.
    ENDIF.

    IF t_zglt082[] IS NOT INITIAL.
      SELECT kalsm mwskz text1
        FROM t007s APPENDING CORRESPONDING FIELDS OF TABLE t_t007s
         FOR ALL ENTRIES IN t_zglt082
       WHERE kalsm EQ	'TAXBRA'
         AND mwskz EQ t_zglt082-taxcode
         AND spras EQ sy-langu.
    ENDIF.

    SORT t_t007s BY kalsm mwskz.

** BSIK
    SELECT bukrs lifnr belnr gsber augdt
      FROM bsik INTO TABLE t_bsak2
      FOR ALL ENTRIES IN t_with_item
      WHERE bukrs IN s_bukrs AND
            gsber IN s_gsber AND
            lifnr EQ t_with_item-wt_acco AND
            belnr EQ t_with_item-belnr AND
            shkzg EQ 'H'." AND
*            hkont IN s_hkont."#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

** BSAK
    SELECT bukrs lifnr belnr gsber augdt hkont "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
      FROM bsak INTO TABLE t_bsak
      FOR ALL ENTRIES IN t_with_item
      WHERE bukrs IN s_bukrs AND
            gsber IN s_gsber AND
            lifnr EQ t_with_item-wt_acco AND
            belnr EQ t_with_item-belnr AND
            shkzg EQ 'H'." AND
*            hkont IN s_hkont. "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

    LOOP AT t_bsak2.
      APPEND t_bsak2 TO t_bsak.
    ENDLOOP.

    SORT t_bsak BY bukrs lifnr belnr gsber augdt.

** LFA1
    SELECT lifnr stcd2 stcd1 name1
           stenr stkzn ort01 regio land1
      FROM lfa1 INTO TABLE t_lfa1
      FOR ALL ENTRIES IN t_with_item
      WHERE lifnr EQ t_with_item-wt_acco.
    SORT t_lfa1 BY lifnr stcd2 stcd1 name1 stenr stkzn.

** J_1BBRANCH
    IF t_bsak[] IS NOT INITIAL.
      SELECT bukrs branch name
        FROM j_1bbranch INTO TABLE t_j_1bbranch
        FOR ALL ENTRIES IN t_bsak
        WHERE bukrs EQ t_bsak-bukrs AND
              branch EQ t_bsak-gsber.
    ENDIF.
    SORT t_j_1bbranch BY bukrs branch name.

  ENDIF.

  SORT t_j_1bnfdoc BY docnum.
  DELETE ADJACENT DUPLICATES FROM t_j_1bnfdoc COMPARING docnum.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  SELECT * FROM t059u
    INTO TABLE @DATA(lt_t059u)
    FOR ALL ENTRIES IN @t_with_item
     WHERE witht = @t_with_item-witht
      AND  text40 <> ' '.

  DATA:v_awkey TYPE zib_contabil_chv-obj_key.
  DATA:wa_zib_contabil_chv TYPE zib_contabil_chv,
       lva_stblg           TYPE bkpf-stblg.

  SELECT * FROM zmm_controle
    INTO TABLE @DATA(lt_zmm_controle).
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

** Monta as informações na tabela que será exibida no alv
  LOOP AT t_with_item.

    CLEAR t_saida. " ramon teste

    MOVE: t_with_item-belnr    TO t_saida-belnr,
          t_with_item-wt_qsshh TO t_saida-wt_qsshh,
          t_with_item-bukrs    TO t_saida-bukrs,
          t_with_item-wt_qbshh TO t_saida-wt_qbshh,
          t_with_item-wt_acco  TO t_saida-wt_acco,
          t_with_item-text40   TO t_saida-text40,
          t_with_item-witht    TO t_saida-witht.


*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> INI
    MOVE t_with_item-wt_qbsh2 TO t_saida-wt_qbsh2.

* multiplica valores por -1 para vim positivo
    IF t_saida-wt_qbsh2 < 0.
      t_saida-wt_qbsh2 = t_saida-wt_qbsh2 * ( - 1 ).
    ENDIF.
*** Modificação - Eduardo Ruttkowski Tavare - 19.08.2013 >>> FIM

* multiplica valores por -1 para vim positivo
    IF t_saida-wt_qsshh < 0.
      t_saida-wt_qsshh = t_saida-wt_qsshh * ( - 1 ).
    ENDIF.

*    read table t_doc with key belnr = t_with_item-belnr
*                              bukrs = t_with_item-bukrs
*                              gjahr = t_with_item-gjahr
*                              binary search.
*    if sy-subrc = 0.
*      t_saida-nfnum = t_doc-nfnum.
*    endif.

* multiplica valores por -1 para vim positivo
    IF t_saida-wt_qbshh < 0.
      t_saida-wt_qbshh = t_saida-wt_qbshh * ( - 1 ).
    ENDIF.

    READ TABLE t_t059z WITH KEY witht = t_with_item-witht.
    IF sy-subrc EQ 0.
      MOVE: t_t059z-qscod TO t_saida-qscod.
    ENDIF.

    READ TABLE t_lfa1 WITH KEY lifnr = t_with_item-wt_acco
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: t_lfa1-name1 TO t_saida-name1,
            t_lfa1-stenr TO t_saida-stenr,
            t_lfa1-ort01 TO t_saida-ort01,
            t_lfa1-regio TO t_saida-regio.
      IF t_lfa1-stkzn EQ 'X'.
        MOVE: t_lfa1-stcd2 TO t_saida-stcd2.
      ELSE.
        MOVE: t_lfa1-stcd1 TO t_saida-stcd2.
      ENDIF.

*---> CS1063987 / IR126317 - Considera BUKRS somente se o cliente existe
      SELECT SINGLE *
         FROM lfbw INTO z_lfbw WHERE lifnr EQ t_with_item-wt_acco
         AND bukrs EQ s_bukrs-low
         AND witht EQ t_with_item-witht.

      SELECT SINGLE qscod INTO t_saida-qscod FROM t059z
        WHERE witht EQ z_lfbw-witht
          AND wt_withcd EQ z_lfbw-wt_withcd
          AND land1 EQ t_lfa1-land1.
*<--- CS1063987 / IR126317

    ENDIF.

    READ TABLE t_bkpf WITH KEY belnr = t_with_item-belnr
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF ( s_usnam IS NOT INITIAL ) AND ( t_bkpf-usnam NOT IN s_usnam ).
        CONTINUE.
      ENDIF.
      CLEAR: t_saida-ktext1.

      MOVE: t_bkpf-budat TO t_saida-budat,
            t_bkpf-bldat TO t_saida-bldat,
            t_bkpf-xblnr TO t_saida-nfnum,
            t_bkpf-usnam TO t_saida-usnam,
            t_bkpf-awkey TO t_saida-awkey."#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

      READ TABLE t_bsak WITH KEY bukrs = t_bkpf-bukrs
                                 lifnr = t_with_item-wt_acco
                                 belnr = t_with_item-belnr
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE: t_bsak-gsber TO t_saida-gsber,
              t_bsak-augdt TO t_saida-augdt.

        "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
        SELECT hkont FROM with_item
          INTO t_saida-hkont
          WHERE belnr = t_with_item-belnr
            AND witht = t_with_item-witht .
        ENDSELECT.
*              t_bsak-hkont TO t_saida-hkont."conta razao
        "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

        READ TABLE t_j_1bbranch WITH KEY bukrs  = t_bsak-bukrs
                                         branch = t_bsak-gsber
                                         BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE: t_j_1bbranch-name TO t_saida-name.
        ENDIF.

        t_rseg-ebeln = ' '. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

        READ TABLE t_rseg WITH KEY bukrs = t_bkpf-bukrs
                                   belnr = t_bkpf-awk10
                                   gjahr = t_bkpf-gjahr
                                   BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ).
          READ TABLE t_rbkp WITH KEY belnr = t_rseg-belnr
                                     gjahr = t_rseg-gjahr
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: t_rbkp-rmwwr TO t_saida-rmwwr,
                  t_rbkp-j_1bnftype TO t_saida-j_1bnftype,"MODIFICAÇÃO 13/09/2016
                  'X' TO t_saida-in_rbkp,"MODIFICAÇÃO 13/09/2016
                  t_rbkp-stblg TO t_saida-stblg. "MODIFICAÇÃO 13/09/2016.
          ENDIF.

          IF NOT t_rseg-ebeln IS INITIAL.
            READ TABLE t_ekpo WITH KEY ebeln = t_rseg-ebeln
                                       ebelp = t_rseg-ebelp
                                       BINARY SEARCH.
            IF ( sy-subrc IS INITIAL ) AND ( NOT t_ekpo-packno IS INITIAL ).
              READ TABLE t_esll WITH KEY packno = t_ekpo-packno BINARY SEARCH.
              IF ( sy-subrc IS INITIAL ) AND ( NOT t_esll-sub_packno IS INITIAL ).
                READ TABLE t_esllk WITH KEY packno = t_esll-sub_packno BINARY SEARCH.
                IF ( sy-subrc IS INITIAL ).
                  "Descrição Serviço
                  MOVE t_esllk-ktext1 TO t_saida-ktext1.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

          READ TABLE t_j_1bnfdoc WITH KEY refkey = t_rseg-refkey.
          IF ( sy-subrc EQ 0 ) AND ( t_rseg-refkey IS NOT INITIAL ).
            t_saida-nbm = t_j_1bnfdoc-nbm.
          ENDIF.

        ELSE.  "Buscar Dados ZGL059

          READ TABLE t_zib_chv WITH KEY obj_key = t_bkpf-awkey
                                        belnr   = t_bkpf-belnr
                                        gjahr   = t_bkpf-gjahr
                                        BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE t_zglt081 WITH KEY bukrs    = t_zib_chv-bukrs
                                          doc_lcto = t_zib_chv-doc_lcto
                                          BINARY SEARCH.
            IF sy-subrc = 0.
              CLEAR: t_j_1bnfdoc.
              READ TABLE t_j_1bnfdoc WITH KEY docnum = t_zglt081-docnum BINARY SEARCH.

              READ TABLE t_zglt082 WITH KEY operacao = t_zglt081-operacao BINARY SEARCH.
              IF ( sy-subrc = 0 ) AND ( t_j_1bnfdoc IS NOT INITIAL ).
                MOVE: t_j_1bnfdoc-nftot  TO t_saida-rmwwr,
                      t_j_1bnfdoc-nftype TO t_saida-j_1bnftype,
                      t_j_1bnfdoc-nbm    TO t_saida-nbm,
                      t_j_1bnfdoc-maktx  TO t_saida-ktext1,
                      'X'                TO t_saida-in_rbkp.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = t_zglt081-nfenum
                  IMPORTING
                    output = t_zglt081-nfenum.

                CONCATENATE t_zglt081-nfenum '-' t_zglt081-series INTO t_saida-nfnum.
                "'X' TO T_SAIDA-IN_RBKP,
                "T_RBKP-STBLG TO T_SAIDA-STBLG.

              ELSEIF t_zglt081-sem_nf IS NOT INITIAL.
                MOVE: t_zglt081-netwr    TO t_saida-rmwwr,
                      ''                 TO t_saida-j_1bnftype,
                      'X'                TO t_saida-in_rbkp.


**********************************************************************
* PSA CONVERT MATNR 18

*** Formata o código do material
                CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                  EXPORTING
                    input  = t_zglt081-matnr "campo de 400char
                  IMPORTING
                    output = aux_matnr.

                CLEAR: t_zglt081-matnr.

                t_zglt081-matnr = aux_matnr.

                CLEAR: aux_matnr.

* END CONVERT
**********************************************************************


                IF t_zglt081-matnr IS NOT INITIAL.
                  SELECT SINGLE maktx
                    FROM makt INTO t_saida-ktext1
                   WHERE matnr = t_zglt081-matnr
                     AND spras = sy-langu.
                ELSEIF t_zglt081-asnum IS NOT INITIAL.
                  SELECT SINGLE asktx
                    FROM asmdt INTO t_saida-ktext1
                   WHERE asnum = t_zglt081-asnum
                     AND spras = sy-langu.
                ENDIF.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = t_zglt081-nfenum
                  IMPORTING
                    output = t_zglt081-nfenum.

                CONCATENATE t_zglt081-nfenum '-' t_zglt081-series INTO t_saida-nfnum.
              ENDIF.

            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.

        ENDIF.


        IF t_saida-wt_qsshh NE 0.
          vperimp = ( t_saida-wt_qbshh / t_saida-wt_qsshh ) * 100 .
        ELSE.
          vperimp = 0.
        ENDIF.
        MOVE vperimp TO t_saida-perimp.

        "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*            anexo
        PERFORM f_verifiva_icone_arquivo.
*obs
        t_saida-ebeln = t_rseg-ebeln.

        PERFORM f_pega_obs.

        IF t_saida-text40 = ' '.
          READ TABLE lt_t059u    INTO DATA(ls_t059u) WITH KEY witht  = t_saida-witht.
          t_saida-text40 = ls_t059u-text40.
        ENDIF.

        READ TABLE lt_zmm_controle INTO ls_zmm_controle WITH KEY bukrs  = t_saida-bukrs
                                                                 belnr  = t_saida-belnr
                                                                 gjahr  = t_saida-gjahr
                                                                 ebeln  = t_saida-ebeln
                                                                 hkont  = t_saida-hkont
                                                                 witht  = t_saida-witht.

        IF sy-subrc = 0. "AND  t_saida-perimp <> '0.00'. "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
          t_saida-imp_correto   = ls_zmm_controle-imp_correto.
          t_saida-vl_correto    = ls_zmm_controle-vl_correto .
          t_saida-deb_cre       = ls_zmm_controle-deb_cre .
          t_saida-base_imposto  = ls_zmm_controle-base_imposto.
        ELSE.
          t_saida-imp_correto   = ' '.
          t_saida-vl_correto    = ' '.
          t_saida-deb_cre       = ' '.
          t_saida-base_imposto  = ' '.
        ENDIF.

        SELECT docnum FROM j_1bnfdoc
          INTO t_saida-xblnr
          WHERE belnr = t_saida-awkey(10)
            AND gjahr = t_saida-awkey+10(4).
        ENDSELECT.

        SELECT ebeln FROM rseg
        INTO t_saida-ebeln
        WHERE belnr = t_saida-awkey(10)
         AND gjahr = t_saida-awkey+10(4).
        ENDSELECT.

        t_saida-imp_gerado = ' '.

        SELECT * FROM zmm_controle_lt
          INTO ls_lote
          WHERE witht = t_saida-witht
            AND xblnr = t_saida-xblnr.
        ENDSELECT.

        IF sy-subrc = 0.

          " 30.07.2024 - RAMON - FIS-#108338 -->
          " foi feito pq se nao tiver a NF, então pega pela chave awkey
          IF t_saida-xblnr IS INITIAL.

            SELECT * FROM zglt035
              INTO TABLE t_zglt035
              WHERE xblnr = t_saida-awkey.

            READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY xblnr = t_saida-awkey.

          ELSE.
            " 30.07.2024 - RAMON - FIS-#108338 --<
            SELECT * FROM zglt035
                INTO TABLE t_zglt035
              WHERE lote = ls_lote-lote.

            READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY lote = ls_lote-lote.

          ENDIF. " 30.07.2024 - RAMON - FIS-#108338

          IF sy-subrc = 0.

            CONCATENATE 'ZGL17' w_zglt035-doc_lcto w_zglt035-budat+0(4) INTO v_awkey.

            SELECT SINGLE mandt obj_key belnr bukrs gjahr
            FROM zib_contabil_chv
            INTO wa_zib_contabil_chv
            WHERE obj_key EQ v_awkey.

            IF sy-subrc = 0.

              CLEAR:lva_stblg.
              SELECT SINGLE stblg
              FROM bkpf
              INTO lva_stblg
              WHERE bukrs = wa_zib_contabil_chv-bukrs AND
                  belnr = wa_zib_contabil_chv-belnr AND
                  gjahr = wa_zib_contabil_chv-gjahr.

              IF lva_stblg IS INITIAL. "Não estornou
                t_saida-imp_gerado = wa_zib_contabil_chv-belnr.
                t_saida-doc_lanc = w_zglt035-doc_lcto.
              ELSE.
                t_saida-imp_gerado  = ' '.
                t_saida-doc_lanc = ' '.
              ENDIF.
            ELSE.
              t_saida-imp_gerado  = ' '.
              t_saida-doc_lanc = w_zglt035-doc_lcto.
            ENDIF.
          ELSE.
            t_saida-imp_gerado  = ' '.
            t_saida-doc_lanc =  ' '.
          ENDIF.
*        ENDIF.
        ENDIF.

        IF t_saida-imp_gerado = ' '.
          t_saida-imp_gerado  = icon_execute_object.
        ENDIF.
        "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

        APPEND t_saida.

        "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
        t_saida-imp_gerado =  ' '.
        t_saida-doc_lanc = ' '.
        t_rseg-ebeln = ' '.
        "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

        READ TABLE t_rseg WITH KEY bukrs = t_bkpf-bukrs
                                   belnr = t_bkpf-awk10
                                   gjahr = t_bkpf-gjahr
                                   BINARY SEARCH.

        IF ( sy-subrc IS INITIAL ).

          "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*          MOVE: t_bkpf-awk10 TO t_saida-belnr.

          t_saida-ebeln_p  = t_rseg-ebeln."pedido
          "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

          READ TABLE it_saida_rseg INTO wa_saida_rseg WITH KEY belnr = t_rseg-belnr
                                                               gjahr = t_rseg-gjahr.

          IF NOT sy-subrc IS INITIAL.

            READ TABLE t_rbkp WITH KEY belnr = t_rseg-belnr
                                       gjahr = t_rseg-gjahr
                                       BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE: t_rbkp-rmwwr TO t_saida-rmwwr,
                    t_rbkp-rmwwr TO t_saida-wt_qsshh,
                    t_rbkp-budat TO t_saida-budat,
                    t_rbkp-bldat TO t_saida-bldat,
                    t_rbkp-j_1bnftype TO t_saida-j_1bnftype,"MODIFICAÇÃO 13/09/2016
                    'X' TO t_saida-in_rbkp,"MODIFICAÇÃO 13/09/2016
                    t_rbkp-stblg TO t_saida-stblg. "MODIFICAÇÃO 13/09/2016


            ELSE.
              CLEAR: t_saida-rmwwr, t_saida-wt_qsshh, t_saida-budat, t_saida-bldat.
            ENDIF.

            IF NOT t_rseg-ebeln IS INITIAL.
              vtotal_impostos = 0.
              LOOP AT t_rseg WHERE bukrs = t_bkpf-bukrs
                             AND   belnr = t_bkpf-awk10
                             AND   gjahr = t_bkpf-gjahr.
                READ TABLE t_ekpo WITH KEY ebeln = t_rseg-ebeln
                                      ebelp = t_rseg-ebelp
                                      BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  ADD t_ekpo-navnw TO vtotal_impostos.
                ENDIF.
              ENDLOOP.
              MOVE vtotal_impostos TO t_saida-wt_qbshh.
*              READ TABLE t_ekpo WITH KEY ebeln = t_rseg-ebeln
*                                         ebelp = t_rseg-ebelp
*                                         BINARY SEARCH.
*              IF sy-subrc IS INITIAL.
*                MOVE t_ekpo-navnw TO t_saida-wt_qbshh.
*              ELSE.
*                CLEAR: t_saida-wt_qbshh.
*              ENDIF.
            ENDIF.

            MOVE: t_rseg-mwskz TO t_saida-witht.

            READ TABLE t_j_1bnfdoc WITH KEY refkey = t_rseg-refkey.
            IF ( sy-subrc EQ 0 ) AND ( t_rseg-refkey IS NOT INITIAL ).
              t_saida-nbm = t_j_1bnfdoc-nbm.
            ENDIF.

            READ TABLE t_t007s WITH KEY kalsm = 'TAXBRA' mwskz = t_rseg-mwskz BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE t_t007s-text1 TO t_saida-text40.
            ENDIF.
            CLEAR: t_saida-qscod.

            wa_saida_rseg-belnr = t_rseg-belnr.
            wa_saida_rseg-gjahr = t_rseg-gjahr.
            APPEND wa_saida_rseg TO it_saida_rseg.
            t_saida-ebeln = t_rseg-ebeln.
            IF t_saida-wt_qsshh NE 0.
              vperimp = ( t_saida-wt_qbshh / t_saida-wt_qsshh ) * 100 .
            ELSE.
              vperimp = 0.
            ENDIF.
            MOVE vperimp TO t_saida-perimp.

            "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
            IF t_saida-text40 = ' '.
              READ TABLE lt_t059u    INTO DATA(ls_t059u2) WITH KEY witht  = t_saida-witht.
              t_saida-text40 = ls_t059u2-text40.
            ENDIF.
*            anexo
            PERFORM f_verifiva_icone_arquivo.

            SELECT docnum FROM j_1bnfdoc
              INTO t_saida-xblnr
              WHERE belnr = t_saida-awkey(10)
                AND gjahr = t_saida-awkey+10(4).
            ENDSELECT.

            READ TABLE lt_zmm_controle INTO ls_zmm_controle WITH KEY bukrs  = t_saida-bukrs
                                                                     belnr  = t_saida-belnr
                                                                     gjahr  = t_saida-gjahr
                                                                     ebeln  = t_saida-ebeln
                                                                     " 31.07.2024 - RAMON 147436  -->
                                                                     " adicionamos esse codigo, pq a conta não estava vindo preenchida,
                                                                     " porém o filtro anterior já valida se a conta e o documento são interligados.
                                                                     " com isso foi retirado essa validação
                                                                     "hkont  = t_saida-hkont
                                                                     " 31.07.2024 - RAMON 147436 --<
                                                                     witht  = t_saida-witht.

            IF sy-subrc = 0. "AND NOT t_saida-perimp IS INITIAL."DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
              t_saida-imp_correto   = ls_zmm_controle-imp_correto.
              t_saida-vl_correto    = ls_zmm_controle-vl_correto .
              t_saida-deb_cre       = ls_zmm_controle-deb_cre .
              t_saida-base_imposto  = ls_zmm_controle-base_imposto.
            ELSE.
              t_saida-imp_correto   = ' '.
              t_saida-vl_correto    = ' '.
              t_saida-deb_cre       = ' '.
              t_saida-base_imposto  = ' '.
            ENDIF.

            PERFORM f_pega_obs.

            SELECT ebeln FROM rseg
            INTO t_saida-ebeln
            WHERE belnr = t_saida-awkey(10)
             AND gjahr = t_saida-awkey+10(4).
            ENDSELECT.

            t_saida-imp_gerado = ' '.

            SELECT * FROM zmm_controle_lt
               INTO ls_lote
               WHERE witht = t_saida-witht
                 AND xblnr = t_saida-xblnr.
            ENDSELECT.

*            IF sy-subrc = 0.
*              SELECT * FROM zglt035
*                INTO TABLE t_zglt035
*                WHERE  lote = ls_lote-lote.
*
*              READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY lote = ls_lote-lote.
*
*              IF sy-subrc = 0.
*
*                CONCATENATE 'ZGL17' w_zglt035-doc_lcto w_zglt035-budat+0(4) INTO v_awkey.
*
*                SELECT SINGLE obj_key belnr bukrs gjahr
*                FROM zib_contabil_chv
*                INTO wa_zib_contabil_chv
*                WHERE obj_key EQ v_awkey.
*
*                IF sy-subrc = 0.
*                  t_saida-imp_gerado = wa_zib_contabil_chv-obj_key.
*                ENDIF.
*                t_saida-doc_lanc = w_zglt035-doc_lcto.
*              ELSE.
*                t_saida-imp_gerado  = ' '.
*                t_saida-doc_lanc =  ' '.
*              ENDIF.
*            ENDIF.
**            ENDIF.
*
*            IF t_saida-imp_gerado = ' '.
*              t_saida-imp_gerado  = icon_execute_object.
*            ENDIF.

            IF sy-subrc = 0.
              SELECT * FROM zglt035
                INTO TABLE t_zglt035
                WHERE  lote = ls_lote-lote.

              READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY lote = ls_lote-lote.

              IF sy-subrc = 0.

                CONCATENATE 'ZGL17' w_zglt035-doc_lcto w_zglt035-budat+0(4) INTO v_awkey.

                SELECT SINGLE mandt obj_key belnr bukrs gjahr
                FROM zib_contabil_chv
                INTO wa_zib_contabil_chv
                WHERE obj_key EQ v_awkey.

                IF sy-subrc = 0.

                  CLEAR:lva_stblg.
                  SELECT SINGLE stblg
                  FROM bkpf
                  INTO lva_stblg
                  WHERE bukrs = wa_zib_contabil_chv-bukrs AND
                      belnr = wa_zib_contabil_chv-belnr AND
                      gjahr = wa_zib_contabil_chv-gjahr.

                  IF lva_stblg IS INITIAL. "Não estornou
                    t_saida-imp_gerado = wa_zib_contabil_chv-belnr.
                    t_saida-doc_lanc = w_zglt035-doc_lcto.
                  ELSE.
                    t_saida-imp_gerado  = ' '.
                    t_saida-doc_lanc =  ' '.
                  ENDIF.
                ELSE.
                  t_saida-imp_gerado  = ' '.
                  t_saida-doc_lanc =  w_zglt035-doc_lcto.
                ENDIF.
              ELSE.
                t_saida-imp_gerado  = ' '.
                t_saida-doc_lanc =  ' '.
              ENDIF.
*        ENDIF.
            ENDIF.

            IF t_saida-imp_gerado = ' '.
              t_saida-imp_gerado  = icon_execute_object.
            ENDIF.
            "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

            APPEND t_saida.

            t_saida-imp_gerado =  ' '. "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
            t_saida-doc_lanc = ' '. "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
          ENDIF.

          CLEAR t_saida.

        ELSE.  "Buscar Dados ZGL059

          READ TABLE t_zib_chv WITH KEY obj_key = t_bkpf-awkey
                                        belnr   = t_bkpf-belnr
                                        gjahr   = t_bkpf-gjahr
                                        BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE t_zglt081 WITH KEY bukrs    = t_zib_chv-bukrs
                                          doc_lcto = t_zib_chv-doc_lcto
                                          BINARY SEARCH.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.

            CLEAR: t_j_1bnfdoc.
            READ TABLE t_j_1bnfdoc WITH KEY docnum = t_zglt081-docnum BINARY SEARCH.

            READ TABLE t_zglt082 WITH KEY operacao = t_zglt081-operacao BINARY SEARCH.
            IF ( sy-subrc NE 0 ) OR ( t_j_1bnfdoc IS INITIAL ).
              CONTINUE.
            ENDIF.

            READ TABLE it_saida_j1bnfdoc WITH KEY docnum = t_j_1bnfdoc-docnum.
            IF sy-subrc EQ 0.
              CONTINUE.
            ENDIF.

            it_saida_j1bnfdoc-docnum = t_j_1bnfdoc-docnum.
            APPEND it_saida_j1bnfdoc.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = t_zglt081-seq_lcto
              IMPORTING
                output = t_saida-belnr.

            MOVE: t_j_1bnfdoc-nftot  TO t_saida-rmwwr,
                  t_j_1bnfdoc-nftot  TO t_saida-wt_qsshh,
                  t_j_1bnfdoc-pstdat TO t_saida-budat,
                  t_j_1bnfdoc-docdat TO t_saida-bldat,
                  t_j_1bnfdoc-nftype TO t_saida-j_1bnftype,
                  t_j_1bnfdoc-nbm    TO t_saida-nbm,
                  'X'                TO t_saida-in_rbkp.
            "T_RBKP-STBLG TO T_SAIDA-STBLG.

*            VTOTAL_IMPOSTOS = 0.
*            LOOP AT T_RSEG WHERE BUKRS = T_BKPF-BUKRS
*                           AND   BELNR = T_BKPF-AWK10
*                           AND   GJAHR = T_BKPF-GJAHR.
*              READ TABLE T_EKPO WITH KEY EBELN = T_RSEG-EBELN
*                                    EBELP = T_RSEG-EBELP
*                                    BINARY SEARCH.
*              IF SY-SUBRC IS INITIAL.
*                ADD T_EKPO-NAVNW TO VTOTAL_IMPOSTOS.
*              ENDIF.
*            ENDLOOP.
*            MOVE VTOTAL_IMPOSTOS TO T_SAIDA-WT_QBSHH.

            t_saida-wt_qbshh = 0.
            t_saida-wt_qbsh2 = 0.

            MOVE: t_zglt082-taxcode TO t_saida-witht.

            READ TABLE t_t007s WITH KEY kalsm = 'TAXBRA' mwskz = t_zglt082-taxcode BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MOVE t_t007s-text1 TO t_saida-text40.
            ENDIF.
            CLEAR: t_saida-qscod.

            IF t_saida-wt_qsshh NE 0.
              vperimp = ( t_saida-wt_qbshh / t_saida-wt_qsshh ) * 100 .
            ELSE.
              vperimp = 0.
            ENDIF.
            MOVE vperimp TO t_saida-perimp.

            "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*            anexo
            PERFORM f_verifiva_icone_arquivo.

            IF t_saida-text40 = ' '.
              READ TABLE lt_t059u    INTO DATA(ls_t059u3) WITH KEY witht  = t_saida-witht.
              t_saida-text40 = ls_t059u3-text40.
            ENDIF.
            "        MOVE vperimp TO t_saida-perimp.

            READ TABLE lt_zmm_controle INTO ls_zmm_controle WITH KEY bukrs  = t_saida-bukrs
                                                                     belnr  = t_saida-belnr
                                                                     gjahr  = t_saida-gjahr
                                                                     ebeln  = t_saida-ebeln
                                                                     hkont  = t_saida-hkont
                                                                     witht  = t_saida-witht.

            IF sy-subrc = 0. "AND NOT t_saida-perimp IS INITIAL. "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
              t_saida-imp_correto   = ls_zmm_controle-imp_correto.
              t_saida-vl_correto    = ls_zmm_controle-vl_correto .
              t_saida-deb_cre       = ls_zmm_controle-deb_cre .
              t_saida-base_imposto  = ls_zmm_controle-base_imposto.
            ELSE.
              t_saida-imp_correto   = ' '.
              t_saida-vl_correto    = ' '.
              t_saida-deb_cre       = ' '.
              t_saida-base_imposto  = ' '.
            ENDIF.
            PERFORM f_pega_obs.

            SELECT ebeln FROM rseg
            INTO t_saida-ebeln
            WHERE belnr = t_saida-awkey(10)
             AND gjahr = t_saida-awkey+10(4).
            ENDSELECT.

            t_saida-imp_gerado = ' '.
            SELECT * FROM zmm_controle_lt
                 INTO ls_lote
                 WHERE witht = t_saida-witht
                   AND xblnr = t_saida-xblnr.
            ENDSELECT.

*            IF sy-subrc = 0.
*              SELECT * FROM zglt035
*                INTO TABLE t_zglt035
*                WHERE  lote = ls_lote-lote .
*
*              READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY lote = ls_lote-lote.
*
*              IF sy-subrc = 0.
*
*                CONCATENATE 'ZGL17' w_zglt035-doc_lcto w_zglt035-budat+0(4) INTO v_awkey.
*
*                SELECT SINGLE obj_key belnr bukrs gjahr
*                FROM zib_contabil_chv
*                INTO wa_zib_contabil_chv
*                WHERE obj_key EQ v_awkey.
*
*                IF sy-subrc = 0.
*                  t_saida-imp_gerado = wa_zib_contabil_chv-obj_key.
*                ENDIF.
*
*                t_saida-doc_lanc = w_zglt035-doc_lcto.
*              ELSE.
*
*                t_saida-imp_gerado  = ' '.
*                t_saida-doc_lanc =  ' '.
*              ENDIF.
**        ENDIF.
*            ENDIF.
**            ENDIF.
*            IF t_saida-imp_gerado = ' '.
*              t_saida-imp_gerado  = icon_execute_object.
*            ENDIF.

            IF sy-subrc = 0.
              SELECT * FROM zglt035
                INTO TABLE t_zglt035
                WHERE  lote = ls_lote-lote.

              READ TABLE t_zglt035 INTO w_zglt035 WITH  KEY lote = ls_lote-lote.

              IF sy-subrc = 0.

                CONCATENATE 'ZGL17' w_zglt035-doc_lcto w_zglt035-budat+0(4) INTO v_awkey.

                SELECT SINGLE mandt obj_key belnr bukrs gjahr
                FROM zib_contabil_chv
                INTO wa_zib_contabil_chv
                WHERE obj_key EQ v_awkey.

                IF sy-subrc = 0.

                  CLEAR:lva_stblg.
                  SELECT SINGLE stblg
                  FROM bkpf
                  INTO lva_stblg
                  WHERE bukrs = wa_zib_contabil_chv-bukrs AND
                      belnr = wa_zib_contabil_chv-belnr AND
                      gjahr = wa_zib_contabil_chv-gjahr.

                  IF lva_stblg IS INITIAL. "Não estornou
                    t_saida-imp_gerado = wa_zib_contabil_chv-belnr.
                    t_saida-doc_lanc = w_zglt035-doc_lcto.
                  ELSE.
                    t_saida-imp_gerado  = ' '.
                    t_saida-doc_lanc =  ' '.
                  ENDIF.
                ELSE.
                  t_saida-imp_gerado  = ' '.
                  t_saida-doc_lanc = w_zglt035-doc_lcto.
                ENDIF.
              ELSE.
                t_saida-imp_gerado  = ' '.
                t_saida-doc_lanc = ' '.
              ENDIF.
*        ENDIF.
            ENDIF.

            IF t_saida-imp_gerado = ' '.
              t_saida-imp_gerado  = icon_execute_object.
            ENDIF.
            "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

            APPEND t_saida.
            t_saida-imp_gerado =  ' '. "#108338 - Alteração Guilherme Rabelo  >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
            t_saida-doc_lanc = ' '.  "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.
    CLEAR t_saida.
  ENDLOOP.
  DELETE t_saida WHERE augdt NOT IN s_augdt.
  DELETE t_saida WHERE bldat NOT IN s_bldat.
  IF t_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não há registros para seleção'.
    STOP.
  ENDIF.

ENDFORM.                   " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  alv_imprime
*&---------------------------------------------------------------------*
FORM alv_imprime .

  PERFORM alv_catalog.
  PERFORM alv_layout.
  PERFORM alv_sort.
  PERFORM alv_output.

ENDFORM.                    " ALV_IMPRIME

*&---------------------------------------------------------------------*
*&      Form  alv_catalog
*&---------------------------------------------------------------------*
FORM alv_catalog .
  DATA tamanho TYPE i.
  REFRESH fieldcat.
  w_pos = 0.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BUKRS'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'BUKRS'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Empresa'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'GSBER'.
  fieldcat-ref_tabname   = 'BSAK'.
  fieldcat-ref_fieldname = 'GSBER'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Filial'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'DIA_VENCIMENTO'.
  fieldcat-ref_tabname   = 'ZFIT0140'.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*  fieldcat-ref_fieldname = 'DIA_VENCIMENTO'.
*  fieldcat-just          = 'C'.
*  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Dt.Vcto'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  ADD 1 TO w_pos.
*  fieldcat-col_pos       = w_pos.
*  fieldcat-tabname       = 'T_SAIDA'.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  fieldcat-fieldname     = 'NAME'.
  fieldcat-ref_tabname   = 'J_1BBRANCH'.
  fieldcat-ref_fieldname = 'NAME'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Nome da filial'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'WITHT'.
  fieldcat-ref_tabname   = 'T059U'.
  fieldcat-ref_fieldname = 'WITHT'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Tipo Imp.'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'TEXT40'.
  fieldcat-ref_tabname   = 'T059U'.
  fieldcat-ref_fieldname = 'TEXT40'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Desc Tp Imposto'."#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  fieldcat-fieldname     = 'HKONT'.
  fieldcat-ref_tabname   = 'BSAK'.
  fieldcat-ref_fieldname = 'HKONT'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Conta Razão'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BUDAT'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'BUDAT'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Data Lançto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BLDAT'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'BLDAT'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Data Doc Contábil'. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'AUGDT'.
  fieldcat-ref_tabname   = 'BSAK'.
  fieldcat-ref_fieldname = 'AUGDT'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic ='Data Pgto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  fieldcat-fieldname     = 'ANEXO'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
  fieldcat-hotspot       = 'X'.
  fieldcat-key           = 'X'.
  fieldcat-just       = 'C'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Anexos'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BELNR'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'BELNR'.
  fieldcat-hotspot       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'N° Doc Contábil'. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  APPEND fieldcat.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'AWKEY'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'AWKEY'.
  fieldcat-hotspot       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'N° Miro'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'XBLNR'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'XBLNR'.
  fieldcat-hotspot       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'N° Doc Fiscal'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'EBELN'.
  fieldcat-ref_tabname   = 'RSEG'.
  fieldcat-ref_fieldname = 'EBELN'.
  fieldcat-hotspot       = 'X'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'N° Pedido'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'STCD2'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'STCD1'.
  "fieldcat-outputlen = 25.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'CNPJ\CPF'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'WT_ACCO'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'WT_ACCO'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cód.Fornecedor'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NAME1'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'NAME1'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nome do Fornecedor'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  fieldcat-fieldname     = 'KTEXT1'.
  fieldcat-ref_tabname   = 'ESLL'.
  fieldcat-ref_fieldname = 'KTEXT1'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Descrição Serviço'.
  APPEND fieldcat.

*  CLEAR fieldcat.
*  ADD 1 TO w_pos.
*  fieldcat-col_pos       = w_pos.
*  fieldcat-tabname       = 'T_SAIDA'.
*  fieldcat-fieldname     = 'QSCOD'.
*  fieldcat-ref_tabname   = 'V_T059Z'.
*  fieldcat-ref_fieldname = 'QSCOD'.
*  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cód.Imposto'.
*  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

*--- INÍCIO MODIFICAÇÃO 13/09/2016 ---*
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'J_1BNFTYPE'.
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'J_1BNFTYPE'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cat. Nota'.
  APPEND fieldcat.
*--- FIM MODIFICAÇÃO 13/09/2016 ---*
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NFNUM'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'XBLNR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nro. NF'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'RMWWR'.
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'RMWWR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Valor Nota'.
  APPEND fieldcat.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'QSCOD'.
  fieldcat-ref_tabname   = 'V_T059Z'.
  fieldcat-ref_fieldname = 'QSCOD'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cód.Imposto'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'WT_QSSHH'. "'RMWWRB'
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'RMWWR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Base cálculo'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'WT_QBSHH'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'WT_QBSHH'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Vlr. Imposto Retido R$'. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  APPEND fieldcat.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'PERIMP'.
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'RMWWR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Perc. Imposto Retido'.
  APPEND fieldcat.

  "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BASE_IMPOSTO'. "'RMWWRB'
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'RMWWR'.
  fieldcat-edit          = abap_true.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Base imposto'.
  APPEND fieldcat.
  "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'IMP_CORRETO'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
  fieldcat-edit = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Perc. Imp. Correto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'VL_CORRETO'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
*  fieldcat-hotspot       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Vlr Imp. Correto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'DEB_CRE'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'DEB/CRED'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'IMP_GERADO'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
  fieldcat-hotspot       = 'X'.
*  fieldcat-edit       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Contab Imp Gerado'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'DOC_LANC'.
  fieldcat-ref_tabname   = ' '.
  fieldcat-ref_fieldname = ' '.
  fieldcat-hotspot       = 'X'.
*  fieldcat-edit       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Doc Lançamento'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'ORT01'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'ORT01'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Município do fornecedor'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'REGIO'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'REGIO'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'UF'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'USNAM'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'USNAM'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nome do Usuário'.
  APPEND fieldcat.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NBM'.
  fieldcat-ref_tabname   = 'J_1BNFLIN'.
  fieldcat-ref_fieldname = 'NBM'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'NCM'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'OBS'.
  fieldcat-ref_tabname   = 'BSEG'.
  fieldcat-ref_fieldname = 'SGTXT'.
  fieldcat-edit       = 'X'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Observação'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'WT_QBSH2'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'WT_QBSH2'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Vlr.Imposto US'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'STENR'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'STENR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'PIS\NT'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'DIA_VENCIMENTO'.
  fieldcat-ref_tabname   = 'ZFIT0140'.
  fieldcat-ref_fieldname = 'DIA_VENCIMENTO'.
  fieldcat-just          = 'C'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Dt.Vcto'.
  APPEND fieldcat.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

ENDFORM.                    " alv_catalog

*&---------------------------------------------------------------------*
*&      Form  alv_layout
*&---------------------------------------------------------------------*
FORM alv_layout .

  CLEAR: layout, st_grid_settings.

*  IF sem_imp IS INITIAL.
*    w_titulo = text-tit.
*  ELSE.
*    w_titulo = text-tis.
*  ENDIF.

  w_repid                  = sy-repid.

* ... Layout do report
  layout-zebra             = 'X'.
  layout-no_keyfix         = 'X'.
  layout-expand_all        = 'X'.
  layout-cell_merge        = ' '.

* ... Configurações do Grid
  st_grid_settings-no_colwopt        = 'X'.
  st_grid_settings-edt_cll_cb        = 'X'.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  layout-box_fieldname = 'MARK'.
  layout-box_tabname  = 'T_SAIDA'.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  CLEAR it_layout.
  it_layout-no_rowmark     = ' '.
  it_layout-sel_mode       = 'X'.
  it_layout-no_hgridln     = 'X'.

ENDFORM.                    " alv_layout

*&---------------------------------------------------------------------*
*&      Form  ALV_SORT
*&---------------------------------------------------------------------*
FORM alv_sort .
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
** ... Ordenação do report
*  REFRESH it_sort.
*
*  CLEAR wa_sort.
*  wa_sort-spos             = '01'.
*  wa_sort-fieldname        = 'BELNR'.
*  wa_sort-tabname          = 'T_SAIDA'.
*  wa_sort-up               = 'X'.
*  wa_sort-subtot           = 'X'.
*  APPEND wa_sort TO it_sort.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
ENDFORM.                    " ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  alv_output
*&---------------------------------------------------------------------*
FORM alv_output.

  IF sy-uname = 'RBLIMA'.
    variant-report = sy-repid.
    variant-variant = '/RAMON'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = w_repid
      i_background_id          = 'ALV_BACKGROUND'
      i_callback_pf_status_set = 'PF_STATUS_001'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'ALV_HEADER'
      i_grid_settings          = st_grid_settings
      it_fieldcat              = fieldcat[]
      is_layout                = layout
      i_grid_title             = w_titulo
      i_default                = 'X'
      i_save                   = 'A'
      is_print                 = print
      it_sort                  = it_sort
      "is_variant               = variant " 31.07.2024 - ramon
    TABLES
      t_outtab                 = t_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " alv_output

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PF_TAB     text
*----------------------------------------------------------------------*
FORM pf_status_001 USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.                    "z_pf_status_001

*&---------------------------------------------------------------------*
*&      Form  alv_header
*&---------------------------------------------------------------------*
FORM alv_header.

  DATA: v_name1 LIKE t001-butxt.
  DATA: v_name2 LIKE t001-butxt.

  REFRESH t_header.
  CLEAR e_header.

  e_header-typ  = 'H'.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*  IF sem_imp IS INITIAL.
  e_header-info = 'Apuração de Impostos'.
*  ELSE.
*    e_header-info = 'Relatório de Tributos Retidos - Doc. sem impostos'.
*  ENDIF.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  APPEND e_header TO t_header.
  CLEAR  e_header.

** Coloca a empresa no cabeçalho.
  IF s_bukrs-low(4) IS NOT INITIAL AND s_bukrs-high(4) IS NOT INITIAL.
    MOVE: s_bukrs-low(4) TO v_bukrs_low.
    MOVE: s_bukrs-high(4) TO v_bukrs_high.

    SELECT SINGLE butxt INTO v_name1 FROM t001
      WHERE bukrs = v_bukrs_low.

    SELECT SINGLE butxt INTO v_name2 FROM t001
      WHERE bukrs = v_bukrs_high.

    CONCATENATE 'Empresa de '
                v_bukrs_low ' - ' v_name1
                ' até '
                v_bukrs_high ' - ' v_name2
                '.'
                INTO v_bukrs SEPARATED BY space.
    e_header-typ = 'A'.
    e_header-info = v_bukrs.
    APPEND e_header TO t_header.
    CLEAR  e_header.
  ELSE.
    SELECT SINGLE butxt INTO v_name1 FROM t001
  WHERE bukrs = s_bukrs-low.
    CONCATENATE 'Empresa '
                s_bukrs-low(4) ' - ' v_name1
                '.'
                INTO v_bukrs SEPARATED BY space.
    e_header-typ = 'A'.
    e_header-info = v_bukrs.
    APPEND e_header TO t_header.
    CLEAR  e_header.
  ENDIF.

** Coloca o período no cabeçalho.
  IF s_budat-low > 0 AND s_budat-high > 0.
    WRITE: s_budat-low TO v_budat_low.
    WRITE: s_budat-high TO v_budat_high.

    CONCATENATE 'Período de '
                v_budat_low
                ' até '
                v_budat_high
                '.'
                INTO v_budat SEPARATED BY space.
    e_header-typ = 'A'.
    e_header-info = v_budat.
    APPEND e_header TO t_header.
    CLEAR  e_header.
  ELSEIF s_budat-low > 0 AND s_budat-high = 0.
    CONCATENATE 'Período '
                s_budat-low
                '.'
                INTO v_budat SEPARATED BY space.
    e_header-typ = 'A'.
    e_header-info = v_budat.
    APPEND e_header TO t_header.
    CLEAR  e_header.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.

ENDFORM.                    " alv_header
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando botão de imprimir do ALV for clicado
*----------------------------------------------------------------------*
FORM user_command  USING ucomm LIKE sy-ucomm
                  selfield TYPE slis_selfield.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  DATA: v_name_create TYPE thead-tdname.
  DATA:t_line   TYPE  TABLE OF tline,
       ls_tline TYPE tline.

  DATA: vl_docnum TYPE j_1bdocnum.

* UCOMM: é o sy-ucomm (Ok-code)
* SELFIELD: é uma estrutura com dados que nos permite identificar
* o que foi selecionado.
  selfield-row_stable = 'X'.   " manter o relatório na linha selecionada antes do drill down.
  selfield-col_stable = 'X'.   " manter o relatório na coluna selecionada antes do drill down.



  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'   "Atualiza tabela interna.
    IMPORTING
      e_grid = o_alv.
  CALL METHOD o_alv->check_changed_data.

  selfield-refresh = 'X'.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  IF ucomm = 'REFRE'.
    PERFORM f_seleciona_dados.

    IF s_miroa = 'X' AND s_miroe IS INITIAL AND s_mirom IS INITIAL.
      PERFORM f_seleciona_miro_ativa.
    ELSEIF s_miroa IS INITIAL AND s_miroe ='X' AND s_mirom IS INITIAL.
      PERFORM f_seleciona_miro_estornada.
    ELSEIF ( s_miroa IS INITIAL AND s_miroe IS INITIAL AND s_mirom = 'X' ) OR ( s_miroa = 'X' AND s_miroe = 'X' AND s_mirom IS INITIAL ).
      PERFORM f_seleciona_miro_total.
    ELSEIF ( s_miroa = 'X' AND s_miroe IS INITIAL AND s_mirom = 'X' ) OR ( s_miroa IS INITIAL AND s_miroe = 'X' AND s_mirom = 'X').
      MESSAGE i000(z01) WITH 'Seleção de Relório MIRO Inválida.'.
      STOP.
    ENDIF.
    PERFORM f_atrib_dia_vencimento.

* #108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
*  IF sem_imp IS INITIAL.


    IF s_ebeln-low <> ' '.
      DELETE t_saida WHERE ebeln NOT IN s_ebeln.
    ENDIF.

    IF s_augdt-low <> ' '.
      DELETE t_saida WHERE augdt NOT IN s_augdt.
    ENDIF.

    IF s_hkont-low <> ' '.
      DELETE t_saida WHERE hkont NOT IN s_hkont.
    ENDIF.

    IF s_awkey-low <> ' '.
      DELETE t_saida WHERE xblnr NOT IN s_awkey.
    ENDIF.

  ENDIF.

  IF ucomm = '&IC1' AND
  NOT selfield-value IS INITIAL AND selfield-fieldname = 'BELNR'. "#108338 - Alteração Guilherme Rabelo >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

    READ TABLE t_saida INDEX selfield-tabindex.

    SET PARAMETER ID 'BLN' FIELD selfield-value.
    SET PARAMETER ID 'BUK' FIELD t_saida-bukrs.
    SET PARAMETER ID 'GJR' FIELD t_saida-budat(4).

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDIF.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
  IF ucomm = '&IC1' AND
  NOT selfield-value IS INITIAL AND selfield-fieldname = 'DOC_LANC' .

    READ TABLE t_saida INDEX selfield-tabindex.
    SET PARAMETER ID 'BLN' FIELD t_saida-doc_lanc.
*            SET PARAMETER ID 'LOT' FIELD lv_lote. "gw_saida-lote.
    CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.

  ENDIF.

  "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
  IF ucomm = '&IC1' AND NOT selfield-value IS INITIAL AND selfield-fieldname = 'XBLNR' .
    READ TABLE t_saida INDEX selfield-tabindex.
    SET PARAMETER ID 'JEF' FIELD t_saida-xblnr.
    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
  ENDIF.

  IF ucomm = '&IC1' AND NOT selfield-value IS INITIAL AND selfield-fieldname = 'AWKEY' .
    READ TABLE t_saida INDEX selfield-tabindex.
    SET PARAMETER ID 'RBN' FIELD t_saida-awkey.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ENDIF.

  IF ucomm = '&IC1' AND NOT selfield-value IS INITIAL AND selfield-fieldname = 'EBELN' .
    READ TABLE t_saida INDEX selfield-tabindex.
    SET PARAMETER ID 'BES' FIELD t_saida-ebeln.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.
  "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA

  IF ucomm = '&IC1' AND
    NOT selfield-value IS INITIAL AND selfield-fieldname = 'IMP_GERADO' AND selfield-value <> '@15@'.

    DATA:v_ano TYPE gjahr.

    READ TABLE t_saida INDEX selfield-tabindex.

    SELECT gjahr FROM zib_contabil_chv
      INTO v_ano
      WHERE belnr = selfield-value.
    ENDSELECT.

    SET PARAMETER ID 'BLN' FIELD selfield-value.
    SET PARAMETER ID 'BUK' FIELD t_saida-bukrs.
    SET PARAMETER ID 'GJR' FIELD v_ano.

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDIF.

  READ TABLE t_saida INDEX selfield-tabindex.

  IF sy-subrc EQ 0.
    IF selfield-fieldname EQ 'ANEXO'.
      PERFORM f_anexa_visualiza_arquivo.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'   "Atualiza tabela interna.
        IMPORTING
          e_grid = o_alv.
      CALL METHOD o_alv->check_changed_data.
      selfield-refresh = 'X'.
    ENDIF.
  ENDIF.

  IF sy-ucomm = '&DATA_SAVE'.

    LOOP AT t_saida INTO DATA(ls_saida) WHERE mark = 'X'.

      IF NOT t_saida-doc_lanc IS INITIAL.
        MESSAGE s024(sd) WITH 'Já existe Doc. de lançamento gerado!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF ls_saida-perimp >= '0.00'.
        "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 -RSA
        "ls_saida-vl_correto = ls_saida-wt_qsshh / 100 *  - ls_saida-imp_correto.
        ls_saida-vl_correto = ls_saida-base_imposto / 100 *  - ls_saida-imp_correto.
        "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 -RSA
        IF ls_saida-vl_correto < 1.
          ls_saida-vl_correto = ls_saida-vl_correto * -1.
          IF ls_saida-perimp EQ '0.00'.
            ls_saida-deb_cre = ls_saida-wt_qbshh - ls_saida-vl_correto."SMC - 108338
          ENDIF.
        ENDIF.

        IF ls_saida-perimp NE '0.00'. "Ajuste / AOENNING
          ls_saida-deb_cre =  ls_saida-wt_qbshh - ls_saida-vl_correto.
        ENDIF.

        IF ls_saida-base_imposto < 1. "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 -RSA
          CLEAR ls_saida-deb_cre.
        ENDIF.

        MODIFY t_saida FROM ls_saida.

        ls_zmm_controle-bukrs          = ls_saida-bukrs.
        ls_zmm_controle-belnr          = ls_saida-belnr.
        ls_zmm_controle-gjahr          = ls_saida-gjahr.
        ls_zmm_controle-ebeln          = ls_saida-ebeln.
        ls_zmm_controle-hkont          = ls_saida-hkont.
        ls_zmm_controle-witht          = ls_saida-witht.
        ls_zmm_controle-imp_correto    = ls_saida-imp_correto.
        ls_zmm_controle-vl_correto     = ls_saida-vl_correto.
        ls_zmm_controle-deb_cre        = ls_saida-deb_cre.
        ls_zmm_controle-base_imposto   = ls_saida-base_imposto.

        MODIFY zmm_controle FROM ls_zmm_controle.

      ENDIF.
      IF ls_saida-obs <> ' '.
        ls_tline-tdline = ls_saida-obs.
        ls_tline-tdformat = '*'.
        APPEND ls_tline TO t_line.
        CLEAR:ls_tline.

        CONCATENATE 'ZFIS001' t_saida-bukrs t_saida-belnr t_saida-ebeln t_saida-hkont t_saida-witht INTO v_name_create.

        CALL FUNCTION 'CREATE_TEXT'
          EXPORTING
            fid       = 'BEST'
            flanguage = sy-langu
            fname     = v_name_create
            fobject   = 'MATERIAL'
          TABLES
            flines    = t_line.
      ENDIF.
      selfield-refresh = 'X'.
    ENDLOOP.
  ENDIF.

  IF selfield-fieldname = 'IMP_GERADO' AND t_saida-imp_gerado = '@15@' AND t_saida-doc_lanc = ' '.

    DATA: v_tp_lcto TYPE  zglt035-tp_lcto,
          v_xblnr   TYPE zglt035-xblnr.

    DATA: it_zglt036 TYPE TABLE OF zglt036,
          wa_zglt036 TYPE zglt036,
          wa_zglt035 TYPE zglt035,
          lote       TYPE zglt035-lote,
          v_liberado TYPE char01,
          dp_resp    TYPE char2.

    READ TABLE t_saida INDEX selfield-tabindex.

    IF t_saida-base_imposto GT t_saida-rmwwr."DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338
      MESSAGE s024(sd) WITH 'Não permitido Base imposto maior que Valor nota' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT low FROM tvarvc
      INTO dp_resp
      WHERE name = 'ZCONTABILIAZA_RESP'.
    ENDSELECT.

    dp_resp = '84'.

    CALL METHOD zcl_gerar_lote=>create_lote
      EXPORTING
        i_bukrs      = t_saida-bukrs
        i_descr_lote = 'Lancamento Manual'
        i_dep_resp   = dp_resp
        i_user_resp  = sy-uname
      IMPORTING
        e_num_lote   = lote.

    IF NOT t_saida-xblnr IS INITIAL."DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338
      vl_docnum = t_saida-xblnr.
      SELECT SINGLE nfenum
             FROM j_1bnfdoc
             INTO @DATA(vl_nfenum)
             WHERE docnum EQ @vl_docnum.

      CONCATENATE 'NFPS' vl_nfenum INTO wa_zglt035-bktxt SEPARATED BY space.
    ENDIF.

    MOVE: lote                          TO wa_zglt035-lote,
          t_saida-bukrs                 TO wa_zglt035-bukrs,
*          '14'                         TO wa_zglt035-tp_lcto,"pegar com samuel
          dp_resp                       TO wa_zglt035-dpto_resp,
          "'Lancamento Manual'          TO wa_zglt035-bktxt, "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
             'BRL'                      TO wa_zglt035-moeda_doc,"'UFIR'
*              wa_zglt031-st_lc_moeda      TO wa_zglt035-st_lc_moeda,
*              wa_zglt031-moeda_int_hist   TO wa_zglt035-moeda_int_hist,
*              wa_zglt031-moeda_ft_hist    TO wa_zglt035-moeda_ft_hist,
*              wa_zglt031-moeda_gp_hist    TO wa_zglt035-moeda_gp_hist,
          'LM'                          TO wa_zglt035-blart,
          "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
          "t_saida-xblnr                TO wa_zglt035-xblnr,
          t_saida-awkey                 TO wa_zglt035-xblnr,
          "DEVK9A1QWJ - Relatório Impostos Bug FIS-#108338 - RSA
*              wa_zglt031-bktxt            TO wa_zglt035-bktxt,
          sy-datum                      TO wa_zglt035-bldat,
          sy-datum                      TO wa_zglt035-budat,
          sy-datum                      TO wa_zglt035-dt_lcto,
*              wa_zglt031-prov_est         TO wa_zglt035-prov_est,
*              wa_zglt031-st_ap_fiscal     TO wa_zglt035-st_ap_fiscal,
          sy-datum+4(2)                 TO wa_zglt035-monat,
          sy-datum+0(4)                 TO wa_zglt035-gjahr,
          sy-uname                      TO wa_zglt035-usnam,
          sy-datum                      TO wa_zglt035-dt_entrada,
          sy-uzeit                      TO wa_zglt035-hr_entrada.

    FREE it_zglt036.

    DATA: lc_proximo_venciment TYPE sy-datum.

    zcl_miro=>get_proximo_venc_fatura(
      IMPORTING
        e_data_vencimento = lc_proximo_venciment    " Data
      EXCEPTIONS
        erro              = 1
        OTHERS            = 2 ).

    IF t_saida-deb_cre > 0."40 e 31 positivo

      MOVE: sy-tabix                  TO wa_zglt036-seqitem,
*              '14'                      TO wa_zglt036-tp_lcto,
              '40'                       TO wa_zglt036-bschl,"chave D ou C
              t_saida-gsber             TO wa_zglt036-gsber,
              t_saida-hkont             TO wa_zglt036-hkont,
              t_saida-obs             TO wa_zglt036-sgtxt,
*                'A'          TO wa_zglt036-umskz,
              sy-datum                  TO wa_zglt036-dt_vct.

      MOVE: abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_doc,
            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_int.
*            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_forte.

      APPEND wa_zglt036 TO it_zglt036.
*      CLEAR: wa_zglt036.

*      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
*        EXPORTING
*          i_arredonda = abap_true
*        CHANGING
*          i_zglt036   = it_zglt036
*          i_zglt035   = wa_zglt035 ).

*      REFRESH:it_zglt036.

      MOVE: sy-tabix                                TO wa_zglt036-seqitem,
*             '14'                                  TO wa_zglt036-tp_lcto,
              '31'                                  TO wa_zglt036-bschl,"chave D ou C
              t_saida-gsber                         TO wa_zglt036-gsber,
              t_saida-wt_acco                       TO wa_zglt036-hkont,
              t_saida-obs                           TO wa_zglt036-sgtxt,
*             'A'          TO wa_zglt036-umskz,
              lc_proximo_venciment                 TO wa_zglt036-dt_vct.

      MOVE: abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_doc,
            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_int.
*            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_forte.

      APPEND wa_zglt036 TO it_zglt036.
      CLEAR: wa_zglt036.

      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        EXPORTING
          i_arredonda = abap_true
        CHANGING
          i_zglt036   = it_zglt036
          i_zglt035   = wa_zglt035 ).

      REFRESH:it_zglt036.

      MESSAGE s024(sd) WITH 'Solicitação de contabilização gerada com sucesso'.

    ELSEIF t_saida-deb_cre < 0."50 e 29 negativo

      REFRESH:it_zglt036.

      MOVE: sy-tabix                  TO wa_zglt036-seqitem,
*              '14'                      TO wa_zglt036-tp_lcto,
              '50'                       TO wa_zglt036-bschl,"chave D ou C
              t_saida-gsber             TO wa_zglt036-gsber,
              t_saida-hkont             TO wa_zglt036-hkont,
              t_saida-obs             TO wa_zglt036-sgtxt,
*              'A'          TO wa_zglt036-umskz,
              sy-datum                  TO wa_zglt036-dt_vct.

      MOVE: abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_doc,
            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_int.
*            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_forte.

      APPEND wa_zglt036 TO it_zglt036.
      CLEAR: wa_zglt036.

*      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
*        EXPORTING
*          i_arredonda = abap_true
*        CHANGING
*          i_zglt036   = it_zglt036
*          i_zglt035   = wa_zglt035 ).
*
*      REFRESH:it_zglt036.

      MOVE: sy-tabix                  TO wa_zglt036-seqitem,
*              '14'                      TO wa_zglt036-tp_lcto,
              '29'                       TO wa_zglt036-bschl,"chave D ou C
              t_saida-gsber             TO wa_zglt036-gsber,
              t_saida-wt_acco             TO wa_zglt036-hkont,
              t_saida-obs             TO wa_zglt036-sgtxt,
              'A'          TO wa_zglt036-umskz,
              sy-datum                  TO wa_zglt036-dt_vct.

      MOVE: abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_doc,
            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_int.
*            abs( t_saida-deb_cre ) TO wa_zglt036-vlr_moeda_forte.

      APPEND wa_zglt036 TO it_zglt036.
      CLEAR: wa_zglt036.

      TRY.

          CALL METHOD zcl_gerar_lote=>contabilizar_lote(
            EXPORTING
              i_arredonda = abap_true
            CHANGING
              i_zglt036   = it_zglt036
              i_zglt035   = wa_zglt035 ).

          REFRESH:it_zglt036.
          MESSAGE s024(sd) WITH 'Solicitação de contabilização gerada com sucesso'.

        CATCH cx_root.
      ENDTRY.
    ENDIF.

    CLEAR: v_liberado.
    CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
      EXPORTING
        p_num_lote = wa_zglt035-lote
      IMPORTING
        p_liberado = v_liberado.

*atauliza alv
    DATA:wa_zmm_controle_lt TYPE zmm_controle_lt.

    wa_zmm_controle_lt-witht   = t_saida-witht.
    wa_zmm_controle_lt-xblnr   = t_saida-xblnr.
    wa_zmm_controle_lt-lote    = wa_zglt035-lote.

    MODIFY zmm_controle_lt FROM wa_zmm_controle_lt.

*    SELECT doc_lcto FROM zglt035
*      INTO t_saida-imp_gerado
*      WHERE lote  = wa_zglt035-lote.
*    ENDSELECT.
*    SELECT belnr FROM zglt035
*           INTO t_saida-imp_gerado
*           WHERE tp_lcto = t_saida-witht
*             AND xblnr = t_saida-xblnr.
*    ENDSELECT.

    SELECT * FROM zglt035
              INTO  w_zglt035
              WHERE lote = wa_zglt035-lote
                 AND xblnr = t_saida-awkey.
    ENDSELECT.

    IF sy-subrc = 0.
      t_saida-imp_gerado  = w_zglt035-belnr.
      t_saida-doc_lanc = w_zglt035-doc_lcto.

      MODIFY  t_saida INDEX selfield-tabindex.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'   "Atualiza tabela interna.
        IMPORTING
          e_grid = o_alv.
      CALL METHOD o_alv->check_changed_data.

      selfield-refresh = 'X'.

    ENDIF.
  ELSEIF  selfield-fieldname = 'IMP_GERADO' AND t_saida-imp_gerado = '@15@' AND t_saida-doc_lanc <> ' '.
    MESSAGE s024(sd) WITH 'Solicitação de contabilização já solicitada' DISPLAY LIKE 'E'.

  ENDIF.
  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<

  IF sy-ucomm = 'ESTORNAR'.
    PERFORM f_estornar.
  ENDIF.

ENDFORM.                    "USER_COMMAND



*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_SEM
*&---------------------------------------------------------------------*
*       Selecionar dados sem impostos
*----------------------------------------------------------------------*
FORM f_seleciona_dados_sem .

  TYPES: BEGIN OF ty_bkpf.
  TYPES: bukrs      TYPE bukrs,
         werks      TYPE werks_d,
         gjahr      TYPE gjahr,
         budat      TYPE budat,
         awkey      TYPE awkey,
         rmwwr      TYPE rmwwr,
         ebeln      TYPE  ebeln,
         lifnr      TYPE lifnr,
         j_1bnftype TYPE j_1bnftype, "MODIFICAÇÃO 13/09/2016
         stblg      TYPE rbkp-stblg, "MODIFICAÇÃO 13/09/2016
         in_rbkp    TYPE char1, "MODIFICAÇÃO 13/09/2016
         nbm        TYPE j_1bnflin-nbm,
         refkey     TYPE j_1bnflin-refkey.
  TYPES: END OF ty_bkpf.

  DATA: it_rbkp      TYPE TABLE OF rbkp INITIAL SIZE 0 WITH HEADER LINE,
        it_rseg      TYPE TABLE OF rseg INITIAL SIZE 0 WITH HEADER LINE,
        it_rseg2     TYPE TABLE OF rseg INITIAL SIZE 0 WITH HEADER LINE,
        it_ekpo      TYPE TABLE OF ekpo INITIAL SIZE 0 WITH HEADER LINE,
        it_bkpf      TYPE TABLE OF bkpf INITIAL SIZE 0 WITH HEADER LINE,
        it_with_item TYPE TABLE OF with_item INITIAL SIZE 0 WITH HEADER LINE,
        it_bkpfa     TYPE TABLE OF ty_bkpf INITIAL SIZE 0 WITH HEADER LINE,

        wa_bkpf      TYPE bkpf,
        wa_bkpfa     TYPE ty_bkpf,
        wa_rseg      TYPE rseg,
        wa_rbkp      TYPE rbkp,
        wa_ekpo      TYPE ekpo,
        wa_with_item TYPE with_item.


  CLEAR: it_rseg2[], it_bkpfa[].
  CLEAR: t_saida, it_rseg, it_rbkp, it_ekpo, it_bkpfa, it_bkpf, t_with_item, t_j_1bbranch, t_lfa1.
  SELECT * INTO TABLE it_rbkp
    FROM rbkp
   WHERE bukrs   IN s_bukrs
     AND budat   IN s_budat
     AND lifnr   IN s_lifnr
     AND blart EQ 'RE'.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_rseg
    FROM rseg
     FOR ALL ENTRIES IN it_rbkp
   WHERE belnr EQ it_rbkp-belnr
    AND werks IN s_gsber. "MODIFICAÇÃO 15/09/2016

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_ekpo
    FROM ekpo
    FOR ALL ENTRIES IN it_rseg
   WHERE ebeln EQ it_rseg-ebeln
     AND ( pstyp EQ '9' OR mtart = 'ZDIE' ).

  CHECK sy-subrc IS INITIAL.

  SORT it_ekpo BY ebeln.

  LOOP AT it_rseg INTO wa_rseg.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_rseg-ebeln.
    IF sy-subrc IS INITIAL.

      APPEND wa_rseg TO it_rseg2.
      wa_bkpfa-bukrs = wa_rseg-bukrs.
      wa_bkpfa-werks = wa_rseg-werks.
      wa_bkpfa-gjahr = wa_rseg-gjahr.
      wa_bkpfa-ebeln = wa_rseg-ebeln.
      READ TABLE it_rbkp INTO wa_rbkp WITH KEY belnr = wa_rseg-belnr.
      wa_bkpfa-rmwwr = wa_rbkp-rmwwr.
      wa_bkpfa-lifnr = wa_rbkp-lifnr.
      wa_bkpfa-j_1bnftype = wa_rbkp-j_1bnftype."MODIFICAÇÃO 13/09/2016
      wa_bkpfa-in_rbkp = 'X'."MODIFICAÇÃO 13/09/2016
      wa_bkpfa-stblg = wa_rbkp-stblg."MODIFICAÇÃO 13/09/2016
      CONCATENATE wa_rseg-belnr wa_rseg-gjahr INTO wa_bkpfa-awkey.
      CONCATENATE wa_rseg-belnr wa_rseg-gjahr INTO wa_bkpfa-refkey.

      APPEND wa_bkpfa TO it_bkpfa.
    ELSE.
      DELETE it_rbkp WHERE belnr EQ wa_rseg-belnr.
    ENDIF.
  ENDLOOP.

  IF it_bkpfa[] IS NOT INITIAL.
    SELECT a~docnum a~nftype a~nftot b~maktx a~pstdat a~docdat b~nbm b~refkey
      APPENDING CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
      FROM j_1bnfdoc AS a INNER JOIN j_1bnflin AS b ON a~docnum = b~docnum
       FOR ALL ENTRIES IN it_bkpfa
    WHERE b~refkey EQ it_bkpfa-refkey.
  ENDIF.

  CLEAR: it_rseg[].
  MOVE it_rseg2[] TO it_rseg[].

  CHECK NOT it_bkpfa[] IS INITIAL.

  SELECT * INTO TABLE it_bkpf
    FROM bkpf
     FOR ALL ENTRIES IN it_bkpfa
   WHERE bukrs EQ it_bkpfa-bukrs
     AND gjahr EQ it_bkpfa-gjahr
     AND awkey EQ it_bkpfa-awkey
     AND usnam IN s_usnam.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_with_item
    FROM with_item
     FOR ALL ENTRIES IN it_bkpf
   WHERE bukrs EQ it_bkpf-bukrs
     AND belnr EQ it_bkpf-belnr
     AND gjahr EQ it_bkpf-gjahr.

  LOOP AT it_with_item INTO wa_with_item.
    DELETE it_bkpf WHERE bukrs EQ wa_with_item-bukrs
                     AND belnr EQ wa_with_item-belnr
                     AND gjahr EQ wa_with_item-gjahr.
  ENDLOOP.

  CHECK NOT it_bkpf[] IS INITIAL.

  CLEAR: t_saida.

  SELECT bukrs branch name
    FROM j_1bbranch INTO TABLE t_j_1bbranch
     FOR ALL ENTRIES IN it_bkpfa
   WHERE bukrs  EQ it_bkpfa-bukrs
     AND branch EQ it_bkpfa-werks.

  SORT t_j_1bbranch BY bukrs branch name.

  DELETE ADJACENT DUPLICATES FROM t_j_1bbranch COMPARING ALL FIELDS.

  SELECT lifnr stcd2 stcd1 name1 stenr stkzn ort01 regio
    FROM lfa1 INTO TABLE t_lfa1
     FOR ALL ENTRIES IN it_rbkp
   WHERE lifnr EQ it_rbkp-lifnr.

  SORT t_lfa1 BY lifnr stcd2 stcd1 name1 stenr stkzn ort01.

  DELETE ADJACENT DUPLICATES FROM t_lfa1 COMPARING ALL FIELDS.

  LOOP AT it_bkpf INTO wa_bkpf.

    READ TABLE it_bkpfa INTO wa_bkpfa WITH KEY bukrs = wa_bkpf-bukrs
                                               gjahr = wa_bkpf-gjahr
                                               awkey = wa_bkpf-awkey.
    IF sy-subrc IS INITIAL.

      t_saida-rmwwr      = wa_bkpfa-rmwwr.
      t_saida-ebeln      = wa_bkpfa-ebeln.
      t_saida-bukrs      = wa_bkpfa-bukrs.
      t_saida-gsber      = wa_bkpfa-werks.
      t_saida-j_1bnftype = wa_bkpfa-j_1bnftype."MODIFICAÇÃO 13/09/2016
      t_saida-in_rbkp    = wa_bkpfa-in_rbkp."MODIFICAÇÃO 13/09/2016
      t_saida-stblg      = wa_bkpfa-stblg."MODIFICAÇÃO 13/09/2016

      READ TABLE t_j_1bbranch WITH KEY bukrs = wa_bkpfa-bukrs branch = wa_bkpfa-werks.
      IF sy-subrc IS INITIAL.
        t_saida-name  = t_j_1bbranch-name.
      ENDIF.

      READ TABLE t_j_1bnfdoc WITH KEY refkey = wa_bkpfa-refkey.
      IF ( sy-subrc EQ 0 ) AND ( wa_bkpfa-refkey IS NOT INITIAL ).
        t_saida-nbm = t_j_1bnfdoc-nbm.
      ENDIF.

      READ TABLE t_lfa1 WITH KEY wa_bkpfa-lifnr.
      IF sy-subrc IS INITIAL.
        t_saida-lifnr = t_lfa1-lifnr.
        t_saida-name1 = t_lfa1-name1.
        t_saida-regio = t_lfa1-regio.
        IF t_lfa1-stkzn EQ 'X'.
          MOVE: t_lfa1-stcd2 TO t_saida-stcd2.
        ELSE.
          MOVE: t_lfa1-stcd1 TO t_saida-stcd2.
        ENDIF.
        t_saida-stenr = t_lfa1-stenr.
        t_saida-ort01 = t_lfa1-ort01.

      ENDIF.

    ELSE.
      CLEAR: t_saida-rmwwr, t_saida-ebeln.
    ENDIF.

    t_saida-belnr     = wa_bkpf-belnr.
    t_saida-budat     = wa_bkpf-budat.
    t_saida-bldat     = wa_bkpf-bldat.
    t_saida-gjahr     = wa_bkpf-gjahr.
    t_saida-nfnum     = wa_bkpf-xblnr.
    t_saida-usnam     = wa_bkpf-usnam.
    "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
    t_saida-awkey     = wa_bkpf-awkey."miro

    SELECT docnum FROM j_1bnfdoc
      INTO t_saida-xblnr
      WHERE belnr = wa_bkpf-xblnr(10)
        AND gjahr = wa_bkpf-xblnr+10(4).
    ENDSELECT.
*    t_saida-xblnr     = wa_bkpf-xblnr."fiscal

    SELECT ebeln FROM rseg
    INTO t_saida-ebeln
    WHERE belnr = t_saida-awkey(10)
     AND gjahr = t_saida-awkey+10(4).
    ENDSELECT.
    "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
    APPEND t_saida.

  ENDLOOP.
  DELETE t_saida WHERE augdt NOT IN s_augdt.
  DELETE t_saida WHERE bldat NOT IN s_bldat.
  DELETE t_saida WHERE lifnr NOT IN s_lifnr.

ENDFORM.                    " F_SELECIONA_DADOS_SEM


*&---------------------------------------------------------------------*
*&      Form  ALV_IMPRIME_SEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_imprime_sem .

  PERFORM alv_catalog_sem.
  PERFORM alv_layout.
  PERFORM alv_sort.
  PERFORM alv_output.

ENDFORM.                    " ALV_IMPRIME_SEM
*&---------------------------------------------------------------------*
*&      Form  ALV_CATALOG_SEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_catalog_sem .


  REFRESH fieldcat.
  w_pos = 0.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BUKRS'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'BUKRS'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Empresa'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'GSBER'.
  fieldcat-ref_tabname   = 'BSAK'.
  fieldcat-ref_fieldname = 'GSBER'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Filial'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'DIA_VENCIMENTO'.
  fieldcat-ref_tabname   = 'ZFIT0140'.
  fieldcat-ref_fieldname = 'DIA_VENCIMENTO'.
  fieldcat-just          = 'C'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Dt.Vcto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NAME'.
  fieldcat-ref_tabname   = 'J_1BBRANCH'.
  fieldcat-ref_fieldname = 'NAME'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Nome da filial'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BELNR'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'BELNR'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Doc. Contabil'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BUDAT'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'BUDAT'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Data'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'BLDAT'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'BLDAT'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Data Docto'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'GJAHR'.
  fieldcat-ref_tabname   = 'WITH_ITEM'.
  fieldcat-ref_fieldname = 'GJAHR'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Ano'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'STCD2'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'STCD1'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'CNPJ\CPF'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'LIFNR'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'LIFNR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cód.Fornecedor'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NAME1'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'NAME1'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nome do Fornecedor'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'EBELN'.
  fieldcat-ref_tabname   = 'RSEG'.
  fieldcat-ref_fieldname = 'EBELN'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Doc. Compras'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'STENR'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'STENR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'PIS\NT'.
  APPEND fieldcat.
*--- INÍCIO MODIFICAÇÃO 13/09/2016 ---*
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'J_1BNFTYPE'.
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'J_1BNFTYPE'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Cat. Nota'.
  APPEND fieldcat.
*--- FIM MODIFICAÇÃO 13/09/2016 ---*
  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NFNUM'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'XBLNR'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nro. NF'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'RMWWR'.
  fieldcat-ref_tabname   = 'RBKP'.
  fieldcat-ref_fieldname = 'RMWWR'.
  fieldcat-reptext_ddic = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-seltext_s = 'Valor Nota'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'ORT01'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'ORT01'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Município do fornecedor'.
  APPEND fieldcat.


  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'REGIO'.
  fieldcat-ref_tabname   = 'LFA1'.
  fieldcat-ref_fieldname = 'REGIO'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'UF'.
  APPEND fieldcat.


  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'USNAM'.
  fieldcat-ref_tabname   = 'BKPF'.
  fieldcat-ref_fieldname = 'USNAM'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'Nome do Usuário'.
  APPEND fieldcat.

  CLEAR fieldcat.
  ADD 1 TO w_pos.
  fieldcat-col_pos       = w_pos.
  fieldcat-tabname       = 'T_SAIDA'.
  fieldcat-fieldname     = 'NBM'.
  fieldcat-ref_tabname   = 'J_1BNFLIN'.
  fieldcat-ref_fieldname = 'NBM'.
  fieldcat-seltext_s = fieldcat-seltext_l = fieldcat-seltext_m = fieldcat-reptext_ddic = 'NCM'.
  APPEND fieldcat.

ENDFORM.                    " ALV_CATALOG_SEM
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_MIRO_ATIVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_miro_ativa .
  DELETE t_saida
    WHERE in_rbkp NE 'X'
    OR stblg IS NOT INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_MIRO_ESTORNADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_miro_estornada .
  DELETE t_saida
    WHERE in_rbkp NE 'X'
    OR stblg IS INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_MIRO_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_miro_total .
  DELETE t_saida
    WHERE in_rbkp NE 'X'.
ENDFORM.

FORM f_sel_dados_sem_zgl059.

  DATA: "IT_LFBW      TYPE TABLE OF LFBW INITIAL SIZE 0 WITH HEADER LINE,
    tg_mara      TYPE TABLE OF mara INITIAL SIZE 0 WITH HEADER LINE,
    it_bkpf      TYPE TABLE OF bkpf INITIAL SIZE 0 WITH HEADER LINE,
    it_with_item TYPE TABLE OF with_item INITIAL SIZE 0 WITH HEADER LINE,
    wa_bkpf      TYPE bkpf,
    wa_with_item TYPE with_item.

  CLEAR: t_zglt081[], it_with_item[], tg_mara[].

  SELECT a~docnum a~nfenum a~series a~bukrs a~gsber a~doc_lcto a~budat a~operacao b~lifnr a~matnr a~asnum a~sem_nf a~netwr
     INTO CORRESPONDING FIELDS OF TABLE t_zglt081
    FROM zglt081 AS a INNER JOIN zglt080 AS b ON a~seq_lcto = b~seq_lcto
   WHERE a~bukrs   IN s_bukrs
     AND a~budat   IN s_budat
     AND b~lifnr   IN s_lifnr.

  "DELETE T_ZGLT081 WHERE ( DOCNUM IS INITIAL ) OR
  "                       ( NFENUM IS INITIAL ) OR
  "                       ( LIFNR  IS INITIAL ).

  DELETE t_zglt081 WHERE ( nfenum IS INITIAL ) OR
                         ( lifnr  IS INITIAL ).

  CHECK t_zglt081[] IS NOT INITIAL.


**********************************************************************
* PSA CONVERT MATNR 18

*** Formata o código do material
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = t_zglt081-matnr "campo de 400char
    IMPORTING
      output = aux_matnr.

  CLEAR: t_zglt081-matnr.

  t_zglt081-matnr = aux_matnr.

  CLEAR: aux_matnr.

* END CONVERT
**********************************************************************


  SELECT *
    FROM mara INTO TABLE tg_mara
     FOR ALL ENTRIES IN t_zglt081
   WHERE matnr = t_zglt081-matnr.

*  SELECT *
*    FROM LFBW INTO TABLE IT_LFBW
*     FOR ALL ENTRIES IN T_ZGLT081
*   WHERE LIFNR = T_ZGLT081-LIFNR
*     AND BUKRS = T_ZGLT081-BUKRS.
*
*  CHECK IT_LFBW[] IS NOT INITIAL.

  LOOP AT t_zglt081.
*    READ TABLE IT_LFBW WITH KEY LIFNR = T_ZGLT081-LIFNR
*                                BUKRS = T_ZGLT081-BUKRS.
*    IF SY-SUBRC NE 0.
*      DELETE T_ZGLT081.
*    ELSE.
    CONCATENATE 'ZGL17' t_zglt081-doc_lcto t_zglt081-budat(4)
           INTO t_zglt081-obj_key.
    MODIFY t_zglt081.
*    ENDIF.
  ENDLOOP.

  CHECK t_zglt081[] IS NOT INITIAL.

  SELECT *
    FROM zglt082 INTO CORRESPONDING FIELDS OF TABLE t_zglt082
     FOR ALL ENTRIES IN t_zglt081
   WHERE operacao EQ t_zglt081-operacao.

  "CHECK T_ZGLT082[] IS NOT INITIAL.

  SELECT *
    FROM zib_contabil_chv INTO CORRESPONDING FIELDS OF TABLE t_zib_chv
     FOR ALL ENTRIES IN t_zglt081
   WHERE obj_key EQ t_zglt081-obj_key.

  CHECK t_zib_chv[] IS NOT INITIAL.

  SELECT *
    FROM bkpf INTO TABLE it_bkpf
     FOR ALL ENTRIES IN t_zib_chv
   WHERE bukrs EQ t_zib_chv-bukrs
     AND belnr EQ t_zib_chv-belnr
     AND gjahr EQ t_zib_chv-gjahr
     AND usnam IN s_usnam.

  CHECK it_bkpf[] IS NOT INITIAL.

  SELECT * INTO TABLE it_with_item
    FROM with_item
     FOR ALL ENTRIES IN it_bkpf
   WHERE bukrs EQ it_bkpf-bukrs
     AND belnr EQ it_bkpf-belnr
     AND gjahr EQ it_bkpf-gjahr.

  LOOP AT it_with_item INTO wa_with_item.
    DELETE it_bkpf WHERE bukrs EQ wa_with_item-bukrs
                     AND belnr EQ wa_with_item-belnr
                     AND gjahr EQ wa_with_item-gjahr.
  ENDLOOP.

  CHECK NOT it_bkpf[] IS INITIAL.

  CLEAR: t_saida.

  SELECT bukrs branch name
    FROM j_1bbranch INTO TABLE t_j_1bbranch
     FOR ALL ENTRIES IN t_zglt081
   WHERE bukrs  EQ t_zglt081-bukrs
     AND branch EQ t_zglt081-gsber.

  SORT t_j_1bbranch BY bukrs branch name.

  DELETE ADJACENT DUPLICATES FROM t_j_1bbranch COMPARING ALL FIELDS.

  SELECT a~docnum a~nftype a~nftot b~maktx a~pstdat a~docdat b~nbm b~refkey
    INTO CORRESPONDING FIELDS OF TABLE t_j_1bnfdoc
    FROM j_1bnfdoc AS a INNER JOIN j_1bnflin AS b ON a~docnum = b~docnum
     FOR ALL ENTRIES IN t_zglt081
   WHERE a~docnum EQ t_zglt081-docnum.

  SELECT lifnr stcd2 stcd1 name1 stenr stkzn ort01 regio
    FROM lfa1 INTO TABLE t_lfa1
     FOR ALL ENTRIES IN t_zglt081
   WHERE lifnr EQ t_zglt081-lifnr.

  SORT t_lfa1 BY lifnr stcd2 stcd1 name1 stenr stkzn ort01.

  DELETE ADJACENT DUPLICATES FROM t_lfa1 COMPARING ALL FIELDS.

  LOOP AT it_bkpf INTO wa_bkpf.

    CLEAR: tg_mara, t_zglt082, t_j_1bnfdoc.

    READ TABLE t_zib_chv WITH KEY bukrs = wa_bkpf-bukrs
                                  belnr = wa_bkpf-belnr
                                  gjahr = wa_bkpf-gjahr.

    CHECK sy-subrc = 0.

    READ TABLE t_zglt081 WITH KEY obj_key = t_zib_chv-obj_key.

    CHECK sy-subrc = 0.

**********************************************************************
* PSA CONVERT MATNR 18

*** Formata o código do material
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = t_zglt081-matnr "campo de 400char
      IMPORTING
        output = aux_matnr.

    CLEAR: t_zglt081-matnr.

    t_zglt081-matnr = aux_matnr.

    CLEAR: aux_matnr.

* END CONVERT
**********************************************************************


    IF t_zglt081-matnr IS NOT INITIAL.
      READ TABLE tg_mara WITH KEY matnr = t_zglt081-matnr.
    ENDIF.

    "Check Prestação de Serviço
    CHECK ( t_zglt081-asnum  IS NOT INITIAL ) OR "Código de Serviço
          ( t_zglt081-matnr  IS NOT INITIAL  AND tg_mara-mtart = 'ZDIE' ). "ZDIE = Prestação de Serviço


    READ TABLE t_zglt082 WITH KEY operacao = t_zglt081-operacao.

    "CHECK SY-SUBRC = 0.

    READ TABLE t_j_1bnfdoc WITH KEY docnum = t_zglt081-docnum.

    "CHECK SY-SUBRC = 0.

    t_saida-rmwwr      = t_j_1bnfdoc-nftot.
    IF ( t_zglt081-sem_nf IS NOT INITIAL ) AND ( t_saida-rmwwr IS INITIAL ).
      t_saida-rmwwr      = t_zglt081-netwr.
    ENDIF.
    t_saida-bukrs      = t_zglt081-bukrs.
    t_saida-gsber      = t_zglt081-gsber.
    t_saida-j_1bnftype = t_j_1bnfdoc-nftype.
    t_saida-nbm        = t_j_1bnfdoc-nbm.
    t_saida-in_rbkp    = 'X'.

    READ TABLE t_j_1bbranch WITH KEY bukrs  = t_zglt081-bukrs
                                     branch = t_zglt081-gsber.
    IF sy-subrc IS INITIAL.
      t_saida-name  = t_j_1bbranch-name.
    ENDIF.

    READ TABLE t_lfa1 WITH KEY t_zglt081-lifnr.
    IF sy-subrc IS INITIAL.
      t_saida-lifnr = t_lfa1-lifnr.
      t_saida-name1 = t_lfa1-name1.
      t_saida-regio = t_lfa1-regio.
      IF t_lfa1-stkzn EQ 'X'.
        MOVE: t_lfa1-stcd2 TO t_saida-stcd2.
      ELSE.
        MOVE: t_lfa1-stcd1 TO t_saida-stcd2.
      ENDIF.
      t_saida-stenr = t_lfa1-stenr.
      t_saida-ort01 = t_lfa1-ort01.
    ENDIF.

    t_saida-belnr     = wa_bkpf-belnr.
    t_saida-budat     = wa_bkpf-budat.
    t_saida-bldat     = wa_bkpf-bldat.
    t_saida-gjahr     = wa_bkpf-gjahr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = t_zglt081-nfenum
      IMPORTING
        output = t_zglt081-nfenum.

    CONCATENATE t_zglt081-nfenum '-' t_zglt081-series INTO t_saida-nfnum.

    t_saida-usnam     = wa_bkpf-usnam.
    APPEND t_saida.

  ENDLOOP.




  DELETE t_saida WHERE augdt NOT IN s_augdt.
  DELETE t_saida WHERE bldat NOT IN s_bldat.
  DELETE t_saida WHERE lifnr NOT IN s_lifnr.

ENDFORM.                    " F_SELECIONA_DADOS_SEM

FORM f_atrib_dia_vencimento .

  t_saida_aux[] = t_saida[].

  DELETE t_saida_aux WHERE gsber IS INITIAL.
  SORT t_saida_aux BY gsber.
  DELETE ADJACENT DUPLICATES FROM t_saida_aux COMPARING gsber.

  CHECK t_saida_aux[] IS NOT INITIAL.

  SELECT *
    FROM j_1bbranch INTO TABLE @DATA(_tg_branch)
     FOR ALL ENTRIES IN @t_saida_aux
   WHERE branch = @t_saida_aux-gsber.

  CHECK _tg_branch[] IS NOT INITIAL.

  SELECT *
    FROM adrc INTO TABLE @DATA(_tg_adrc)
     FOR ALL ENTRIES IN @_tg_branch
   WHERE addrnumber = @_tg_branch-adrnr.

  CHECK _tg_adrc[] IS NOT INITIAL.

  SELECT *
    FROM zfit0140 INTO TABLE @DATA(_tg_0140)
     FOR ALL ENTRIES IN @_tg_adrc
   WHERE txjcd = @_tg_adrc-taxjurcode.

  CHECK _tg_0140[] IS NOT INITIAL.

  SORT: _tg_branch BY branch,
        _tg_adrc   BY addrnumber,
        _tg_0140   BY txjcd.

  LOOP AT t_saida.

    READ TABLE _tg_branch INTO DATA(_wl_branch) WITH KEY branch = t_saida-gsber.
    CHECK sy-subrc = 0.

    READ TABLE _tg_adrc INTO DATA(_wl_adrc) WITH KEY addrnumber = _wl_branch-adrnr.
    CHECK sy-subrc = 0.

    READ TABLE _tg_0140 INTO DATA(_wl_0140) WITH KEY txjcd = _wl_adrc-taxjurcode.
    CHECK sy-subrc = 0.

    t_saida-dia_vencimento = _wl_0140-dia_vencimento.
    MODIFY t_saida.
  ENDLOOP.

  "#108338 - Alteração Guilherme Rabelo Inicio >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ANEXA_VISUALIZA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_anexa_visualiza_arquivo .

  DATA: ls_object    TYPE sibflporb,
        save_request TYPE sgs_flag.

  READ TABLE t_bkpf INTO DATA(lsbkpf) WITH KEY belnr = t_saida-belnr.


*  CONCATENATE t_bkpf-bukrs t_saida-belnr t_bkpf-gjahr INTO ls_object-instid.
*  ls_object-instid = t_saida-BELNR."como pegar a ov?

*  ls_object-typeid = 'SAPMF05L'."sy-cprog.

*wbarbosa 16/12/2024
  CHECK sy-subrc IS INITIAL.

  ls_object-instid = lsbkpf-awkey.
  ls_object-typeid = 'BUS2081'.
  ls_object-catid  = 'BO'.
*wbarbosa 16/12/2024

  CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
    EXPORTING
      is_object       = ls_object
      ip_mode         = 'E' " Edit mode
    IMPORTING
      ep_save_request = save_request.
  IF save_request = 'X'.
    COMMIT WORK.
  ENDIF.
  "Atualiza Tela da ALV
*  obj_ret->refresh( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFIVA_ICONE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifiva_icone_arquivo .

  DATA:
    lv_logical_system	    TYPE bapibds01-log_system, "
    lt_gos_connections    TYPE STANDARD TABLE OF bdn_con, "
    lv_no_objects_found	  TYPE bdn_con, "
    lv_classname          TYPE bapibds01-classname, "
    lv_internal_error	    TYPE bapibds01, "
    lv_objkey	            TYPE swotobjid-objkey, "
    lv_internal_gos_error	TYPE swotobjid, "
    lv_client	            TYPE sy-mandt. "   SY-MANDT

  lv_classname = sy-cprog.
*  lv_objkey = wg_saida-nro_sol_ov. pega a ov??
  lv_client = sy-mandt.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'  "
    EXPORTING
      classname          = lv_classname
      objkey             = lv_objkey
      client             = lv_client
    TABLES
      gos_connections    = lt_gos_connections
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3.

  IF lines( lt_gos_connections ) > 0.
    t_saida-anexo = icon_attachment.
  ELSE.
    t_saida-anexo = icon_aggregate.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_pega_obs .

  DATA: v_name TYPE thead-tdname.
  DATA:t_lines TYPE  TABLE OF tline.

  CONCATENATE 'ZFIS001'  t_saida-bukrs t_saida-belnr t_saida-ebeln t_saida-hkont t_saida-witht INTO v_name.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'BEST'
      language                = sy-langu
      name                    = v_name
      object                  = 'MATERIAL'
    TABLES
      lines                   = t_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.

  ENDIF.

  CLEAR t_saida-obs.
  LOOP AT t_lines INTO DATA(ls_tline).
    CONCATENATE t_saida-obs ls_tline-tdline INTO t_saida-obs SEPARATED BY space.

  ENDLOOP.
  "#108338 - Alteração Guilherme Rabelo Fim >>> - EQUALIZAÇÃO ECC X HANA - SMC <<<
ENDFORM.


FORM f_estornar.

  DATA: lva_obj_key          TYPE zib_contabil_chv-obj_key,
        lva_stblg            TYPE bkpf-stblg,
        lwa_zib_contabil_chv TYPE zib_contabil_chv,
        lwa_bsak             TYPE bsak,
        lwa_zfit0171         TYPE zfit0171,
        lwa_zglt035          TYPE zglt035,
        lit_zfit0171         TYPE TABLE OF zfit0171,
        lwa_mensagem(50).

  o_alv->get_selected_rows(
    IMPORTING
      et_index_rows = DATA(lit_rows) ).

  CHECK ( lit_rows[] IS NOT INITIAL ).
  LOOP AT lit_rows[] INTO DATA(lwa_rows).
    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX lwa_rows-index.

    CHECK sy-subrc EQ 0.

    IF <fs_saida>-imp_gerado  = icon_execute_object OR <fs_saida>-imp_gerado IS INITIAL.
      MESSAGE s000(z01) WITH 'Documento não foi processado' DISPLAY LIKE 'E'.
      CONTINUE.
    ENDIF.

* - Para buscar doc contábil com erro
    SELECT gjahr FROM zib_contabil_chv
        INTO @DATA(v_ano)
        WHERE belnr = @<fs_saida>-imp_gerado.
    ENDSELECT.

    CLEAR: lwa_bsak.
    SELECT SINGLE * FROM bsak INTO lwa_bsak
     WHERE bukrs       = <fs_saida>-bukrs
          AND gjahr    = v_ano
          AND belnr    = <fs_saida>-imp_gerado.

    IF lwa_bsak IS INITIAL. " Estorno Permitido.
*Estorno documento contábil

      SELECT SINGLE awkey FROM bkpf INTO lva_obj_key
      WHERE belnr = <fs_saida>-imp_gerado AND
            bukrs =  <fs_saida>-bukrs.

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
        CONCATENATE 'Documento' <fs_saida>-imp_gerado 'não foi estornado' INTO lwa_mensagem SEPARATED BY space.
        MESSAGE lwa_mensagem TYPE 'I'.
        EXIT.
      ELSE.
* - Estorno do documento 'Doc_Lcto'
        CLEAR ls_lote.
        SELECT * FROM zmm_controle_lt
       INTO ls_lote
       WHERE witht = <fs_saida>-witht
         AND xblnr = <fs_saida>-xblnr.
        ENDSELECT.
        CLEAR: lwa_zglt035.
        SELECT SINGLE *
         FROM zglt035
         INTO lwa_zglt035
       WHERE doc_lcto EQ ls_lote-lote.

        lwa_zglt035-loekz =  'X'.

        MODIFY zglt035 FROM lwa_zglt035.
        COMMIT WORK.


        <fs_saida>-imp_gerado = icon_execute_object.
        <fs_saida>-doc_lanc = ' '.
*                 <fs_saida>-dt_lcto_ajuste,
*                 <fs_saida>-nr_lote,
*                 <fs_saida>-belnr_zib.

        "APPEND lwa_zfit0171 TO lit_zfit0171.

        "MODIFY zfit0171  FROM TABLE lit_zfit0171.
        COMMIT WORK.



      ENDIF.
    ELSE.
      CONCATENATE 'Documento' <fs_saida>-imp_gerado 'esta compensado. Impossível estornar' INTO lwa_mensagem SEPARATED BY space.
      MESSAGE lwa_mensagem TYPE 'I'.
    ENDIF.
  ENDLOOP.

ENDFORM.
