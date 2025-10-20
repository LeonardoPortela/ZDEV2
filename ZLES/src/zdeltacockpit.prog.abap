*&---------------------------------------------------------------------*
*& Report  ZDELTACOCKPIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zdeltacockpit.

TABLES: zlest0015,
        zlest0016,
        zlest0020,
        zlest0013,
        vttk.

CONSTANTS:

  cc_i                                     VALUE 'I',
  cc_e                                     VALUE 'E',
  cc_s                                     VALUE 'S',
  cc_kappl                 TYPE c          VALUE 'F',
  cc_eq(2)                 TYPE c          VALUE 'EQ',
  cc_cp(2)                 TYPE c          VALUE 'CP',
* ---> S4 Migration - 07/07/2023 - GB - Inicio
*  cc_refty_8               TYPE vbtyp      VALUE '8',
  cc_refty_8               TYPE vbtypl      VALUE '8',
* <--- S4 Migration - 07/07/2023 - GB - Fim
  cc_unid_peso_default     TYPE gewei      VALUE 'KG',
  cc_idinter_l1            TYPE zidinter   VALUE 'L1',
  cc_tp_reg_30             TYPE ztp_reg    VALUE '30',
  cc_fkpty_z001            TYPE fkpty      VALUE 'Z001',
  cc_add02_03              TYPE vttk_add02 VALUE '0000000003',
  cc_vbtypv_j              TYPE vbtypl     VALUE 'J',
  cc_vbtypn_m              TYPE vbtypl     VALUE 'M',
  cc_vbtypn_r              TYPE vbtypl     VALUE 'R',
  cc_reftyp_bi             TYPE j_1breftyp VALUE 'BI',
  cc_reftyp_md             TYPE j_1breftyp VALUE 'MD',
  cc_kschl_zmrg            TYPE kscha      VALUE 'ZMRG',
  cc_kschl_zadm            TYPE kscha      VALUE 'ZADM',
  cc_kschl_ziof            TYPE kscha      VALUE 'ZIOF',
  cc_kschl_zfre            TYPE kscha      VALUE 'ZFRE',
  cc_kschl_zseg            TYPE kscha      VALUE 'ZSEG'.


RANGES: rc_kschl           FOR konv-kschl       OCCURS 15.

TYPES:

       BEGIN OF ty_nfbalanca,
         docnum                    TYPE j_1bdocnum,
         nfenum                    TYPE j_1bnfnum9,
         nfnum                     TYPE j_1bnfnumb,
         series                    TYPE j_1bseries,
         bukrs                     TYPE bukrs,
         branch                    TYPE j_1bbranc_,
         gewei                     TYPE gewei,
         cnpj                      TYPE stcd1,
         data_chega                TYPE zdata_chega,
         peso_chega                TYPE zpeso_chega,
       END   OF ty_nfbalanca,

       BEGIN OF ty_notafiscal,
         docnum                    TYPE j_1bdocnum,
         nfenum                    TYPE j_1bnfnum9,
         nfnum                     TYPE j_1bnfnumb,
         series                    TYPE j_1bseries,
         bukrs                     TYPE bukrs,
         branch                    TYPE j_1bbranc_,
         gewei                     TYPE gewei,
*        Expansão para acesso
         chave                     TYPE zchaveid,
       END   OF ty_notafiscal,

       BEGIN OF ty_balanca,
         chave                     TYPE zchaveid,
         dtachegada                TYPE zdtachegada,
         pesodvagao                TYPE zpesovagao,
       END   OF ty_balanca,

       BEGIN OF ty_j_1bnflin,
         refkey                    TYPE j_1brefkey,
         docnum                    TYPE j_1bdocnum,
         meins                     TYPE meins,
       END   OF ty_j_1bnflin,

       BEGIN OF ty_vbfa,
         vbelv                     TYPE vbeln_von,
         posnv                     TYPE posnr_von,
         vbeln                     TYPE vbeln_nach,
         posnn                     TYPE posnr_nach,
* ---> S4 Migration - 07/07/2023 - GB - Inicio
*         vbtyp_n                   TYPE vbtyp_n,
         vbtyp_n                   TYPE vbtypl_n,
* <--- S4 Migration - 07/07/2023 - GB - Fim
*        Expansão para acesso
         refkey                    TYPE j_1brefkey,
       END   OF ty_vbfa,

       BEGIN OF ty_conhecimento,
         codtrp                    TYPE zcodtrp,
         codposto                  TYPE zcodposto,
         lote                      TYPE char10,
         status                    TYPE ztatuslote,
         datalote                  TYPE zdatalote,
         chvid                     TYPE zchvid,
         ctafrete                  TYPE zctafrete,
         conhec                    TYPE zconhec,
         dta_chegada               TYPE dareg,
         vlrlote                   TYPE zvlrlote,
         vlrconhec                 TYPE zvlrconhec,
         qtde                      TYPE zqtde1,
         id_origem_zles(2)         TYPE n,
       END   OF ty_conhecimento,

       BEGIN OF ty_tolerancia,
         tplst                     TYPE tplst,
         bukrs                     TYPE bukrs,
         tolerancia                TYPE zlesltol,
       END   OF ty_tolerancia,

       BEGIN OF ty_margtoler,
         vbeln                     TYPE vbeln_vl,
         matnr                     TYPE matnr,
         knumh                     TYPE knumh,
         kbetr                     TYPE kbetr_kond,
       END   OF ty_margtoler,

       BEGIN OF ty_transporte,
         tknum                     TYPE tknum,
         tdlnr                     TYPE tdlnr,
         exti1                     TYPE exti1,
         exti2                     TYPE exti2,
         tplst                     TYPE tplst,
         add02                     TYPE vttk_add02,
         bukrs                     TYPE bukrs,
         dsc_tplst                 TYPE bezei,
       END   OF ty_transporte,
       BEGIN OF ty_remessa,
         tknum                     TYPE tknum,
         vbeln                     TYPE vbeln_vl,
         btgew                     TYPE gsgew,
         gewei                     TYPE gewei,
       END   OF ty_remessa,

       BEGIN OF ty_vfkp,
         fknum                     TYPE fknum,
         fkpos                     TYPE fkpos,
         knumv                     TYPE knumv,
         kzwi1                     TYPE kzwi1,
         kzwi2                     TYPE kzwi2,
         rebel                     TYPE rebel,
         werks                     TYPE werks_d,
       END   OF ty_vfkp,

       BEGIN OF ty_konv,
         knumv                     TYPE knumv,
         kwert                     TYPE kwert,
         kbetr                     TYPE kbetr,
         kpein                     TYPE kpein,
         kmein                     TYPE kvmei,
         kschl                     TYPE kschl,
       END   OF ty_konv .


DATA: ti_zlest0015 TYPE TABLE OF zlest0015 WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0020 TYPE STANDARD TABLE OF zlest0020 WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0013 TYPE STANDARD TABLE OF zlest0013 WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0025           TYPE STANDARD TABLE OF zlest0025
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_transporte          TYPE STANDARD TABLE OF ty_transporte
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_remessa             TYPE STANDARD TABLE OF ty_remessa
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_vfkp                TYPE STANDARD TABLE OF ty_vfkp
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_vbfa                TYPE STANDARD TABLE OF ty_vbfa
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_notafiscal          TYPE STANDARD TABLE OF ty_notafiscal
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_balanca             TYPE STANDARD TABLE OF ty_balanca
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_nfbalanca           TYPE STANDARD TABLE OF ty_nfbalanca
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_j_1bnflin           TYPE STANDARD TABLE OF ty_j_1bnflin
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_konv                TYPE STANDARD TABLE OF ty_konv
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_tolerancia          TYPE STANDARD TABLE OF ty_tolerancia
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_margtoler           TYPE STANDARD TABLE OF ty_margtoler
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docpartner          TYPE STANDARD TABLE OF j_1bnfnad
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docitem             TYPE STANDARD TABLE OF j_1bnflin
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docitemtax          TYPE STANDARD TABLE OF j_1bnfstx
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docheadermsg        TYPE STANDARD TABLE OF j_1bnfftx
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docrefermsg         TYPE STANDARD TABLE OF j_1bnfref
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_ext_item            TYPE STANDARD TABLE OF j_1binlin
                                  WITH HEADER LINE INITIAL SIZE 0,
*      ti_conhec              type standard table of ty_conhecimento                                 with header line initial size 0,
      w_lancto              TYPE zles_cockpit_lancto,
      w_delta               TYPE zlest0020,"zles_cockpit_delta,
      w_docheader           TYPE j_1bnfdoc,
      w_ext_header          TYPE j_1bindoc,
      ti_deltas    TYPE TABLE OF zlest0020 .

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK zb01.

SELECT-OPTIONS:
                s_lote  FOR  zlest0015-lote          MODIF ID io1 OBLIGATORY,
                s_perio FOR  vttk-erdat              MODIF ID io1 OBLIGATORY.
*selection-screen comment 46(60) p_dscsta for field p_stats.
SELECTION-SCREEN END OF BLOCK zb01.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM f_seleciona_dados.

PERFORM yf_busca_dadosadic_conhec.
* Busca dados de remessa e transporte
PERFORM yf_busca_dtl_tranporte.
* Busca dados de tolerância para os documentos de remessa
PERFORM yf_busca_tolerancia.
* Busca dados da nota fiscal relacionada ao conhecimento
PERFORM yf_busca_doctos_nf.
* Recalculando os valores deltas do lote
PERFORM f_calcula_delta.
* Grava dados recalculados
PERFORM f_grava_delta.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

* Limpa base de seleção
  REFRESH:
           ti_zlest0013,
           ti_zlest0015,
           ti_zlest0020,
           ti_transporte,
           ti_remessa,
           ti_deltas.


** Selecao dos lotes
  SELECT *
      INTO TABLE ti_zlest0015
      FROM zlest0015
     WHERE
           lote          IN s_lote[]
       AND data          IN s_perio.
  IF sy-subrc IS INITIAL.
    SORT ti_zlest0015  BY transportador posto lote status data.
  ENDIF.

  CHECK: NOT ti_zlest0015[]    IS INITIAL.

* Seleciona os documentos importados para recalculo
  SELECT *
    INTO TABLE ti_zlest0013
    FROM zlest0013
     FOR ALL ENTRIES IN ti_zlest0015
   WHERE codtrp  = ti_zlest0015-transportador
     AND codposto         = ti_zlest0015-posto
     AND lote          = ti_zlest0015-lote.

  CHECK: NOT ti_zlest0013[]    IS INITIAL.
  SORT ti_zlest0013 BY codtrp codposto lote.

ENDFORM.                    " F_SELECIONA_DADOS



*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DADOSADIC_CONHEC
*&---------------------------------------------------------------------*
FORM yf_busca_dadosadic_conhec .

  DATA: BEGIN OF lt_lote        OCCURS 0,
         lote                   TYPE char10,
       END   OF lt_lote,
       BEGIN OF lt_chvid        OCCURS 0,
         chvid                  TYPE zchvid,
       END   OF lt_chvid,
       BEGIN OF lt_conhec_1     OCCURS 0,
         codtrp                 TYPE zcodtrp,
         exti1                  TYPE exti1,
         exti2                  TYPE exti2,
       END   OF lt_conhec_1,
       BEGIN OF lt_conhec_2     OCCURS 0,
         codtrp                 TYPE zcodtrp,
         codposto               TYPE zcodposto,
       END   OF lt_conhec_2.
  DATA: lc_subrc   TYPE i.

* Converte chave para compatibilidade de acesso
* 1) Tamanho conhec na tabela ZLEST0013 é diferente VTTPK
* 2) Tira duplicidades para acesso a base de dados
  CHECK: NOT ti_zlest0013[]    IS INITIAL
     OR  NOT ti_zlest0015[]    IS INITIAL.
*     or  not ti_zlest0016[]    is initial.

* Dados importados do Posto de Gasolina
  LOOP AT ti_zlest0013.
    APPEND ti_zlest0013-lote       TO lt_lote.
    APPEND ti_zlest0013-chvid      TO lt_chvid.
    MOVE: ti_zlest0013-codtrp      TO lt_conhec_1-codtrp,
          ti_zlest0013-conhec      TO lt_conhec_1-exti1,
          ti_zlest0013-ctafrete    TO lt_conhec_1-exti2.
    APPEND lt_conhec_1.
    MOVE: ti_zlest0013-codtrp      TO lt_conhec_2-codtrp,
          ti_zlest0013-codposto    TO lt_conhec_2-codposto.
    APPEND lt_conhec_2.
  ENDLOOP.

** Dados A confirmar ou confirmados Postos - Lotes (Detalhe)
*  loop at ti_zlest0016.
*    append ti_zlest0016-chvid      to lt_chvid.
*  endloop.

* Dados A confirmar ou confirmados Postos - Lotes (Header)
  LOOP AT ti_zlest0015.
    APPEND ti_zlest0015-lote    TO lt_lote.
    MOVE: ti_zlest0015-transportador   TO lt_conhec_2-codtrp,
          ti_zlest0015-posto           TO lt_conhec_2-codposto.
    APPEND lt_conhec_2.
  ENDLOOP.

* Classifica dados de conhecimento para acesso adicionais
  SORT: lt_lote,
        lt_chvid,
        lt_conhec_1,
        lt_conhec_2.

  DELETE ADJACENT DUPLICATES FROM: lt_lote,
                                   lt_chvid,
                                   lt_conhec_1,
                                   lt_conhec_2.

* Controle chave de identificação de lote
  IF NOT lt_chvid[] IS INITIAL.
    SELECT *
      INTO TABLE ti_zlest0025
      FROM zlest0025
       FOR ALL ENTRIES IN lt_chvid
     WHERE chvid = lt_chvid-chvid.

*   Classifica Histórico
    SORT: ti_zlest0025  BY chvid calcockpit.
  ENDIF.

* Dados de transporte / valor de adiantamento (somente para zlest0013)
  IF NOT lt_conhec_1[] IS INITIAL.
    SELECT t1~tknum  t1~tdlnr t1~exti1 t1~exti2 t1~tplst t1~add02
           t2~bukrs t3~bezei
      INTO TABLE ti_transporte
      FROM vttk AS t1
       LEFT OUTER JOIN ttds AS t2
        ON t2~tplst = t1~tplst
      LEFT OUTER JOIN ttdst AS t3
        ON t3~spras = sy-langu
       AND t3~tplst = t1~tplst
       FOR ALL ENTRIES IN lt_conhec_1
     WHERE t1~tdlnr = lt_conhec_1-codtrp
       AND t1~exti1 = lt_conhec_1-exti1
       AND t1~exti2 = lt_conhec_1-exti2.

    IF sy-subrc IS INITIAL.
      SORT ti_transporte BY tknum tdlnr exti1 exti2.
      DELETE ADJACENT DUPLICATES FROM ti_transporte
                      COMPARING tknum tdlnr exti1 exti2.
    ENDIF.
  ENDIF.

ENDFORM.                    " YF_BUSCA_DADOSADIC_CONHEC

*&---------------------------------------------------------------------*
*&      Form  YF_MONTA_RANGE_KSCHL
*&---------------------------------------------------------------------*
FORM yf_monta_range_kschl .

  REFRESH: rc_kschl.
  CLEAR:   rc_kschl.

  rc_kschl-sign = cc_i.
  rc_kschl-option = cc_eq.

  rc_kschl-low = 'ZFRE'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZSET'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZINS'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZIRF'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZADM'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZIOF'.
  APPEND rc_kschl.

  rc_kschl-low = 'ZSEG'.
  APPEND rc_kschl.

ENDFORM.                    " YF_MONTA_RANGE_KSCHL


*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DTL_TRANPORTE
*&---------------------------------------------------------------------*
FORM yf_busca_dtl_tranporte .

  RANGES: rg_vbtyp_n FOR vbfa-vbtyp_n OCCURS 0.
  DATA: lw_range      TYPE lxhme_range_c1.

  CHECK: NOT ti_transporte[] IS INITIAL.

* Custo de frete
  SELECT fknum fkpos knumv kzwi1 kzwi2 rebel werks
    INTO TABLE ti_vfkp
    FROM vfkp
     FOR ALL ENTRIES IN ti_transporte
   WHERE rebel = ti_transporte-tknum
     AND refty = cc_refty_8
     AND fkpty = cc_fkpty_z001.

* Condições de preço
  IF NOT ti_vfkp[] IS INITIAL.

    PERFORM yf_monta_range_kschl.

    SELECT FROM V_KONV FIELDS KNUMV , KWERT , KBETR , KPEIN , KMEIN , KSCHL FOR ALL ENTRIES IN @TI_VFKP WHERE KNUMV = @TI_VFKP-KNUMV AND KSCHL IN @RC_KSCHL INTO TABLE @TI_KONV .

  ENDIF.

* Obtem itens de transporte e dados de remessa
  SELECT t1~tknum t1~vbeln t2~btgew  t2~gewei
    INTO TABLE ti_remessa
    FROM vttp AS t1
    LEFT OUTER JOIN likp AS t2
      ON t2~vbeln = t1~vbeln
     FOR ALL ENTRIES IN ti_transporte
   WHERE t1~tknum = ti_transporte-tknum.

  lw_range-sign   = cc_i.
  lw_range-option = cc_eq.
  lw_range-low    = cc_vbtypn_r.
  APPEND lw_range TO rg_vbtyp_n.
  lw_range-low    = cc_vbtypn_m.
  APPEND lw_range TO rg_vbtyp_n.

* Documentos de faturamentos
*  SELECT vbelv posnv vbeln posnn vbtyp_n
*    INTO TABLE ti_vbfa
*    FROM vbfa
*     FOR ALL ENTRIES IN ti_remessa
*   WHERE vbelv   = ti_remessa-vbeln
*     AND vbtyp_v = cc_vbtypv_j
*     AND vbtyp_n = cc_vbtypn_m.

* Documentos de faturamento e material - transferencia
  SELECT vbelv posnv vbeln posnn vbtyp_n mjahr
    INTO TABLE ti_vbfa
    FROM vbfa
     FOR ALL ENTRIES IN ti_remessa
   WHERE vbelv   = ti_remessa-vbeln
     AND vbtyp_v = cc_vbtypv_j
     AND vbtyp_n IN rg_vbtyp_n.

ENDFORM.                    " YF_BUSCA_DTL_TRANPORTE

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_TOLERANCIA
*&---------------------------------------------------------------------*
FORM yf_busca_tolerancia .

* Tolerância
  REFRESH ti_tolerancia.

* Dados de fornecimento e condição para tolerância
  SELECT DISTINCT t1~vbeln t1~matnr t2~knumh t3~kbetr
    INTO TABLE ti_margtoler
    FROM lips AS t1
   INNER JOIN a912 AS t2
      ON t2~kappl = cc_kappl
     AND t2~kschl = cc_kschl_zmrg
     AND t2~matnr = t1~matnr
     AND t2~datab <= sy-datum
     AND t2~datbi >= sy-datum
   INNER JOIN konp AS t3
      ON t3~knumh = t2~knumh
     FOR ALL ENTRIES IN ti_remessa
   WHERE t1~vbeln = ti_remessa-vbeln.

  CHECK: NOT ti_margtoler[] IS INITIAL.
  SORT ti_margtoler BY vbeln.

* Extrai margem de tolerância para organização de transporte
  LOOP AT ti_remessa.
*   Obtem valor condição tolerância do primeiro material da remessa 1:1
    READ TABLE ti_margtoler WITH KEY vbeln = ti_remessa-vbeln
                            BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.
*   Verifica documento de transporte
    READ TABLE ti_transporte WITH KEY tknum = ti_remessa-tknum
                             BINARY SEARCH.
    CHECK sy-subrc IS INITIAL.
    ti_tolerancia-tplst = ti_transporte-tplst.
    ti_tolerancia-bukrs = ti_transporte-bukrs.
    ti_tolerancia-tolerancia = ti_margtoler-kbetr / 10.
    APPEND ti_tolerancia.
  ENDLOOP.

  FREE ti_margtoler.

ENDFORM.                    " YF_BUSCA_TOLERANCIA


*&---------------------------------------------------------------------*
*&      Form  YF_DADOS_ADIC_REMESSA_TRANSP
*&---------------------------------------------------------------------*
FORM yf_dados_adic_remessa_transp USING p_vlrconhec.

  DATA: li_index             TYPE i,
        li_index2            TYPE i,
        lqbr_vbeln           TYPE vbeln_vl,
        lp_peso_origem       TYPE brgew_ap,
        lp_peso_confirm_all  TYPE brgew_ap,
        lp_peso_hermasa      TYPE brgew_ap,
        lp_vlr_aliq_frete    TYPE kbetr,
        lp_vlr_iof_seguro    TYPE kbetr,
        lp_vlr_vlrorigem     TYPE kwert,
        lp_vlr_impostos      TYPE kwert,
        lp_vlr_nota_fiscal   TYPE kwert,
        lp_vlr_adiantamento  TYPE kwert,
        lp_vlr_seguro        TYPE kwert.
*        vl_unid_peso         type kmein.

* Valor de Origem
  CLEAR: w_delta,
         w_lancto.

  READ TABLE ti_vfkp WITH KEY rebel = ti_transporte-tknum BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    li_index = sy-tabix.
    DO.
      lp_vlr_vlrorigem = lp_vlr_vlrorigem +
                         ti_vfkp-kzwi1. " + ti_vfkp-kzwi2.
      READ TABLE ti_konv WITH KEY knumv = ti_vfkp-knumv
                         BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        li_index2 = sy-tabix.
        DO.
          IF ti_konv-kschl = cc_kschl_zfre.
*           Extrai a unidade de peso para relatório
            w_lancto-unid_peso = ti_konv-kmein.
*           Soma valores  de frete
            lp_vlr_aliq_frete = lp_vlr_aliq_frete + ti_konv-kbetr.
          ELSEIF ti_konv-kschl = cc_kschl_zadm.
*           Soma adiantamento
            lp_vlr_adiantamento = lp_vlr_adiantamento + ti_konv-kbetr.
          ELSEIF ti_konv-kschl = cc_kschl_zseg.
*           Soma valor de seguro
            lp_vlr_seguro = lp_vlr_seguro + ti_konv-kwert.
          ELSE.
*           Soma valores de impostos
            lp_vlr_impostos = lp_vlr_impostos + ti_konv-kwert.
            IF ti_konv-kschl = cc_kschl_ziof.
*             Salva IOF para pessoa juridica
              lp_vlr_iof_seguro = lp_vlr_iof_seguro + ti_konv-kwert.
            ENDIF.
          ENDIF.

          ADD 1 TO li_index2.
          READ TABLE ti_konv INDEX li_index2.
          IF sy-subrc <> 0 OR ti_konv-knumv <> ti_vfkp-knumv.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.

      ADD 1 TO li_index.
      READ TABLE ti_vfkp INDEX li_index.
      IF sy-subrc <> 0 OR ti_vfkp-rebel <> ti_transporte-tknum.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

* Converte para valores absoluto
  lp_vlr_impostos = ABS( lp_vlr_impostos ).

* Determina a unidade de medida a ser devolvida
  IF w_lancto-unid_peso IS INITIAL.
    w_lancto-unid_peso = cc_unid_peso_default.
  ENDIF.

* Lê remessa
  READ TABLE ti_remessa WITH KEY tknum = ti_transporte-tknum
                        BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    li_index = sy-tabix.

*   Inicializa variável de quebra
    REPLACE ALL OCCURRENCES OF REGEX '\s' IN lqbr_vbeln
          WITH cl_abap_char_utilities=>maxchar.

*   Documento de transporte 1:N Remessas
    DO.
*     Soma Peso de Origem na unidade default
      PERFORM yf_converte_vlr_unid_peso USING ti_remessa-gewei
                                              w_lancto-unid_peso
                                     CHANGING ti_remessa-btgew.
      lp_peso_origem = lp_peso_origem + ti_remessa-btgew.

      IF lqbr_vbeln <> ti_remessa-vbeln.
        lqbr_vbeln = ti_remessa-vbeln.
*       Soma Peso Confirmado ALL
        PERFORM yf_soma_peso_confirmado_all USING ti_remessa-vbeln
                                                  w_lancto-unid_peso
                                         CHANGING lp_peso_confirm_all
                                                  lp_vlr_nota_fiscal.
**       Soma Peso Confirmado ALL
*        perform yf_soma_peso_hermasa using ti_remessa-vbeln
*                                           vl_unid_peso
*                                  changing lp_peso_hermasa.
      ENDIF.

      ADD 1 TO li_index.
      READ TABLE ti_remessa INDEX li_index.
      IF sy-subrc <> 0 OR ti_remessa-tknum <> ti_transporte-tknum.
        EXIT.
      ENDIF.
    ENDDO.

  ENDIF.

* Valor Confirmado
  CLEAR ti_tolerancia.
  READ TABLE ti_tolerancia WITH KEY tplst = ti_transporte-tplst
                           BINARY SEARCH.

  IF NOT lp_peso_confirm_all IS INITIAL.
    w_lancto-peso_confirmado = lp_peso_confirm_all.
  ELSEIF NOT lp_peso_hermasa IS INITIAL.
    w_lancto-peso_confirmado = lp_peso_hermasa.
  ELSE.
    w_lancto-peso_confirmado = ti_zlest0013-qtde.
    PERFORM yf_converte_vlr_unid_peso: USING cc_unid_peso_default
                                             w_lancto-unid_peso
                                    CHANGING w_lancto-peso_confirmado.
  ENDIF.

* Soma IOF para seguro quando for pessoa Jurídica
  IF lp_vlr_seguro > 0 AND w_delta-grpconta <> cc_add02_03.
    lp_vlr_seguro = lp_vlr_seguro + lp_vlr_iof_seguro.
  ENDIF.

* Valores base de cálculo
  MOVE-CORRESPONDING ti_zlest0013  TO w_lancto.

  w_delta-transportador    = w_lancto-codtrp.
  w_delta-posto         = w_lancto-codposto.
  w_delta-lote             = w_lancto-lote.
  w_delta-erdat            = w_lancto-datalote.
  w_delta-chvid            = w_lancto-chvid.
  w_delta-conhecimento     = w_lancto-conhec.
*  w_delta-ctafrete         = w_lancto-ctafrete.
  w_delta-vlradiantamento  = lp_vlr_adiantamento.
  w_delta-vlrimp_retidos   = lp_vlr_impostos.
  w_delta-vlrseguro        = lp_vlr_seguro.
  w_delta-vlrfrete         = lp_vlr_aliq_frete.
  w_lancto-vlrorigem       = lp_vlr_vlrorigem.
  w_lancto-peso_origem     = lp_peso_origem.
  w_delta-tolerancia       = ti_tolerancia-tolerancia.
  w_delta-grpconta         = ti_transporte-add02.
  w_delta-unid_peso        = w_lancto-unid_peso.

* Calculo usado no cockpit
  PERFORM yf_cockpit_calculo_deltas USING w_lancto-peso_confirmado
                                          lp_peso_origem
                                          w_lancto-unid_peso
                                          w_delta-grpconta
                                          w_delta-tolerancia
                                          lp_vlr_nota_fiscal
                                          lp_vlr_adiantamento
                                          lp_vlr_impostos
                                          lp_vlr_aliq_frete
                                          lp_vlr_seguro
                                          p_vlrconhec
                                 CHANGING w_lancto-vlrconfirmado
                                          w_lancto-vlrdiferenca
                                          w_lancto-vlrprogramado
                                          w_delta-vlconfirmado_ref
                                          w_delta-vlrperda
                                          w_delta-vlrsobra_quebra
                                          w_delta-difer_transp
                                          w_delta-diferenca_peso
                                          w_delta-quebra_peso
                                          w_delta-quebra_real
                                          w_delta-fator_conversao.

** Atualiza tabela de calculos - Valores Deltas
  APPEND w_delta          TO ti_deltas.

ENDFORM.                    " YF_DADOS_ADIC_REMESSA_TRANSP

*&---------------------------------------------------------------------*
*&      Form  YF_COCKPIT_CALCULO_DELTAS
*&---------------------------------------------------------------------*
FORM yf_cockpit_calculo_deltas  USING p_peso_confirmado
                                      p_peso_origem
                                      p_unid_peso
                                      p_grupo_conta
                                      p_tolerancia
                                      p_vlr_nota_fiscal
                                      p_vlr_adiantamento
                                      p_vlr_impostos
                                      p_vlr_frete
                                      p_vlr_seguro
                                      p_vlr_importado
                             CHANGING s_vlr_confirmado
                                      s_vlr_diferenca
                                      s_vlr_programado
                                      s_vlr_confirm_ref
                                      s_vlr_perda
                                      s_vlr_sobra_quebra
                                      s_difer_transp
                                      s_difer_peso
                                      s_quebra_peso
                                      s_quebra_real
                                      s_fator_conversao.

  DATA: lp_peso_orig_unidef  TYPE zquant17_7,
       	lp_peso_conf_default TYPE zquant17_7,
       	lp_quebra_peso       TYPE zquant17_7,
       	lp_fator_convers     TYPE zquant17_7,
       	lp_difer_transp      TYPE zquant17_7,
       	lp_difer_peso        TYPE zquant17_7,
       	lp_quebra_real       TYPE zquant17_7,
        lp_quantidade_aux1   TYPE zquant17_7,
        lp_quantidade_aux2   TYPE zquant17_7,
        lp_vlr_perda         TYPE kwert,
        lp_vlr_difer_conf    TYPE kwert,
        lp_vlr_sobra_quebra  TYPE kwert.

* Converte valores para unidade default
  lp_peso_conf_default = p_peso_confirmado.
  PERFORM yf_converte_vlr_unid_peso USING p_unid_peso
                                          cc_unid_peso_default
                                 CHANGING lp_peso_conf_default.

  lp_peso_orig_unidef  = p_peso_origem.
  PERFORM yf_converte_vlr_unid_peso USING p_unid_peso
                                          cc_unid_peso_default
                                 CHANGING lp_peso_orig_unidef.

* Calcula valores de peso na unidade default
  lp_difer_peso  = lp_peso_conf_default - lp_peso_orig_unidef.
  lp_quebra_real = ( lp_peso_orig_unidef * p_tolerancia ) / 100.
  lp_quebra_peso = lp_quebra_real - ABS( lp_difer_peso ).
*  IF lp_quebra_peso > 0.
  IF lp_peso_conf_default > lp_peso_orig_unidef OR lp_quebra_peso > 0.
    lp_quebra_peso = 0.
  ENDIF.
  IF lp_peso_orig_unidef > 0.
    lp_fator_convers = p_vlr_nota_fiscal / lp_peso_orig_unidef.
  ELSE.
    lp_fator_convers = 0.
  ENDIF.

  IF lp_quebra_peso = 0.
    CLEAR lp_difer_transp.
  ELSE.
    lp_difer_transp  = lp_quebra_peso * lp_fator_convers.
  ENDIF.

* Calcula valores de moeda na unidade de peso
  IF p_unid_peso = cc_unid_peso_default.
    lp_vlr_difer_conf   = p_vlr_frete * lp_peso_conf_default.
    lp_vlr_sobra_quebra = p_vlr_frete * lp_quebra_peso.
    lp_vlr_perda      = ( p_vlr_frete * lp_peso_conf_default ) -
                        ( p_vlr_frete * lp_peso_orig_unidef  ).
  ELSE.
    lp_quantidade_aux1 = lp_peso_conf_default.
    lp_quantidade_aux2 = lp_quebra_peso.
    PERFORM yf_converte_vlr_unid_peso USING  cc_unid_peso_default
                                             p_unid_peso
                                   CHANGING: lp_quantidade_aux1,
                                             lp_quantidade_aux2.

    lp_vlr_difer_conf   = p_vlr_frete * lp_quantidade_aux1.
    lp_vlr_sobra_quebra = p_vlr_frete * lp_quantidade_aux2.

    lp_quantidade_aux1 = lp_peso_orig_unidef.
    lp_quantidade_aux2 = lp_peso_conf_default.
    PERFORM yf_converte_vlr_unid_peso USING  cc_unid_peso_default
                                             p_unid_peso
                                   CHANGING: lp_quantidade_aux1,
                                             lp_quantidade_aux2.

    lp_vlr_perda      = ( p_vlr_frete * lp_quantidade_aux2 ) -
                        ( p_vlr_frete * lp_quantidade_aux1 ).
  ENDIF.

* Atribui para tipo de parceiro
  IF p_grupo_conta = cc_add02_03.
*   Pessoa Fisica
    s_vlr_confirmado = lp_vlr_difer_conf - p_vlr_adiantamento
                       - ABS( lp_difer_transp ) - p_vlr_impostos.
  ELSE.
*   Pessoa Jurídica
    IF ABS( lp_quebra_peso ) > 0.
      s_vlr_confirmado = lp_vlr_difer_conf - p_vlr_adiantamento
                       - ABS( lp_difer_transp ).
    ELSE.
      s_vlr_confirmado = lp_vlr_difer_conf - p_vlr_adiantamento.
    ENDIF.
    SUBTRACT p_vlr_seguro FROM s_vlr_confirmado.
  ENDIF.

  s_vlr_diferenca = s_vlr_confirmado - p_vlr_importado.
  IF s_vlr_confirmado < p_vlr_importado.
    s_vlr_programado = s_vlr_confirmado.
  ELSE.
    s_vlr_programado = p_vlr_importado.
  ENDIF.

  s_difer_transp     = lp_difer_transp.
  s_vlr_confirm_ref  = lp_vlr_difer_conf.
  s_fator_conversao  = lp_fator_convers.
  s_vlr_perda        = lp_vlr_perda.
  s_vlr_sobra_quebra = lp_vlr_sobra_quebra.

* Converte valores de unidade default de calculo peso p/unidade de peso
  PERFORM yf_converte_vlr_unid_peso USING cc_unid_peso_default
                                          p_unid_peso
                                CHANGING: lp_difer_peso,
                                          lp_quebra_peso,
                                          lp_quebra_real.
  s_difer_peso   = lp_difer_peso.
  s_quebra_peso  = lp_quebra_peso.
  s_quebra_real  = lp_quebra_real.

ENDFORM.                    " YF_COCKPIT_CALCULO_DELTAS

*&---------------------------------------------------------------------*
*&      Form  LANCTO_ATLZ_ZLEST0020
*&---------------------------------------------------------------------*
FORM lancto_atlz_zlest0020.

  DATA: l_index          TYPE i,
        l_vlr_recusado   TYPE kwert,
        l_vlr_confirmado TYPE kwert,
        l_vlr_importado  TYPE kwert,
        vg_wa_lote       TYPE zles_cockpit_lote,
        vg_wa_lancto     TYPE zlest0013,
        vg_wa_deltas     TYPE zlest0020."zles_cockpit_delta.



  LOOP AT ti_zlest0013 INTO vg_wa_lancto.

    READ TABLE ti_deltas INTO vg_wa_deltas
                             WITH KEY transportador = vg_wa_lancto-codtrp
                                       posto = vg_wa_lancto-codposto
                                        lote = vg_wa_lancto-lote
*                                    ctafrete = vg_wa_lancto-ctafrete
                                      conhecimento = vg_wa_lancto-conhec
                                       chvid = vg_wa_lancto-chvid
                                       erdat = vg_wa_lancto-datalote
*                                    datalote = vg_wa_lancto-datalote
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.
*     Dados de calculo - Deltas
      MOVE-CORRESPONDING vg_wa_deltas TO zlest0020.
      MOVE: vg_wa_lancto-codtrp       TO zlest0020-transportador,
            vg_wa_lancto-codposto     TO zlest0020-posto,
            vg_wa_lancto-conhec       TO zlest0020-conhecimento,
            sy-datum                  TO zlest0020-erdat,
            sy-uname                  TO zlest0020-uname.

      MODIFY zlest0020 FROM zlest0020.
      CLEAR zlest0020.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " LANCTO_ATLZ_ZLEST0020
*&---------------------------------------------------------------------*
*&      Form  YF_CONVERTE_VLR_UNID_PESO
*&---------------------------------------------------------------------*
FORM yf_converte_vlr_unid_peso USING p_unid_orig TYPE gewei
                                     p_unid_dest TYPE gewei
                            CHANGING p_valor     TYPE p.

  DATA: lv_factor      TYPE f,
        lv_value       TYPE f,
        lv_valor       TYPE zquant17_7.

  CHECK: p_unid_orig <> p_unid_dest AND p_valor <> 0.

* Trabalha com casas decimais maiores
  lv_valor = p_valor.

* Obtem fator de conversão
  CALL FUNCTION 'MC_UNIT_CONVERSION'
    EXPORTING
      nach_meins           = p_unid_dest
      von_meins            = p_unid_orig
    IMPORTING
      umref                = lv_factor
    EXCEPTIONS
      conversion_not_found = 1
      material_not_found   = 2
      nach_meins_missing   = 3
      overflow             = 4
      von_meins_missing    = 5
      OTHERS               = 6.

  CHECK: sy-subrc IS INITIAL.

  lv_value = lv_valor * lv_factor.
  CLEAR lv_valor.

* Aplica arredondamento para sete casas decimais
  CALL FUNCTION 'ROUND'
    EXPORTING
      decimals      = 7
      input         = lv_value
      sign          = 'X'
    IMPORTING
      output        = lv_valor
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.

* Devolve o valor para casas decimais informada
  p_valor = lv_valor.

ENDFORM.                    " YF_CONVERTE_VLR_UNID_PESO

*&---------------------------------------------------------------------*
*&      Form  YF_BUSCA_DOCTOS_NF
*&---------------------------------------------------------------------*
FORM yf_busca_doctos_nf .

  RANGES: rg_reftyp FOR j_1bnflin-reftyp OCCURS 0.
  DATA: lw_range      TYPE lxhme_range_c2,
        vl_refkey     TYPE j_1bnflin-refkey.

  DATA: li_index       TYPE i.

  lw_range-sign   = cc_i.
  lw_range-option = cc_eq.
  lw_range-low    = cc_reftyp_md.
  APPEND lw_range TO rg_reftyp.
  lw_range-low    = cc_reftyp_bi.
  APPEND lw_range TO rg_reftyp.

  CHECK: NOT ti_vbfa[] IS INITIAL.

  REFRESH ti_nfbalanca.
  CLEAR:  ti_nfbalanca.

* Documentos de NF de itens
  LOOP AT ti_vbfa.
    IF ti_vbfa-vbtyp_n = 'R'.
      CONCATENATE ti_vbfa-vbeln ti_vbfa-refkey INTO vl_refkey.
      ti_vbfa-refkey = vl_refkey.
    ELSE.
      ti_vbfa-refkey = ti_vbfa-vbeln.
    ENDIF.
    MODIFY ti_vbfa INDEX sy-tabix TRANSPORTING refkey.
  ENDLOOP.

  SELECT DISTINCT refkey docnum meins
    INTO TABLE ti_j_1bnflin
    FROM j_1bnflin
     FOR ALL ENTRIES IN ti_vbfa
   WHERE reftyp IN rg_reftyp
     AND refkey = ti_vbfa-refkey
     AND refitm = ti_vbfa-posnn.

*  SELECT DISTINCT refkey docnum meins
*    INTO TABLE ti_j_1bnflin
*    FROM j_1bnflin
*     FOR ALL ENTRIES IN ti_vbfa
*   WHERE reftyp = cc_reftyp_md
*     AND refkey = ti_vbfa-refkey
*     AND refitm = ti_vbfa-posnn.

* Documentos de NF de cabeçalho
  CHECK: NOT ti_j_1bnflin[] IS INITIAL.

  SELECT docnum nfenum nfnum series bukrs  branch gewei
    INTO TABLE ti_notafiscal
    FROM j_1bnfdoc
     FOR ALL ENTRIES IN ti_j_1bnflin
   WHERE docnum = ti_j_1bnflin-docnum.

  LOOP AT ti_notafiscal.

    li_index = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ti_notafiscal-nfenum
      IMPORTING
        output = ti_notafiscal-nfenum.

    CONCATENATE ti_notafiscal-bukrs  '-'
                ti_notafiscal-branch '-'
                ti_notafiscal-nfenum
                ti_notafiscal-nfnum
           INTO ti_notafiscal-chave.

    MODIFY ti_notafiscal INDEX li_index TRANSPORTING chave.
  ENDLOOP.

* Dados de balança
  CHECK: NOT ti_notafiscal[] IS INITIAL.

  SELECT chave dtachegada pesodvagao
    INTO TABLE ti_balanca
    FROM zlest0019
     FOR ALL ENTRIES IN ti_notafiscal
   WHERE idinter = cc_idinter_l1
     AND tp_movi = cc_e
     AND tp_reg  = cc_tp_reg_30
     AND chave   = ti_notafiscal-chave.

  SORT ti_balanca BY chave.

  LOOP AT ti_notafiscal.
    MOVE-CORRESPONDING ti_notafiscal TO ti_nfbalanca.
    READ TABLE ti_balanca WITH KEY chave = ti_notafiscal-chave
         BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      MOVE: ti_balanca-dtachegada  TO ti_nfbalanca-data_chega,
            ti_balanca-pesodvagao  TO ti_nfbalanca-peso_chega.
    ENDIF.
    APPEND ti_nfbalanca.
    CLEAR  ti_nfbalanca.
  ENDLOOP.

  FREE: ti_notafiscal,
        ti_balanca.

ENDFORM.                    " YF_BUSCA_DOCTOS_NF

*&---------------------------------------------------------------------*
*&      Form  YF_SOMA_PESO_CONFIRMADO_ALL
*&---------------------------------------------------------------------*
FORM yf_soma_peso_confirmado_all  USING p_vbeln
                                        p_unid_conversao
                               CHANGING p_peso_confirm_all
                                        p_vlr_nota_fiscal.
  DATA: li_index      TYPE i,
        lp_peso_chega TYPE brgew_ap.

  SORT ti_vbfa BY vbelv.

* Lê fluxo de documento de faturamento
  READ TABLE ti_vbfa WITH KEY vbelv = ti_remessa-vbeln BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.

* Lê documento de NF para documento de faturamento
  READ TABLE ti_j_1bnflin WITH KEY refkey = ti_vbfa-refkey
                          BINARY SEARCH.
  CHECK: sy-subrc IS INITIAL.
  li_index = sy-tabix.

  SORT ti_nfbalanca BY docnum.

  DO.
*   Lê documento de NF e dados da Balança
    READ TABLE ti_nfbalanca
         WITH KEY docnum = ti_j_1bnflin-docnum BINARY SEARCH.

    IF sy-subrc IS INITIAL AND ti_nfbalanca-peso_chega > 0.

      lp_peso_chega = ti_nfbalanca-peso_chega.

      PERFORM yf_converte_vlr_unid_peso USING ti_nfbalanca-gewei
                                              p_unid_conversao
                                     CHANGING lp_peso_chega.

      p_peso_confirm_all = p_peso_confirm_all +  lp_peso_chega.
    ENDIF.

*   Acumula Valor da nota fiscal
    PERFORM yf_acumula_vlr_nota_fiscal USING ti_j_1bnflin-docnum
                                    CHANGING p_vlr_nota_fiscal.

    ADD 1 TO li_index.
    READ TABLE ti_j_1bnflin INDEX li_index.
    IF sy-subrc <> 0 OR ti_j_1bnflin-refkey <> ti_vbfa-refkey.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.                    " YF_SOMA_PESO_CONFIRMADO_ALL


*&---------------------------------------------------------------------*
*&      Form  YF_ACUMULA_VLR_NOTA_FISCAL
*&---------------------------------------------------------------------*
FORM yf_acumula_vlr_nota_fiscal  USING p_docnum
                              CHANGING p_vlr_nota_fiscal.

  STATICS: lc_docnum           TYPE j_1bdocnum,
           lp_vlr_nota_fiscal  TYPE j_1bnftot.

  IF lc_docnum = p_docnum.
    p_vlr_nota_fiscal = p_vlr_nota_fiscal + lp_vlr_nota_fiscal.
    EXIT.
  ELSE.
    lc_docnum = p_docnum.
    CLEAR lp_vlr_nota_fiscal.
  ENDIF.

  CLEAR w_docheader.

  CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
    EXPORTING
      doc_number         = p_docnum
    IMPORTING
      doc_header         = w_docheader
    TABLES
      doc_partner        = ti_docpartner
      doc_item           = ti_docitem
      doc_item_tax       = ti_docitemtax
      doc_header_msg     = ti_docheadermsg
      doc_refer_msg      = ti_docrefermsg
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CHECK: NOT ti_docitem[] IS INITIAL
    AND  NOT ti_docitemtax[] IS INITIAL.

  CLEAR: w_ext_header.

  CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION'
    EXPORTING
      nf_header   = w_docheader
    IMPORTING
      ext_header  = w_ext_header
    TABLES
      nf_item     = ti_docitem
      nf_item_tax = ti_docitemtax
      ext_item    = ti_ext_item.

  LOOP AT ti_ext_item.
    p_vlr_nota_fiscal = p_vlr_nota_fiscal + ti_ext_item-nftot.
    lp_vlr_nota_fiscal = lp_vlr_nota_fiscal + ti_ext_item-nftot.
  ENDLOOP.

ENDFORM.                    " YF_ACUMULA_VLR_NOTA_FISCAL

*&---------------------------------------------------------------------*
*&      Form  F_CALCULA_DELTA
*&---------------------------------------------------------------------*
FORM f_calcula_delta .
  DATA: lc_subrc TYPE i.
  SORT:
        ti_transporte BY tdlnr exti1 exti2,
        ti_remessa    BY tknum vbeln,
        ti_vfkp       BY rebel fknum fkpos,
        ti_konv       BY knumv kschl,
        ti_j_1bnflin  BY refkey docnum,
        ti_tolerancia BY tplst bukrs.


* Recalculando as quebras/sobras/perdas
  LOOP AT ti_zlest0013.
* Controle chave de identificação de lote
    CLEAR ti_zlest0025.
    READ TABLE ti_zlest0025 WITH KEY chvid = ti_zlest0013-chvid
                            BINARY SEARCH.
*   Dados de Transporte (VTTK + ZLEST0024)
    READ TABLE ti_transporte WITH KEY tdlnr = ti_zlest0013-codtrp
                                      exti1 = ti_zlest0013-conhec
                                      exti2 = ti_zlest0013-ctafrete
                             BINARY SEARCH.
    lc_subrc = sy-subrc.
    IF ti_zlest0025-calcockpit = cc_s.
      IF lc_subrc IS INITIAL.
*       Obtem dados da Remessa
        PERFORM yf_dados_adic_remessa_transp USING ti_zlest0013-vlrconhec.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " F_CALCULA_DELTA

*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DELTA
*&---------------------------------------------------------------------*
FORM f_grava_delta .

  SORT ti_zlest0013 BY codtrp codposto lote.
  DELETE ADJACENT DUPLICATES FROM ti_zlest0013
                  COMPARING codtrp codposto lote.

  LOOP  AT ti_zlest0013.
* Seleciona os documentos importados para recalculo
    DELETE FROM zlest0020
     WHERE transportador = ti_zlest0013-codtrp
       AND posto         = ti_zlest0013-codposto
       AND lote          = ti_zlest0013-lote.
  ENDLOOP.

  MODIFY zlest0020 FROM TABLE ti_deltas.

ENDFORM.                    " F_GRAVA_DELTA
