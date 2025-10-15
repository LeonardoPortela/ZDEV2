* &---------------------------------------------------------------------*
* & Report  ZSDR0102
* &
* &---------------------------------------------------------------------*
* &
* &
* &---------------------------------------------------------------------*
 REPORT zsdr0102.

 TABLES: zsdt0192, zsdt0193, zsdt0194.

 TYPES: BEGIN OF ty_safra,
          vbeln         TYPE vbak-vbeln,
          posnr         TYPE vbap-posnr,
          matnr         TYPE csap_mbom-matnr,
          werks         TYPE csap_mbom-werks,
          kwmeng        TYPE vbap-kwmeng,
          vrkme         TYPE vbap-vrkme,
          bwart         TYPE bwart,
          safra         TYPE zsdt0040-safra,
          kunnr         TYPE kunag,                     "CS2019001220 - Sara Oikawa - Jun/2020
          cultura       TYPE zsded008,
          doc_simulacao TYPE zsdt0090-doc_simulacao,
          matnrv        TYPE zsdt0090-matnrv,
        END OF ty_safra.

 TYPES: BEGIN OF ty_vd_produzido_sf,
          mblnr TYPE  aufm-mblnr,
          vbeln TYPE  vbak-vbeln,
          posnr TYPE  vbap-posnr,
          matnr TYPE  aufm-matnr,
          menge TYPE  zsdt0192-qtd_t,
          meins TYPE  aufm-meins,
          kdauf TYPE  aufk-kdauf,
          bwart TYPE bwart,
          safra TYPE  zsdt0040-safra,
        END OF ty_vd_produzido_sf.

 TYPES: BEGIN OF ty_vd_consumido_sf,
          mblnr TYPE  aufm-mblnr,
          vbeln TYPE  vbak-vbeln,            "Sara Oikawa - Jun/2020
          posnr TYPE  vbap-posnr,
          matnr TYPE  aufm-matnr,
          menge TYPE  zsdt0192-qtd_t,
          meins TYPE  aufm-meins,
          werks TYPE  aufm-werks,
          kdauf TYPE  aufk-kdauf,
          bwart TYPE bwart,
          safra TYPE  zsdt0040-safra,
        END OF ty_vd_consumido_sf.

 TYPES: BEGIN OF ty_tsaldos,
          "VSALDO TYPE AUFM-MENGE,
          vbeln  TYPE  vbak-vbeln,
          posnr  TYPE  vbap-posnr,
          vsaldo TYPE  zsdt0192-qtd_t,
          matnr  TYPE  mara-matnr,
          werks  TYPE  vbap-werks,
          meins  TYPE  aufm-meins,
          safra  TYPE  zsdt0040-safra,
        END OF ty_tsaldos.

 TYPES: BEGIN OF ty_tsaldos_mat,
          vsaldo TYPE zsdt0192-qtd_t,
          matnr  TYPE mara-matnr,
          werks  TYPE vbap-werks,
          safra  TYPE  zsdt0040-safra,
        END OF ty_tsaldos_mat.

 TYPES: BEGIN OF ty_pedido_compra,
          ebeln TYPE ekko-ebeln,
          ebelp TYPE ekpo-ebelp,
          bsart TYPE ekko-bsart,
          aedat TYPE ekko-aedat,
          matnr TYPE ekpo-matnr,
          txz01 TYPE ekpo-txz01,
          mtart TYPE ekpo-mtart,
          bukrs TYPE ekpo-bukrs,
          werks TYPE ekpo-werks,
          reswk TYPE ekko-reswk,
          menge TYPE ekpo-menge,
          meins TYPE ekpo-meins,
          elikz TYPE ekpo-elikz,
        END OF ty_pedido_compra.

 TYPES: BEGIN OF ty_prod_rev,
          matnr          TYPE aufm-matnr,
          safra          TYPE zsdt0040-safra,
          werks          TYPE zsdt0192-werks,
          qtd_entregue   TYPE zsdt0192-qtd_n,
          saldo_entregar TYPE zsdt0192-qtd_n,
          vrkme          TYPE zsdt0192-um_ov,
        END OF ty_prod_rev.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
 TYPES: BEGIN OF ty_vd_ndd_ov,
          vbeln  TYPE  vbak-vbeln,
          posnr  TYPE  vbap-posnr,
          vsaldo TYPE  zsdt0192-qtd_t,
          matnr  TYPE  mara-matnr,
          werks  TYPE  vbap-werks,
          meins  TYPE  aufm-meins,
          safra  TYPE  zsdt0040-safra,
          fator  TYPE  zde_percentual_10_4,
        END OF ty_vd_ndd_ov.

 TYPES: BEGIN OF ty_matnr_ov,
          vbeln TYPE  vbak-vbeln,
          posnr TYPE  vbap-posnr,
          matnr TYPE  aufm-matnr,
          werks TYPE  vbap-werks,
          safra TYPE  zsdt0040-safra,
        END OF ty_matnr_ov.

 TYPES: BEGIN OF ty_prod_rev_ov,
          vbeln          TYPE vbak-vbeln,
          posnr          TYPE vbap-posnr,
          matnr          TYPE aufm-matnr,
          safra          TYPE zsdt0040-safra,
          werks          TYPE zsdt0192-werks,
          qtd_entregue   TYPE zsdt0192-qtd_n,
          saldo_entregar TYPE zsdt0192-qtd_n,
          vrkme          TYPE zsdt0192-um_ov,
        END OF ty_prod_rev_ov.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

 TYPES: BEGIN OF ty_matnr,
          matnr TYPE  aufm-matnr,
          werks TYPE  vbap-werks,
          safra TYPE  zsdt0040-safra,
        END OF ty_matnr.

 TYPES: BEGIN OF ty_zsdt0041_proc,
          doc_simulacao TYPE zsdt0041-doc_simulacao,
          vbeln         TYPE zsdt0041-vbeln,
          safra_apl     TYPE zsdt0041-safra_apl,
          matnr         TYPE zsdt0041-matnr,
          cultura_apl   TYPE zsdt0041-cultura_apl,
        END OF ty_zsdt0041_proc.

 DATA: BEGIN OF tg_zsdt0090_proc OCCURS 0,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
         vbeln         TYPE zsdt0090-vbeln,
         posnn         TYPE zsdt0090-posnn,
         vbelv         TYPE zsdt0090-vbelv,
         posnv         TYPE zsdt0090-posnv,
         estorno       TYPE zsdt0090-estorno,
         matnrv        TYPE zsdt0090-matnrv,
       END OF tg_zsdt0090_proc.


 DATA: it_zsdt0192      TYPE TABLE OF zsdt0192,
       it_zsdt0193      TYPE TABLE OF zsdt0193,
       it_safra         TYPE TABLE OF ty_safra,
       it_safrinha      TYPE TABLE OF ty_safra,
       it_dados         TYPE TABLE OF ty_safra,
       it_dados_aux     TYPE TABLE OF ty_safra,
       it_vd_cons_sf    TYPE TABLE OF ty_vd_consumido_sf,
       it_vd_cons_sfe   TYPE TABLE OF ty_vd_consumido_sf,
       it_saldos        TYPE TABLE OF ty_tsaldos,
       it_saldos_aux    TYPE TABLE OF ty_tsaldos,
       it_saldos_mat    TYPE TABLE OF ty_tsaldos_mat,
       it_vd_ndd        TYPE TABLE OF ty_tsaldos,
       it_aufm          TYPE TABLE OF ty_vd_consumido_sf,
       it_aufm_p        TYPE TABLE OF ty_vd_produzido_sf,
       it_aufm_aux      TYPE TABLE OF ty_vd_consumido_sf,
       it_aufm_p_aux    TYPE TABLE OF ty_vd_produzido_sf,
       it_pedido_compra TYPE TABLE OF ty_pedido_compra,
       it_pdd_cmp_aux   TYPE TABLE OF ty_pedido_compra,
       it_prod_rev      TYPE TABLE OF ty_prod_rev,
       it_matnr         TYPE TABLE OF ty_matnr,
       it_zsdt0090      TYPE TABLE OF zsdt0090,
       tg_zsdt0041_proc TYPE TABLE OF ty_zsdt0041_proc,
       wa_zsdt0090      TYPE zsdt0090,
       wa_zsdt0193      TYPE zsdt0193,
       wa_zsdt0192      TYPE zsdt0192,
       wa_safra         TYPE ty_safra,
       wa_safrinha      TYPE ty_safra,
       wa_vd_cons_sf    TYPE ty_vd_consumido_sf,
       wa_aufm          TYPE ty_vd_consumido_sf,
       wa_aufm_p        TYPE ty_vd_produzido_sf,
       wa_saldos        TYPE ty_tsaldos,
       wa_saldos_aux    TYPE ty_tsaldos,
       wa_saldos_mat    TYPE ty_tsaldos_mat,
       wa_vd_ndd        TYPE ty_tsaldos,
       wa_pedido_compra TYPE ty_pedido_compra,
       wa_pdd_cmp_aux   TYPE ty_pedido_compra,
       wa_prod_rev      TYPE ty_prod_rev,
       wa_matnr         TYPE ty_matnr.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
 DATA: it_vd_cons_sf_ov TYPE TABLE OF ty_vd_consumido_sf,

       it_vd_ndd_ov     TYPE TABLE OF ty_vd_ndd_ov,
       wa_vd_ndd_ov     TYPE ty_vd_ndd_ov,

       it_zsdt0256      TYPE TABLE OF zsdt0256,
       wa_zsdt0256      TYPE zsdt0256,

       it_matnr_ov      TYPE TABLE OF ty_matnr_ov,
       wa_matnr_ov      TYPE ty_matnr_ov,

       it_prod_rev_ov   TYPE TABLE OF ty_prod_rev_ov,
       wa_prod_rev_ov   TYPE ty_prod_rev_ov,

       tot_devol_ov     TYPE vbap-kwmeng.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020


 DATA: it_stpo        TYPE TABLE OF stpo_api02,
       wa_stpo        TYPE stpo_api02,
       it_stko        TYPE TABLE OF stko_api02,
       wa_stko        TYPE stko_api02,
       ck_fl_warning  LIKE capiflag-flwarning,
       lc_bom_usage   LIKE  csap_mbom-stlan,initialization.


 DATA: vtotal    TYPE aufm-menge,
       vsaldo    TYPE aufm-menge,
       bom_usage TYPE csap_mbom-stlan,
       datuv     TYPE csap_mbom-datuv,
       vmatnr    LIKE mara-matnr,
       qtd1      TYPE p DECIMALS 5,
       qtd2      TYPE p DECIMALS 5,
       tot_devol TYPE vbap-kwmeng.


 TYPES: BEGIN OF ty_total,
          vbeln  TYPE  vbak-vbeln,
          posnr  TYPE  vbap-posnr,
          matnr  TYPE zsdt0192-matnr,
          vtotal TYPE zsdt0192-qtd_t,
          meins  TYPE aufm-meins,
          safra  TYPE  zsdt0040-safra,
        END OF ty_total.

 DATA: it_total TYPE TABLE OF ty_total,
       t_total  TYPE TABLE OF ty_total,
       wa_total TYPE ty_total.


 DATA: it_values LIKE rgsb4  OCCURS 0  WITH HEADER LINE.

 DATA: it_werks      TYPE RANGE OF vbap-werks,
       wa_werks      LIKE LINE OF it_werks,
       it_auart      TYPE RANGE OF vbak-auart,
       wa_auart      LIKE LINE OF it_auart,
       r_safra       TYPE RANGE OF zsdt0040-safra,
       wr_safra      LIKE LINE OF r_safra,
       it_cultura    TYPE RANGE OF zsdt0040-cultura,
       wa_cultura    LIKE LINE OF it_cultura,
       it_cultura_sf TYPE RANGE OF zsdt0040-cultura,
       wa_cultura_sf LIKE LINE OF it_cultura,
       it_bsart      TYPE RANGE OF ekko-bsart,
       wa_bsart      LIKE LINE OF it_bsart,
       vaedat        LIKE ekko-aedat.


**********************************************************************
* PSA CONVERT MATNR 18
 DATA bgv_matcri18    TYPE matnr18.


 START-OF-SELECTION.


   PERFORM check_pedidos_mod. "Verificar pedidos modificados e fazer alteração na tabela ZSDT0193.
   PERFORM z_busca_set.
   PERFORM z_busca_dados_ov. "Ordem de venda
   PERFORM z_busca_dados_pr. "Produto Revenda
   PERFORM z_busca_dados_pc. "Pedido de compra


 END-OF-SELECTION.


 FORM z_busca_set.

   REFRESH: it_values, it_werks.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_01'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wa_werks-sign   = 'I'.
     wa_werks-option = 'EQ'.
     wa_werks-low    = it_values-from.
     APPEND wa_werks TO it_werks.
     CLEAR  wa_werks.
   ENDLOOP.



   REFRESH: it_values, it_auart.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_02'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wa_auart-sign   = 'I'.
     wa_auart-option = 'EQ'.
     wa_auart-low    = it_values-from.
     APPEND wa_auart TO it_auart.
     CLEAR  wa_auart.
   ENDLOOP.

   REFRESH: it_values,r_safra.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_03'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wr_safra-sign   = 'I'.
     wr_safra-option = 'EQ'.
     wr_safra-low    = it_values-from.
     APPEND wr_safra TO r_safra.
     CLEAR  wr_safra.
   ENDLOOP.

   REFRESH: it_values, it_cultura.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_04'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wa_cultura-sign   = 'I'.
     wa_cultura-option = 'EQ'.
     wa_cultura-low    = it_values-from.
     APPEND wa_cultura TO it_cultura.
     CLEAR  wa_cultura.
   ENDLOOP.

   REFRESH: it_values, it_cultura_sf.

   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_05'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wa_cultura_sf-sign   = 'I'.
     wa_cultura_sf-option = 'EQ'.
     wa_cultura_sf-low    = it_values-from.
     APPEND wa_cultura_sf TO it_cultura_sf.
     CLEAR  wa_cultura_sf.
   ENDLOOP.


   REFRESH: it_values, it_bsart.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_06'
       class      = '0000'
     TABLES
       set_values = it_values.

   LOOP AT it_values.
     wa_bsart-sign   = 'I'.
     wa_bsart-option = 'EQ'.
     wa_bsart-low    = it_values-from.
     APPEND wa_bsart TO it_bsart.
     CLEAR  wa_bsart.
   ENDLOOP.


   REFRESH it_values.
   CLEAR vaedat.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       setnr      = 'ZSDR0102_07'
       class      = '0000'
     TABLES
       set_values = it_values.

   READ TABLE it_values INDEX 1.
   vaedat = it_values-from.

 ENDFORM.


 FORM z_busca_dados_ov.
   FREE: it_dados_aux[].

   DATA: cont           TYPE p,
         ws_zsdt0041    TYPE ty_zsdt0041_proc,
         lv_key_sim_old TYPE char16.



   "SAFRA
   SELECT      b~vbeln,
   a~posnr,
   a~matnr,
   a~werks,
   a~kwmeng,
   a~vrkme,
   d~safra_apl AS safra,
   b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
   d~cultura_apl AS cultura
     FROM vbap AS  a
     INNER JOIN vbak AS b ON b~vbeln = a~vbeln
     INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
     INNER JOIN zsdt0041 AS d ON d~vbeln = f~vbeln AND d~matnr = a~matnr
     INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
     INNER JOIN mara  AS  c ON c~matnr = a~matnr
     APPENDING CORRESPONDING FIELDS OF TABLE @it_dados
                 WHERE a~werks   IN @it_werks
                 AND   b~spart   EQ '02'
                 AND   b~auart   NOT IN @it_auart
                 AND   c~mtart   EQ 'ZFER'
                 AND   d~safra_apl   IN @r_safra "USER STORY 106316 / AOENNING
                 AND   d~cultura_apl IN @it_cultura
                 AND   f~lifsp   NE '12'
                 GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  d~safra_apl,  b~kunnr, d~cultura_apl.

   FREE: it_dados_aux[].
   SELECT  b~vbeln,
           a~posnr,
           a~matnr,
           a~werks,
           a~kwmeng,
           a~vrkme,
*           e~safra,
           b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
*           e~cultura,
           d~doc_simulacao,
           d~matnrv
     FROM vbap AS  a
     INNER JOIN vbak AS b ON b~vbeln = a~vbeln
     INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
     INNER JOIN zsdt0090 AS d ON d~vbeln = f~vbeln
     INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
     INNER JOIN mara  AS  c ON c~matnr = a~matnr
     APPENDING CORRESPONDING FIELDS OF TABLE @it_dados_aux
   WHERE a~werks   IN @it_werks
   AND   b~spart   EQ '02'
   AND   b~auart   NOT IN @it_auart
   AND   c~mtart   EQ 'ZFER'
*   AND   e~safra   IN @r_safra
*   AND   e~cultura IN @it_cultura
   AND   f~lifsp   NE '12'
   GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  e~safra,  b~kunnr, e~cultura, d~doc_simulacao, d~matnrv.

   SORT it_dados BY vbeln posnr.
   DELETE ADJACENT DUPLICATES FROM it_dados COMPARING vbeln posnr.

   DATA(lit_dados_sim) = it_dados_aux[].
   SORT lit_dados_sim BY doc_simulacao.
   DELETE ADJACENT DUPLICATES FROM lit_dados_sim COMPARING doc_simulacao.

   IF lit_dados_sim[] IS NOT INITIAL.
     SELECT  doc_simulacao, vbeln ,posnn, vbelv, posnv, estorno, matnrv
        FROM zsdt0090 INTO TABLE @tg_zsdt0090_proc
        FOR ALL ENTRIES IN @lit_dados_sim
      WHERE doc_simulacao EQ @lit_dados_sim-doc_simulacao.

     IF tg_zsdt0090_proc[] IS NOT INITIAL.
       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 INTO TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbelv.

       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 APPENDING TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbeln.

       IF tg_zsdt0041_proc[] IS NOT INITIAL.

         SELECT doc_simulacao , safra
          FROM zsdt0040 INTO TABLE @DATA(tg_zsdt0040_proc)
          FOR ALL ENTRIES IN @tg_zsdt0041_proc
        WHERE doc_simulacao EQ @tg_zsdt0041_proc-doc_simulacao.

       ENDIF.

       SORT tg_zsdt0041_proc.
       DELETE ADJACENT DUPLICATES FROM tg_zsdt0041_proc COMPARING ALL FIELDS.

     ENDIF.
   ENDIF.

   "Verifica se safra foi preenchida, filtrar por safra / USER STORY 106316 / AOENNING.
   IF r_safra[] IS NOT INITIAL.

     DELETE tg_zsdt0090_proc WHERE estorno EQ abap_true.
     DELETE tg_zsdt0090_proc WHERE vbeln   EQ space AND posnn EQ space.
     DELETE tg_zsdt0090_proc WHERE vbelv   EQ space OR posnv EQ space.

     SORT: tg_zsdt0090_proc BY vbeln posnn,
           tg_zsdt0041_proc BY vbeln,
           tg_zsdt0040_proc BY doc_simulacao,
           it_dados_aux[]  BY vbeln posnr.

     LOOP AT it_dados_aux ASSIGNING FIELD-SYMBOL(<ls_dados>).
       lv_key_sim_old = <ls_dados>-vbeln && <ls_dados>-posnr.
       DATA(l_count) = 0.
       WHILE lv_key_sim_old IS NOT INITIAL.
         ADD 1 TO l_count.
         DATA(_key_simulador_busca) = lv_key_sim_old.
         PERFORM f_get_safra_zsdt0090 USING _key_simulador_busca <ls_dados>-matnrv
                                   CHANGING lv_key_sim_old
                                            ws_zsdt0041.
         IF ws_zsdt0041-vbeln IS NOT INITIAL.
           <ls_dados>-safra   = ws_zsdt0041-safra_apl.
           <ls_dados>-cultura = ws_zsdt0041-cultura_apl.

           IF <ls_dados>-safra EQ 0.
             READ TABLE tg_zsdt0040_proc ASSIGNING FIELD-SYMBOL(<fs_zsdt0040_proc>) WITH KEY  doc_simulacao = ws_zsdt0041-doc_simulacao BINARY SEARCH.
             IF sy-subrc EQ 0.
               <ls_dados>-safra = <fs_zsdt0040_proc>-safra.
             ENDIF.
           ENDIF.

           EXIT.
         ENDIF.
       ENDWHILE.
     ENDLOOP.

     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY safra.
       DELETE it_dados_aux[] WHERE safra NOT IN r_safra OR cultura NOT IN it_cultura.

       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados.
     ENDIF.
   ELSE.

     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados.
     ENDIF.

   ENDIF.

   IF it_dados[] IS NOT INITIAL.
**PQ
     SORT it_dados[] BY vbeln posnr.
     DELETE ADJACENT DUPLICATES FROM it_dados[] COMPARING vbeln posnr.
**PQ
     MOVE it_dados TO it_safra.
   ENDIF.

   PERFORM z_safra_ov.

   REFRESH it_dados[].

   "SAFRINHA
   SELECT   b~vbeln,
            a~posnr,
            a~matnr,
            a~werks,
            a~kwmeng,
            a~vrkme,
            d~safra_apl AS safra ,
            b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
            d~cultura_apl AS cultura
      FROM vbap AS  a
      INNER JOIN vbak AS b ON b~vbeln = a~vbeln
      INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
      INNER JOIN zsdt0041 AS d ON d~vbeln = f~vbeln AND d~matnr = a~matnr
      INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
      INNER JOIN mara  AS  c ON c~matnr = a~matnr
    APPENDING CORRESPONDING FIELDS OF TABLE @it_dados
    WHERE a~werks   IN @it_werks
    AND   b~spart   EQ '02'
    AND   b~auart   NOT IN @it_auart
    AND   c~mtart   EQ 'ZFER'
    AND   d~safra_apl   IN @r_safra
    AND   d~cultura_apl IN @it_cultura_sf
    AND   f~lifsp   NE '12'
    GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme, d~safra_apl, b~kunnr, d~cultura_apl.


   "Verifica se safra foi preenchida, filtrar por safra / USER STORY 106316 / AOENNING.
   FREE: it_dados_aux[].
   SELECT   b~vbeln,
            a~posnr,
            a~matnr,
            a~werks,
            a~kwmeng,
            a~vrkme,
*            e~safra,
            b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
*            e~cultura,
            d~doc_simulacao,
            d~matnrv
      FROM vbap AS  a
      INNER JOIN vbak AS b ON b~vbeln = a~vbeln
      INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
      INNER JOIN zsdt0090 AS d ON d~vbeln = f~vbeln
      INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
      INNER JOIN mara  AS  c ON c~matnr = a~matnr
     APPENDING CORRESPONDING FIELDS OF TABLE @it_dados_aux
    WHERE a~werks   IN @it_werks
    AND   b~spart   EQ '02'
    AND   b~auart   NOT IN @it_auart
    AND   c~mtart   EQ 'ZFER'
*    AND   e~safra   IN @r_safra
*    AND   e~cultura IN @it_cultura_sf
    AND   f~lifsp   NE '12'
    GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme, e~safra,  b~kunnr, e~cultura, d~doc_simulacao, d~matnrv.

   SORT it_dados BY vbeln posnr.
   DELETE ADJACENT DUPLICATES FROM it_dados COMPARING vbeln posnr.


   FREE: lit_dados_sim.
   lit_dados_sim = it_dados_aux[].
   SORT lit_dados_sim BY doc_simulacao.
   DELETE ADJACENT DUPLICATES FROM lit_dados_sim COMPARING doc_simulacao.

   IF lit_dados_sim[] IS NOT INITIAL.

     FREE: tg_zsdt0090_proc.
     SELECT  doc_simulacao, vbeln ,posnn, vbelv, posnv, estorno, matnrv
        FROM zsdt0090 INTO TABLE @tg_zsdt0090_proc
        FOR ALL ENTRIES IN @lit_dados_sim
      WHERE doc_simulacao EQ @lit_dados_sim-doc_simulacao.

     IF tg_zsdt0090_proc[] IS NOT INITIAL.

       FREE: tg_zsdt0041_proc.
       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 INTO TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbelv.

       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 APPENDING TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbeln.

       IF tg_zsdt0041_proc[] IS NOT INITIAL.

         FREE: tg_zsdt0040_proc.
         SELECT doc_simulacao , safra
          FROM zsdt0040 INTO TABLE @tg_zsdt0040_proc
          FOR ALL ENTRIES IN @tg_zsdt0041_proc
        WHERE doc_simulacao EQ @tg_zsdt0041_proc-doc_simulacao.

       ENDIF.

       SORT tg_zsdt0041_proc.
       DELETE ADJACENT DUPLICATES FROM tg_zsdt0041_proc COMPARING ALL FIELDS.

     ENDIF.
   ENDIF.

   IF r_safra[] IS NOT INITIAL.

     DELETE tg_zsdt0090_proc WHERE estorno EQ abap_true.
     DELETE tg_zsdt0090_proc WHERE vbeln   EQ space AND posnn EQ space.
     DELETE tg_zsdt0090_proc WHERE vbelv   EQ space OR posnv EQ space.

     SORT: tg_zsdt0090_proc BY vbeln posnn matnrv,
           tg_zsdt0041_proc BY vbeln,
           tg_zsdt0040_proc BY doc_simulacao,
           it_dados_aux[]   BY vbeln posnr.

     CLEAR: l_count, _key_simulador_busca.
     LOOP AT it_dados_aux ASSIGNING <ls_dados>.
       lv_key_sim_old = <ls_dados>-vbeln && <ls_dados>-posnr.
       l_count = 0.
       WHILE lv_key_sim_old IS NOT INITIAL.
         ADD 1 TO l_count.
         _key_simulador_busca = lv_key_sim_old.
         PERFORM f_get_safra_zsdt0090 USING _key_simulador_busca <ls_dados>-matnrv
                                   CHANGING lv_key_sim_old
                                            ws_zsdt0041.
         IF ws_zsdt0041-vbeln IS NOT INITIAL.
           <ls_dados>-safra   = ws_zsdt0041-safra_apl.
           <ls_dados>-cultura = ws_zsdt0041-cultura_apl.

           IF <ls_dados>-safra EQ 0.
             READ TABLE tg_zsdt0040_proc ASSIGNING <fs_zsdt0040_proc> WITH KEY  doc_simulacao = ws_zsdt0041-doc_simulacao BINARY SEARCH.
             IF sy-subrc EQ 0.
               <ls_dados>-safra = <fs_zsdt0040_proc>-safra.
             ENDIF.
           ENDIF.
           EXIT.
         ENDIF.
       ENDWHILE.
     ENDLOOP.

     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY safra.
       DELETE it_dados_aux[] WHERE safra   NOT IN r_safra.
       DELETE it_dados_aux[] WHERE cultura NOT IN it_cultura_sf.


       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados.
     ENDIF.
   ELSE.
     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados.
     ENDIF.
   ENDIF.
   IF it_dados[] IS NOT INITIAL.
**PQ
     SORT it_dados[] BY vbeln posnr.
     DELETE ADJACENT DUPLICATES FROM it_dados[] COMPARING vbeln posnr.
**PQ
     MOVE it_dados TO it_safrinha.
   ENDIF.


   PERFORM z_safrinha_ov.

   REFRESH: it_safra, it_safrinha, it_dados.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SAFRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM z_safra_ov.

   REFRESH it_zsdt0192.

   CHECK it_safra IS NOT INITIAL.

   SORT it_safra BY vbeln.

   "BUSCA A QUANTIDADE DE CADA MATERIA-PRIMA DO QUE JÁ FOI PRODUZIDO DE CADA OV.
   SELECT  g~mblnr
           h~kdauf
           h~kdpos
           g~matnr
           g~menge
           g~meins
           g~werks
           h~kdauf
           g~bwart
       FROM aufm AS g
       INNER JOIN aufk AS h ON h~aufnr = g~aufnr
       INNER JOIN jest AS i ON i~objnr = h~objnr
     INTO TABLE it_aufm
     FOR ALL ENTRIES IN it_safra
    WHERE g~bwart IN ( '261', '262' )
     AND  h~kdauf EQ it_safra-vbeln
     AND  h~kdpos EQ it_safra-posnr
     AND  i~stat  IN ( 'I0009', 'I0010' )
     AND  i~inact NE 'X'.

   SORT it_aufm BY kdauf.

   LOOP AT it_aufm ASSIGNING FIELD-SYMBOL(<wa_aufm>).
     READ TABLE it_safra INTO wa_safra WITH KEY vbeln = <wa_aufm>-kdauf.
     <wa_aufm>-safra = wa_safra-safra.
   ENDLOOP.

   MOVE-CORRESPONDING it_aufm TO it_aufm_aux.

   SORT it_aufm_aux BY matnr werks safra.
   DELETE ADJACENT DUPLICATES FROM it_aufm_aux COMPARING matnr werks safra.

   "TOTALIZA A QUANTIDADE POR MATERIA-PRIMA E CENTRO.
   LOOP AT it_aufm_aux INTO DATA(wa_aufm_aux).
     LOOP AT it_aufm INTO wa_aufm
        WHERE matnr = wa_aufm_aux-matnr
          AND werks = wa_aufm_aux-werks
       	  AND safra = wa_aufm_aux-safra.

       IF wa_aufm-bwart EQ '261'.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf.
         CLEAR wa_aufm.
       ENDIF.

       IF wa_aufm-bwart EQ '262'.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sfe.
         CLEAR wa_aufm.
       ENDIF.
     ENDLOOP.
     CLEAR wa_aufm_aux.
   ENDLOOP.

**==============================================================================
****  Retirar a quantidade estornada do total movimento 261
   LOOP AT it_vd_cons_sf ASSIGNING FIELD-SYMBOL(<w_vd_cons_sf>). "Total de movimento 261
     LOOP AT it_vd_cons_sfe ASSIGNING FIELD-SYMBOL(<w_vd_cons_sfe>) "Total de movimento 262
       WHERE matnr EQ <w_vd_cons_sf>-matnr
         AND werks EQ <w_vd_cons_sf>-werks
         AND safra EQ <w_vd_cons_sf>-safra.

       <w_vd_cons_sf>-menge = <w_vd_cons_sf>-menge - <w_vd_cons_sfe>-menge.
     ENDLOOP.
   ENDLOOP.
**==============================================================================

* Início - CS2019001220 - Sara Oikawa - Jun/2020
*** Armazena Totais Por OV

   REFRESH: it_aufm_aux, it_vd_cons_sf_ov.

   it_aufm_aux[] = it_aufm[].

   SORT it_aufm_aux BY  vbeln posnr matnr werks safra.
   DELETE ADJACENT DUPLICATES FROM it_aufm_aux COMPARING vbeln posnr matnr werks safra.

   "TOTALIZA A QUANTIDADE POR OV MATERIA-PRIMA E CENTRO.
   LOOP AT it_aufm_aux INTO wa_aufm_aux.
     LOOP AT it_aufm INTO wa_aufm
        WHERE vbeln = wa_aufm_aux-vbeln
          AND posnr = wa_aufm_aux-posnr
          AND matnr = wa_aufm_aux-matnr
          AND werks = wa_aufm_aux-werks
       	  AND safra = wa_aufm_aux-safra.

       IF wa_aufm-bwart EQ '261'.
         wa_vd_cons_sf-vbeln   =  wa_aufm-vbeln.
         wa_vd_cons_sf-posnr   =  wa_aufm-posnr.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf_ov.
       ENDIF.

       IF wa_aufm-bwart EQ '262'.
         wa_vd_cons_sf-vbeln   =  wa_aufm-vbeln.
         wa_vd_cons_sf-posnr   =  wa_aufm-posnr.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge * -1.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf_ov.
       ENDIF.

       CLEAR wa_vd_cons_sf.
     ENDLOOP.

     CLEAR wa_aufm_aux.
   ENDLOOP.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020


   CLEAR: wa_safra.
   REFRESH: it_aufm, it_aufm_aux.

   "BUSCA TODOS OS REGISTROS DE PRODUÇÃO DE 'PRODUTO ACABADO' POR OV.
   SELECT  j~mblnr
           k~kdauf
           k~kdpos
           j~matnr
           j~menge
           j~meins
           k~kdauf
           j~bwart
   FROM aufm AS j
   INNER JOIN aufk AS k ON k~aufnr = j~aufnr
   INNER JOIN jest AS l ON l~objnr = k~objnr
   INTO TABLE it_aufm_p
     FOR ALL ENTRIES IN it_safra
  WHERE j~bwart IN ( '131', '132' )
   AND  k~kdauf EQ it_safra-vbeln
   AND  k~kdpos EQ it_safra-posnr
   AND  l~stat  IN ( 'I0009', 'I0010' )
   AND  l~inact NE 'X'.

   SORT it_aufm_p BY kdauf.

   LOOP AT it_aufm_p ASSIGNING FIELD-SYMBOL(<wa_aufm_p>).
     READ TABLE it_safra INTO wa_safra WITH KEY vbeln = <wa_aufm_p>-kdauf.
     <wa_aufm_p>-safra = wa_safra-safra.
   ENDLOOP.

   SORT it_aufm_p BY vbeln posnr matnr.

   MOVE-CORRESPONDING it_aufm_p TO it_aufm_p_aux.
*   SORT IT_AUFM_AUX BY MATNR.
   DELETE ADJACENT DUPLICATES FROM it_aufm_p_aux COMPARING vbeln posnr matnr.

   "TOTALIZA O QUE JÁ FOI PROZUDO POR ORDEM DE VENDA / ITEM / MATERIAL (PRODUTO ACABADO).
   LOOP AT it_aufm_p_aux INTO DATA(wa_aufm_p_aux).
     LOOP AT it_aufm_p INTO wa_aufm_p
       WHERE vbeln = wa_aufm_p_aux-vbeln
         AND posnr = wa_aufm_p_aux-posnr
         AND matnr = wa_aufm_p_aux-matnr.

       IF wa_aufm_p-bwart EQ '131'.
         wa_total-vbeln = wa_aufm_p-vbeln.
         wa_total-posnr = wa_aufm_p-posnr.
         wa_total-matnr = wa_aufm_p-matnr.
         wa_total-meins = wa_aufm_p-meins.
         wa_total-vtotal = wa_aufm_p-menge.
         wa_total-safra  = wa_aufm_p-safra.
         COLLECT wa_total INTO it_total.
         CLEAR: wa_aufm_p, wa_total.
       ENDIF.

       IF wa_aufm_p-bwart EQ '132'.
         wa_total-vbeln = wa_aufm_p-vbeln.
         wa_total-posnr = wa_aufm_p-posnr.
         wa_total-matnr = wa_aufm_p-matnr.
         wa_total-meins = wa_aufm_p-meins.
         wa_total-vtotal = wa_aufm_p-menge.
         wa_total-safra  = wa_aufm_p-safra.
         COLLECT wa_total INTO t_total.
         CLEAR: wa_aufm_p, wa_total.
       ENDIF.
     ENDLOOP.
     CLEAR wa_aufm_p_aux.
   ENDLOOP.

**====================================================================================================
   "Retirar a quantidade movimento '131' da quantidade total movimento '132'.
   LOOP AT it_total ASSIGNING FIELD-SYMBOL(<w_total>). "Total materiais movimento '131'
     LOOP AT t_total ASSIGNING FIELD-SYMBOL(<l_total>) "Total materiais movimento '132'
       WHERE vbeln EQ <w_total>-vbeln
       AND posnr  EQ <w_total>-posnr
       AND matnr EQ <w_total>-matnr
       AND safra EQ <w_total>-safra.

       <w_total>-vtotal = <w_total>-vtotal - <l_total>-vtotal. "Retirando quantidade movimento '132'
     ENDLOOP.
   ENDLOOP.
**====================================================================================================

   " FAZ O CÁLCULO PARA IDENTIFICAR O QUE FALTA A SER PRODUZIDO POR OV / ITEM / MATERIAL
   LOOP AT it_safra INTO wa_safra.
     wa_saldos-vbeln  =  wa_safra-vbeln.
     wa_saldos-posnr  =  wa_safra-posnr.
     wa_saldos-matnr  =  wa_safra-matnr.
     wa_saldos-werks  =  wa_safra-werks.
     wa_saldos-safra  =  wa_safra-safra.

     READ TABLE it_total INTO wa_total WITH KEY vbeln = wa_safra-vbeln
                                                posnr = wa_safra-posnr
                                                matnr = wa_safra-matnr.

     IF sy-subrc EQ 0.
       IF wa_safra-vrkme EQ 'TO'.
         wa_safra-kwmeng = wa_safra-kwmeng / 1000.
       ENDIF.

       IF wa_total-meins EQ 'TO'.
         wa_total-vtotal = wa_total-vtotal / 1000.
       ENDIF.

       wa_saldos-vsaldo = wa_safra-kwmeng - wa_total-vtotal.

       IF wa_total-meins EQ 'UN'.
         wa_saldos-meins  =  wa_total-meins.
       ELSE.
         wa_saldos-meins  =  'KG'.
       ENDIF.
     ELSE.
       wa_saldos-vsaldo = wa_safra-kwmeng.
       wa_saldos-meins  = wa_safra-vrkme.
     ENDIF.

     COLLECT wa_saldos INTO it_saldos.
     CLEAR: wa_total, wa_safra, tot_devol.
   ENDLOOP.

   DELETE it_saldos WHERE vsaldo LE 0.

   SORT it_saldos BY matnr werks safra.

   MOVE-CORRESPONDING it_saldos TO it_saldos_aux.

   DELETE ADJACENT DUPLICATES FROM it_saldos_aux COMPARING matnr werks safra.

   "AQUI DEVERÁ FAZER O SALDO POR MATERIAL

   LOOP AT it_saldos_aux INTO wa_saldos_aux.
     LOOP AT it_saldos INTO wa_saldos
       WHERE matnr = wa_saldos_aux-matnr
         AND werks = wa_saldos_aux-werks
         AND safra = wa_saldos_aux-safra.

       wa_saldos_mat-matnr  =  wa_saldos-matnr.
       wa_saldos_mat-werks  =  wa_saldos-werks.
       wa_saldos_mat-vsaldo =  wa_saldos-vsaldo.
       wa_saldos_mat-safra  =  wa_saldos-safra.

       COLLECT wa_saldos_mat INTO it_saldos_mat.
       CLEAR: wa_saldos, wa_saldos_mat.
     ENDLOOP.
     CLEAR: wa_saldos_aux.
   ENDLOOP.

   SORT it_saldos_mat BY matnr werks safra.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
* Saldo a produzir por OV/Material

*   LOOP AT IT_SALDOS_MAT INTO WA_SALDOS_MAT.
*
*     BOM_USAGE  = '1'.
*     CONCATENATE  SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO DATUV.
*
*     CALL FUNCTION 'CSAP_MAT_BOM_READ'
*       EXPORTING
*         MATERIAL   = WA_SALDOS_MAT-MATNR
*         PLANT      = WA_SALDOS_MAT-WERKS
*         BOM_USAGE  = BOM_USAGE
*         VALID_FROM = DATUV
*       IMPORTING
*         FL_WARNING = CK_FL_WARNING
*       TABLES
*         T_STPO     = IT_STPO
*         T_STKO     = IT_STKO
*       EXCEPTIONS
*         ERROR      = 1
*         OTHERS     = 2.
*
*     LOOP AT IT_STPO INTO WA_STPO.
*       READ TABLE IT_STKO INTO WA_STKO INDEX 1.
*
*       REPLACE ALL OCCURRENCES OF '.' IN  WA_STKO-BASE_QUAN WITH ''.
*       REPLACE ALL OCCURRENCES OF '.' IN  WA_STPO-COMP_QTY WITH ''.
*
*       QTD1 = CONV #( WA_STKO-BASE_QUAN ).
*       VMATNR = |{ WA_STPO-COMPONENT ALPHA = IN }|.
*       QTD2 = CONV #( WA_STPO-COMP_QTY ).
*
*       VSALDO           = ( QTD2 / QTD1 ).
*       WA_VD_NDD-VSALDO = ( VSALDO * WA_SALDOS_MAT-VSALDO ).
*
*       WA_VD_NDD-MATNR  =  VMATNR.
*       WA_VD_NDD-WERKS  =  WA_SALDOS_MAT-WERKS.
*       WA_VD_NDD-MEINS  =  WA_STPO-COMP_UNIT.
*       WA_VD_NDD-SAFRA  =  WA_SALDOS_MAT-SAFRA.
*
*       COLLECT WA_VD_NDD INTO  IT_VD_NDD.
*       CLEAR: WA_STPO, VSALDO, WA_VD_NDD, WA_STKO.
*     ENDLOOP.
*     CLEAR: WA_SALDOS_MAT, QTD1, QTD2.
*   ENDLOOP.
   LOOP AT it_saldos INTO wa_saldos.

     bom_usage  = '1'.
     CONCATENATE  sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO datuv.

     CALL FUNCTION 'CSAP_MAT_BOM_READ'
       EXPORTING
         material   = wa_saldos-matnr
         plant      = wa_saldos-werks
         bom_usage  = bom_usage
         valid_from = datuv
       IMPORTING
         fl_warning = ck_fl_warning
       TABLES
         t_stpo     = it_stpo
         t_stko     = it_stko
       EXCEPTIONS
         error      = 1
         OTHERS     = 2.

     LOOP AT it_stpo INTO wa_stpo.
       READ TABLE it_stko INTO wa_stko INDEX 1.

       REPLACE ALL OCCURRENCES OF '.' IN  wa_stko-base_quan WITH ''.
       REPLACE ALL OCCURRENCES OF '.' IN  wa_stpo-comp_qty WITH ''.
       REPLACE ALL OCCURRENCES OF ',' IN  wa_stko-base_quan WITH '.'.   "<< RIM-SKM-IR127070-13.02.23
       REPLACE ALL OCCURRENCES OF ',' IN  wa_stpo-comp_qty WITH '.'.    "<< RIM-SKM-IR127070-13.02.23

       qtd1 = CONV #( wa_stko-base_quan ).


**********************************************************************
* PSA CONVERT MATNR 18
       "vmatnr = |{ wa_stpo-component ALPHA = IN }|.

       DATA gv_matcri18_08    TYPE matnr18.

*** Formata o código do material
       CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
           input  = wa_stpo-component
         IMPORTING
           output = gv_matcri18_08.

       CLEAR: vmatnr.

       vmatnr = gv_matcri18_08 .

       CLEAR: gv_matcri18_08.

* END CONVERT
**********************************************************************


       qtd2 = CONV #( wa_stpo-comp_qty ).

       vsaldo           = ( qtd2 / qtd1 ).
       wa_vd_ndd-vsaldo = ( vsaldo * wa_saldos-vsaldo ).

       wa_vd_ndd-matnr  =  vmatnr.
       wa_vd_ndd-werks  =  wa_saldos-werks.
       wa_vd_ndd-meins  =  wa_stpo-comp_unit.
       wa_vd_ndd-safra  =  wa_saldos-safra.

       COLLECT wa_vd_ndd INTO  it_vd_ndd.

       MOVE-CORRESPONDING wa_vd_ndd TO wa_vd_ndd_ov.
       wa_vd_ndd_ov-vbeln  =  wa_saldos-vbeln.
       wa_vd_ndd_ov-posnr  =  wa_saldos-posnr.
       wa_vd_ndd_ov-fator  =  vsaldo.

       COLLECT wa_vd_ndd_ov INTO it_vd_ndd_ov.

       CLEAR: wa_stpo, vsaldo, wa_vd_ndd, wa_vd_ndd_ov, wa_stko.


     ENDLOOP.

     CLEAR: wa_zsdt0256, wa_safra, wa_saldos, qtd1, qtd2.

   ENDLOOP.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020

   SORT it_vd_ndd BY matnr werks.
   SORT it_vd_cons_sf BY matnr werks.

   LOOP AT it_vd_cons_sf INTO wa_vd_cons_sf.


**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr-matnr  = |{ wa_vd_cons_sf-matnr ALPHA = IN }|.

     DATA gv_matcri18_09    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_cons_sf-matnr
       IMPORTING
         output = gv_matcri18_09.

     CLEAR: wa_matnr-matnr .

     wa_matnr-matnr  = gv_matcri18_09.

     CLEAR: gv_matcri18_09.

* END CONVERT
**********************************************************************

     wa_matnr-werks  = wa_vd_cons_sf-werks.
     wa_matnr-safra  = wa_vd_cons_sf-safra.
     COLLECT  wa_matnr INTO it_matnr.
     CLEAR wa_vd_cons_sf.
   ENDLOOP.

   LOOP AT it_vd_ndd INTO wa_vd_ndd.


**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr-matnr  = |{ wa_vd_ndd-matnr ALPHA = IN }|.

     DATA gv_matcri18_10    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_ndd-matnr
       IMPORTING
         output = gv_matcri18_10.

     CLEAR: wa_matnr-matnr.

     wa_matnr-matnr = gv_matcri18_10 .

     CLEAR: gv_matcri18_10.

* END CONVERT
**********************************************************************

     wa_matnr-werks  = wa_vd_ndd-werks.
     wa_matnr-safra  = wa_vd_ndd-safra.
     COLLECT  wa_matnr INTO it_matnr.
     CLEAR wa_vd_ndd.
   ENDLOOP.

   SORT it_matnr BY matnr werks safra.

   LOOP AT it_matnr INTO wa_matnr.

     READ TABLE it_vd_cons_sf INTO wa_vd_cons_sf WITH KEY  matnr = wa_matnr-matnr
                                                           werks = wa_matnr-werks
                                                           safra = wa_matnr-safra.

     READ TABLE it_vd_ndd INTO wa_vd_ndd WITH KEY  matnr = wa_matnr-matnr
                                                   werks = wa_matnr-werks
                                                   safra = wa_matnr-safra.

*     READ TABLE IT_SAFRA INTO WA_SAFRA WITH  KEY MATNR =  WA_MATNR-MATNR.
     wa_zsdt0192-mandt       = sy-mandt.
     wa_zsdt0192-data        = sy-datum.
     wa_zsdt0192-tipo        = 'V'.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0192-matnr       = |{ wa_matnr-matnr ALPHA =  IN  }|.

     DATA gv_matcri18_01    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_matnr-matnr
       IMPORTING
         output = gv_matcri18_01.

     CLEAR: wa_zsdt0192-matnr.

     wa_zsdt0192-matnr = gv_matcri18_01.

     CLEAR: gv_matcri18_01.

* END CONVERT
**********************************************************************

     wa_zsdt0192-werks       = wa_matnr-werks.
     wa_zsdt0192-safra       = wa_matnr-safra.
     wa_zsdt0192-tp_safra    = '1'.
     wa_zsdt0192-qtd_t       = wa_vd_cons_sf-menge + wa_vd_ndd-vsaldo.
     wa_zsdt0192-qtd_u       = wa_vd_cons_sf-menge.
     wa_zsdt0192-qtd_n       = wa_vd_ndd-vsaldo.
     wa_zsdt0192-um_ov       = wa_vd_ndd-meins.
     IF wa_zsdt0192-um_ov IS INITIAL.
       wa_zsdt0192-um_ov    = wa_vd_cons_sf-meins.    "CS2019001220 - Sara Oikawa - Jun/2020
     ENDIF.
     wa_zsdt0192-usnam       = sy-uname.
     wa_zsdt0192-data_atual  = sy-datum.
     wa_zsdt0192-hora_atual  = sy-uzeit.

     APPEND wa_zsdt0192 TO it_zsdt0192.
     CLEAR: wa_matnr, wa_vd_cons_sf, wa_vd_ndd,  wa_zsdt0192, wa_safrinha.
   ENDLOOP.

   DELETE FROM zsdt0192
    WHERE data EQ sy-datum.

   MODIFY zsdt0192  FROM TABLE it_zsdt0192.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
*  Gravar Tabela ZSDT0256 - Processamento necessidades Matéria-Prima - OV
   SORT it_vd_ndd_ov     BY vbeln posnr matnr werks.
   SORT it_vd_cons_sf_ov BY vbeln posnr matnr werks.

   LOOP AT it_vd_cons_sf_ov INTO wa_vd_cons_sf.
     wa_matnr_ov-vbeln  = wa_vd_cons_sf-vbeln.
     wa_matnr_ov-posnr  = wa_vd_cons_sf-posnr.


**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr_ov-matnr  = |{ wa_vd_cons_sf-matnr ALPHA = IN }|.

     DATA gv_matcri18_11    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_cons_sf-matnr
       IMPORTING
         output = gv_matcri18_11.

     CLEAR: wa_matnr_ov-matnr.

     wa_matnr_ov-matnr = gv_matcri18_11.

     CLEAR: gv_matcri18_11.

* END CONVERT
**********************************************************************

     wa_matnr_ov-werks  = wa_vd_cons_sf-werks.
     wa_matnr_ov-safra  = wa_vd_cons_sf-safra.
     COLLECT  wa_matnr_ov INTO it_matnr_ov.
     CLEAR wa_vd_cons_sf.
   ENDLOOP.

   LOOP AT it_vd_ndd_ov INTO wa_vd_ndd_ov.
     wa_matnr_ov-vbeln  = wa_vd_ndd_ov-vbeln.
     wa_matnr_ov-posnr  = wa_vd_ndd_ov-posnr.



**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr_ov-matnr  = |{ wa_vd_ndd_ov-matnr ALPHA = IN }|.

     DATA gv_matcri18_12    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_ndd_ov-matnr
       IMPORTING
         output = gv_matcri18_12.

     CLEAR: wa_matnr_ov-matnr.

     wa_matnr_ov-matnr = gv_matcri18_12 .

     CLEAR: gv_matcri18_12.

* END CONVERT
**********************************************************************

     wa_matnr_ov-werks  = wa_vd_ndd_ov-werks.
     wa_matnr_ov-safra  = wa_vd_ndd_ov-safra.
     COLLECT  wa_matnr_ov INTO it_matnr_ov.
     CLEAR wa_vd_ndd_ov.
   ENDLOOP.

   SORT it_matnr_ov BY  matnr vbeln posnr werks safra.
   SORT it_safra  BY vbeln posnr werks safra.
   SORT it_saldos BY vbeln posnr matnr werks safra.
   SORT it_vd_cons_sf_ov BY vbeln posnr matnr werks safra.
   SORT it_vd_ndd_ov BY vbeln posnr matnr werks safra.

   " Para cada Matéria-Prima
   LOOP AT it_matnr_ov INTO wa_matnr_ov.

     " Obtém Material da OV
     CLEAR wa_safra.
     READ TABLE it_safra INTO wa_safra WITH KEY vbeln = wa_matnr_ov-vbeln
                                                posnr = wa_matnr_ov-posnr
                                                werks = wa_matnr_ov-werks
                                                safra = wa_matnr_ov-safra
                                       BINARY SEARCH.
     " Obtém Saldo da OV
     CLEAR wa_saldos.
     READ TABLE it_saldos INTO wa_saldos WITH KEY vbeln = wa_safra-vbeln
                                                  posnr = wa_safra-posnr
                                                  matnr = wa_safra-matnr
                                                  werks = wa_safra-werks
                                                  safra = wa_safra-safra
                                         BINARY SEARCH.

     " Obtém Qtde Consumida
     CLEAR wa_vd_cons_sf.
     READ TABLE it_vd_cons_sf_ov INTO wa_vd_cons_sf WITH KEY  vbeln = wa_matnr_ov-vbeln
                                                              posnr = wa_matnr_ov-posnr
                                                              matnr = wa_matnr_ov-matnr
                                                              werks = wa_matnr_ov-werks
                                                              safra = wa_matnr_ov-safra
                                                    BINARY SEARCH.
     " Obtém Qtde a Consumir
     CLEAR wa_vd_ndd_ov.
     READ TABLE it_vd_ndd_ov INTO wa_vd_ndd_ov WITH KEY  vbeln = wa_matnr_ov-vbeln
                                                         posnr = wa_matnr_ov-posnr
                                                         matnr = wa_matnr_ov-matnr
                                                         werks = wa_matnr_ov-werks
                                                         safra = wa_matnr_ov-safra
                                               BINARY SEARCH.

     wa_zsdt0256-mandt       = sy-mandt.
     wa_zsdt0256-data        = sy-datum.
     wa_zsdt0256-tipo        = 'V'.
     wa_zsdt0256-tp_safra    = '1'.
     wa_zsdt0256-usnam       = sy-uname.
     wa_zsdt0256-data_atual  = sy-datum.
     wa_zsdt0256-hora_atual  = sy-uzeit.

     " Dados da OV
     wa_zsdt0256-vbeln     = wa_safra-vbeln.
     wa_zsdt0256-posnr     = wa_safra-posnr.
     wa_zsdt0256-werks     = wa_safra-werks.
     wa_zsdt0256-safra     = wa_safra-safra.
     wa_zsdt0256-kunnr     = wa_safra-kunnr.
     wa_zsdt0256-cultura   = wa_safra-cultura.
     wa_zsdt0256-qtd_ov    = wa_safra-kwmeng.

     wa_zsdt0256-qtd_sald  = wa_saldos-vsaldo.

     " Dados de Consumo e Necessidade
     wa_zsdt0256-fator_mp  =  wa_vd_ndd_ov-fator.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0256-matnr_mp  = |{ wa_matnr_ov-matnr ALPHA =  IN  }|.

     DATA gv_matcri18_02    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_matnr_ov-matnr
       IMPORTING
         output = gv_matcri18_02.

     CLEAR: wa_zsdt0256-matnr_mp.

     wa_zsdt0256-matnr_mp = gv_matcri18_02.

     CLEAR: gv_matcri18_02.

* END CONVERT
**********************************************************************


     wa_zsdt0256-um_ov     =  wa_vd_ndd_ov-meins.
     IF wa_zsdt0256-um_ov IS INITIAL.
       wa_zsdt0256-um_ov  =  wa_vd_cons_sf-meins.
     ENDIF.

     wa_zsdt0256-qtd_cons  =  wa_vd_cons_sf-menge.

     wa_zsdt0256-qtd_ac    =  wa_vd_ndd_ov-vsaldo.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0256-matnr     = |{ wa_safra-matnr ALPHA =  IN  }|.

     DATA gv_matcri18_03    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_safra-matnr
       IMPORTING
         output = gv_matcri18_03.

     CLEAR: wa_zsdt0256-matnr.

     wa_zsdt0256-matnr = gv_matcri18_03.

     CLEAR: gv_matcri18_03.

* END CONVERT
**********************************************************************

     APPEND wa_zsdt0256 TO it_zsdt0256.
     CLEAR: wa_zsdt0256.

   ENDLOOP.

   DELETE FROM zsdt0256
    WHERE data EQ sy-datum.

   MODIFY zsdt0256  FROM TABLE it_zsdt0256.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

   REFRESH: it_stpo, it_zsdt0192, it_vd_cons_sf, it_vd_ndd, it_aufm_p, it_aufm_p_aux, it_saldos, it_saldos_aux, it_saldos_mat.
   CLEAR: wa_aufm, wa_safra, wa_vd_cons_sf, wa_zsdt0192 ,wa_stpo, wa_saldos, vsaldo, wa_vd_ndd, vtotal, vsaldo.

   REFRESH: it_zsdt0256, it_vd_cons_sfe, it_vd_cons_sf_ov, it_vd_ndd_ov, it_matnr, it_matnr_ov, it_safra.
   CLEAR:   wa_zsdt0256.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SAFRINHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM z_safrinha_ov.

   REFRESH: it_zsdt0192, it_aufm_aux, it_matnr, it_total, it_stpo, it_stko.
   CLEAR: vmatnr, wa_matnr, wa_total.

   REFRESH: it_zsdt0256, it_vd_cons_sf, it_vd_cons_sfe, it_vd_cons_sf_ov, it_vd_ndd_ov,
            it_matnr, it_matnr_ov, it_safra.


   CHECK it_safrinha IS NOT INITIAL.

   SELECT g~mblnr
          h~kdauf
          h~kdpos
          g~matnr
          g~menge
          g~meins
          g~werks
          h~kdauf
          g~bwart
       FROM aufm AS g
       INNER JOIN aufk AS h ON h~aufnr = g~aufnr
       INNER JOIN jest AS i ON i~objnr = h~objnr
     INTO TABLE it_aufm
     FOR ALL ENTRIES IN it_safrinha
    WHERE g~bwart IN ( '261', '262' )
     AND  h~kdauf EQ it_safrinha-vbeln
     AND  h~kdpos EQ it_safrinha-posnr
     AND  i~stat  IN ( 'I0009', 'I0010' )
     AND  i~inact NE 'X'.

   SORT it_aufm BY kdauf.

   LOOP AT it_aufm ASSIGNING FIELD-SYMBOL(<wa_aufm>).
     READ TABLE it_safrinha INTO wa_safrinha WITH KEY vbeln = <wa_aufm>-kdauf.
     <wa_aufm>-safra = wa_safrinha-safra.
   ENDLOOP.

   MOVE-CORRESPONDING it_aufm TO it_aufm_aux.

   SORT it_aufm_aux BY  matnr werks safra.
   DELETE ADJACENT DUPLICATES FROM it_aufm_aux COMPARING matnr werks safra.

   LOOP AT it_aufm_aux INTO DATA(wa_aufm_aux).
     LOOP AT it_aufm INTO wa_aufm
        WHERE matnr = wa_aufm_aux-matnr
        AND   werks = wa_aufm_aux-werks
        AND   safra = wa_aufm_aux-safra.

       IF wa_aufm-bwart EQ '261'.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf.
         CLEAR wa_aufm.
       ENDIF.

       IF wa_aufm-bwart EQ '262'.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sfe.
         CLEAR wa_aufm.
       ENDIF.
     ENDLOOP.
     CLEAR wa_aufm_aux.
   ENDLOOP.

**==============================================================================
****  Retirar a quantidade estornada do total movimento 261
   LOOP AT it_vd_cons_sf ASSIGNING FIELD-SYMBOL(<w_vd_cons_sf>). "Total de movimento 261
     LOOP AT it_vd_cons_sfe ASSIGNING FIELD-SYMBOL(<w_vd_cons_sfe>) "Total de movimento 262
       WHERE matnr EQ <w_vd_cons_sf>-matnr
         AND werks EQ <w_vd_cons_sf>-werks
         AND safra EQ <w_vd_cons_sf>-safra.

       <w_vd_cons_sf>-menge = <w_vd_cons_sf>-menge - <w_vd_cons_sfe>-menge.
     ENDLOOP.
   ENDLOOP.
**==============================================================================

* Início - CS2019001220 - Sara Oikawa - Jun/2020
*** Totais Por OV
*** Armazena Totais Por OV

   REFRESH: it_aufm_aux, it_vd_cons_sf_ov.

   it_aufm_aux[] = it_aufm[].

   SORT it_aufm_aux BY  vbeln posnr matnr werks safra.
   DELETE ADJACENT DUPLICATES FROM it_aufm_aux COMPARING vbeln posnr matnr werks safra.

   "TOTALIZA A QUANTIDADE POR OV MATERIA-PRIMA E CENTRO.
   LOOP AT it_aufm_aux INTO wa_aufm_aux.
     LOOP AT it_aufm INTO wa_aufm
        WHERE vbeln = wa_aufm_aux-vbeln
          AND posnr = wa_aufm_aux-posnr
          AND matnr = wa_aufm_aux-matnr
          AND werks = wa_aufm_aux-werks
       	  AND safra = wa_aufm_aux-safra.

       IF wa_aufm-bwart EQ '261'.
         wa_vd_cons_sf-vbeln   =  wa_aufm-vbeln.
         wa_vd_cons_sf-posnr   =  wa_aufm-posnr.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf_ov.
       ENDIF.

       IF wa_aufm-bwart EQ '262'.
         wa_vd_cons_sf-vbeln   =  wa_aufm-vbeln.
         wa_vd_cons_sf-posnr   =  wa_aufm-posnr.
         wa_vd_cons_sf-matnr   =  wa_aufm-matnr.
         wa_vd_cons_sf-meins   =  wa_aufm-meins.
         wa_vd_cons_sf-menge   =  wa_aufm-menge * -1.
         wa_vd_cons_sf-werks   =  wa_aufm-werks.
         wa_vd_cons_sf-safra   =  wa_aufm-safra.
         COLLECT wa_vd_cons_sf INTO it_vd_cons_sf_ov.
       ENDIF.

       CLEAR wa_vd_cons_sf.
     ENDLOOP.

     CLEAR wa_aufm_aux.
   ENDLOOP.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020

   REFRESH: it_aufm_aux, it_aufm.

   SELECT  j~mblnr
           k~kdauf
           k~kdpos
           j~matnr
           j~menge
           j~meins
           k~kdauf
           j~bwart
  FROM aufm AS j
  INNER JOIN aufk AS k ON k~aufnr = j~aufnr
  INNER JOIN jest AS l ON l~objnr = k~objnr
  INTO TABLE it_aufm_p
    FOR ALL ENTRIES IN it_safrinha
 WHERE j~bwart IN ( '131', '132' )
  AND  k~kdauf EQ it_safrinha-vbeln
  AND  k~kdpos EQ it_safrinha-posnr
  AND  l~stat  IN ( 'I0009', 'I0010' )
  AND  l~inact NE 'X'.

   SORT it_aufm_p BY kdauf.

   LOOP AT it_aufm_p ASSIGNING FIELD-SYMBOL(<wa_aufm_p>).
     READ TABLE it_safrinha INTO wa_safrinha WITH KEY vbeln = <wa_aufm_p>-kdauf.
     <wa_aufm_p>-safra = wa_safrinha-safra.
   ENDLOOP.

   SORT it_aufm_p BY vbeln posnr matnr.
   MOVE-CORRESPONDING it_aufm_p TO it_aufm_p_aux.
   DELETE ADJACENT DUPLICATES FROM  it_aufm_p_aux COMPARING vbeln posnr matnr.

   LOOP AT it_aufm_p_aux INTO DATA(wa_aufm_p_aux).
     LOOP AT it_aufm_p INTO wa_aufm_p
        WHERE vbeln = wa_aufm_p_aux-vbeln
        AND   posnr = wa_aufm_p_aux-posnr
        AND   matnr = wa_aufm_p_aux-matnr.

       IF wa_aufm_p-bwart EQ '131'.
         wa_total-vbeln  = wa_aufm_p-vbeln.
         wa_total-posnr  = wa_aufm_p-posnr.
         wa_total-matnr  = wa_aufm_p-matnr.
         wa_total-meins  = wa_aufm_p-meins.
         wa_total-vtotal = wa_aufm_p-menge.
         wa_total-safra  = wa_aufm_p-safra.
         COLLECT wa_total INTO it_total.
         CLEAR: wa_aufm_p, wa_total.
       ENDIF.

       IF wa_aufm_p-bwart EQ '132'.
         wa_total-vbeln  = wa_aufm_p-vbeln.
         wa_total-posnr  = wa_aufm_p-posnr.
         wa_total-matnr  = wa_aufm_p-matnr.
         wa_total-meins  = wa_aufm_p-meins.
         wa_total-vtotal = wa_aufm_p-menge.
         wa_total-safra  = wa_aufm_p-safra.
         COLLECT wa_total INTO t_total.
         CLEAR: wa_aufm_p, wa_total.
       ENDIF.
     ENDLOOP.
     CLEAR wa_aufm_p_aux.
   ENDLOOP.

**====================================================================================================
   "Retirar a quantidade movimento '131' da quantidade movimento '132'.
   LOOP AT it_total ASSIGNING FIELD-SYMBOL(<w_total>). "Total materiais movimento '131'
     LOOP AT t_total ASSIGNING FIELD-SYMBOL(<l_total>) "Total materiais movimento '132'
       WHERE vbeln EQ <w_total>-vbeln
       AND posnr  EQ <w_total>-posnr
       AND matnr EQ <w_total>-matnr
       AND safra EQ <w_total>-safra.

       <w_total>-vtotal = <w_total>-vtotal - <l_total>-vtotal. "Retirando quantidade movimento '132'
     ENDLOOP.
   ENDLOOP.
**====================================================================================================

   LOOP AT it_safrinha INTO wa_safrinha.
     wa_saldos-vbeln  =  wa_safrinha-vbeln.
     wa_saldos-posnr  =  wa_safrinha-posnr.
     wa_saldos-matnr  =  wa_safrinha-matnr.
     wa_saldos-werks  =  wa_safrinha-werks.
     wa_saldos-safra  =  wa_safrinha-safra.

     READ TABLE it_total INTO wa_total WITH KEY vbeln = wa_safrinha-vbeln
                                                posnr = wa_safrinha-posnr
                                                matnr = wa_safrinha-matnr.

     IF sy-subrc EQ 0.
*     LOOP AT IT_TOTAL INTO WA_TOTAL
*       WHERE VBELN = WA_SAFRINHA-VBELN
*       AND   POSNR = WA_SAFRINHA-POSNR
*       AND   MATNR = WA_SAFRINHA-MATNR.
*       WA_SALDOS-VBELN  =  WA_SAFRINHA-VBELN.
*       WA_SALDOS-POSNR  =  WA_SAFRINHA-POSNR.
*       WA_SALDOS-MATNR  =  WA_SAFRINHA-MATNR.
*       WA_SALDOS-WERKS  =  WA_SAFRINHA-WERKS.
*       WA_SALDOS-SAFRA  =  WA_SAFRINHA-SAFRA.

       IF wa_safrinha-vrkme EQ 'TO'.
         wa_safrinha-kwmeng = wa_safrinha-kwmeng / 100.
       ENDIF.

       IF wa_total-meins EQ 'TO'.
         wa_total-vtotal =  wa_total-vtotal  / 100.
       ENDIF.

       wa_saldos-vsaldo = wa_safrinha-kwmeng -  wa_total-vtotal.

       IF wa_total-meins EQ 'UN'.
         wa_saldos-meins = wa_total-meins.
       ELSE.
         wa_saldos-meins = 'KG'.
       ENDIF.
*       COLLECT WA_SALDOS INTO IT_SALDOS.
*       CLEAR WA_TOTAL.
*     ENDLOOP.
     ELSE.
       wa_saldos-vsaldo = wa_safrinha-kwmeng.
       wa_saldos-meins  = wa_safrinha-vrkme.
     ENDIF.

     COLLECT wa_saldos INTO it_saldos.
     CLEAR wa_total.
     CLEAR wa_safrinha.
   ENDLOOP.

   DELETE it_saldos WHERE vsaldo LE 0.
   SORT it_saldos BY matnr werks safra.

   MOVE-CORRESPONDING it_saldos TO it_saldos_aux.
   DELETE ADJACENT DUPLICATES FROM it_saldos_aux COMPARING matnr werks safra.

   LOOP AT it_saldos_aux INTO wa_saldos_aux.
     LOOP AT it_saldos INTO wa_saldos
       WHERE matnr = wa_saldos_aux-matnr
       AND   werks = wa_saldos_aux-werks
       AND   safra = wa_saldos_aux-safra.

       wa_saldos_mat-matnr  = wa_saldos-matnr.
       wa_saldos_mat-werks  = wa_saldos-werks.
       wa_saldos_mat-vsaldo = wa_saldos-vsaldo.
       wa_saldos_mat-safra  = wa_saldos-safra.

       COLLECT wa_saldos_mat INTO it_saldos_mat.
       CLEAR: wa_saldos, wa_saldos_mat.
     ENDLOOP.
     CLEAR wa_saldos_aux.
   ENDLOOP.

   SORT it_saldos_mat BY matnr werks safra.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
* Saldo a produzir por OV/Material

*   LOOP AT IT_SALDOS_MAT INTO WA_SALDOS_MAT.
*     BOM_USAGE  = '1'.
*     CONCATENATE  SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM+0(4) INTO DATUV.
*
*     CALL FUNCTION 'CSAP_MAT_BOM_READ'
*       EXPORTING
*         MATERIAL   = WA_SALDOS_MAT-MATNR
*         PLANT      = WA_SALDOS_MAT-WERKS
*         BOM_USAGE  = BOM_USAGE
*         VALID_FROM = DATUV
*       IMPORTING
*         FL_WARNING = CK_FL_WARNING
*       TABLES
*         T_STPO     = IT_STPO
*         T_STKO     = IT_STKO
*       EXCEPTIONS
*         ERROR      = 1
*         OTHERS     = 2.
*
*     LOOP AT IT_STPO INTO WA_STPO.
*
*       READ TABLE IT_STKO INTO WA_STKO INDEX 1.
*
*       REPLACE ALL OCCURRENCES OF '.' IN WA_STKO-BASE_QUAN WITH ''.
*       REPLACE ALL OCCURRENCES OF '.' IN WA_STPO-COMP_QTY WITH ''.
*
*       QTD1   =  CONV #( WA_STKO-BASE_QUAN ).
*       VMATNR = |{ WA_STPO-COMPONENT ALPHA = IN }|.
*       QTD2   =   CONV #( WA_STPO-COMP_QTY ).
*
*       VSALDO =  ( QTD2 / QTD1 ).
*       WA_VD_NDD-VSALDO = ( VSALDO * WA_SALDOS_MAT-VSALDO ).
*       WA_VD_NDD-MATNR  =  VMATNR.
*       WA_VD_NDD-WERKS  =  WA_SALDOS_MAT-WERKS.
*       WA_VD_NDD-MEINS  =  WA_STPO-COMP_UNIT.
*       WA_VD_NDD-SAFRA  =  WA_SALDOS_MAT-SAFRA.
*       COLLECT WA_VD_NDD INTO  IT_VD_NDD.
*       CLEAR: WA_STPO, WA_SALDOS, VSALDO, WA_VD_NDD.
*     ENDLOOP.
*     CLEAR: WA_SALDOS_MAT, QTD1, QTD2.
*   ENDLOOP.

   LOOP AT it_saldos INTO wa_saldos.

     bom_usage  = '1'.
     CONCATENATE  sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO datuv.

     CALL FUNCTION 'CSAP_MAT_BOM_READ'
       EXPORTING
         material   = wa_saldos-matnr
         plant      = wa_saldos-werks
         bom_usage  = bom_usage
         valid_from = datuv
       IMPORTING
         fl_warning = ck_fl_warning
       TABLES
         t_stpo     = it_stpo
         t_stko     = it_stko
       EXCEPTIONS
         error      = 1
         OTHERS     = 2.

     LOOP AT it_stpo INTO wa_stpo.
       READ TABLE it_stko INTO wa_stko INDEX 1.

       REPLACE ALL OCCURRENCES OF '.' IN  wa_stko-base_quan WITH ''.
       REPLACE ALL OCCURRENCES OF '.' IN  wa_stpo-comp_qty WITH ''.
       REPLACE ALL OCCURRENCES OF ',' IN  wa_stko-base_quan WITH '.'.      "<< RIM-SKM-IR127070-13.02.23
       REPLACE ALL OCCURRENCES OF ',' IN  wa_stpo-comp_qty WITH '.'.       "<< RIM-SKM-IR127070-13.02.23

       qtd1 = CONV #( wa_stko-base_quan ).



**********************************************************************
* PSA CONVERT MATNR 18
       "vmatnr = |{ wa_stpo-component ALPHA = IN }|.

       DATA gv_matcri18_13    TYPE matnr18.

*** Formata o código do material
       CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
           input  = wa_stpo-component
         IMPORTING
           output = gv_matcri18_13.

       CLEAR: vmatnr.

       vmatnr = gv_matcri18_13.

       CLEAR: gv_matcri18_13.

* END CONVERT
**********************************************************************

       qtd2 = CONV #( wa_stpo-comp_qty ).

       vsaldo           = ( qtd2 / qtd1 ).
       wa_vd_ndd-vsaldo = ( vsaldo * wa_saldos-vsaldo ).

       wa_vd_ndd-matnr  =  vmatnr.
       wa_vd_ndd-werks  =  wa_saldos-werks.
       wa_vd_ndd-meins  =  wa_stpo-comp_unit.
       wa_vd_ndd-safra  =  wa_saldos-safra.

       COLLECT wa_vd_ndd INTO  it_vd_ndd.

       MOVE-CORRESPONDING wa_vd_ndd TO wa_vd_ndd_ov.
       wa_vd_ndd_ov-vbeln  =  wa_saldos-vbeln.
       wa_vd_ndd_ov-posnr  =  wa_saldos-posnr.
       wa_vd_ndd_ov-fator  =  vsaldo.

       COLLECT wa_vd_ndd_ov INTO it_vd_ndd_ov.

       CLEAR: wa_stpo, vsaldo, wa_vd_ndd, wa_vd_ndd_ov, wa_stko.

     ENDLOOP.

     CLEAR: wa_safra, wa_saldos, qtd1, qtd2.

   ENDLOOP.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020

   SORT it_vd_ndd BY matnr werks.
   SORT it_vd_cons_sf BY matnr werks.

   LOOP AT  it_vd_cons_sf INTO wa_vd_cons_sf.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr-matnr  = |{ wa_vd_cons_sf-matnr ALPHA = IN }|.

     DATA gv_matcri18_14    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_cons_sf-matnr
       IMPORTING
         output = gv_matcri18_14.

     CLEAR: wa_matnr-matnr.

     wa_matnr-matnr = gv_matcri18_14.

     CLEAR: gv_matcri18_14.

* END CONVERT
**********************************************************************

     wa_matnr-werks  = wa_vd_cons_sf-werks.
     wa_matnr-safra  = wa_vd_cons_sf-safra.
     COLLECT wa_matnr INTO it_matnr.
     CLEAR wa_vd_cons_sf.
   ENDLOOP.

   LOOP AT it_vd_ndd INTO wa_vd_ndd.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr-matnr  = |{ wa_vd_ndd-matnr ALPHA = IN }|.

     DATA gv_matcri18_15    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_ndd-matnr
       IMPORTING
         output = gv_matcri18_15.

     CLEAR: wa_matnr-matnr.

     wa_matnr-matnr = gv_matcri18_15.

     CLEAR: gv_matcri18_15.

* END CONVERT
**********************************************************************

     wa_matnr-werks  = wa_vd_ndd-werks.
     wa_matnr-safra  = wa_vd_ndd-safra.
     COLLECT wa_matnr INTO it_matnr.
     CLEAR wa_vd_cons_sf.
   ENDLOOP.

   SORT it_matnr BY matnr werks safra.

   LOOP AT it_matnr  INTO wa_matnr.

     READ TABLE it_vd_cons_sf INTO wa_vd_cons_sf WITH KEY  matnr = wa_matnr-matnr
                                                           werks = wa_matnr-werks
                                                           safra = wa_matnr-safra.

     READ TABLE it_vd_ndd    INTO wa_vd_ndd WITH KEY  matnr = wa_matnr-matnr
                                                      werks = wa_matnr-werks
                                                      safra = wa_matnr-safra.

     wa_zsdt0192-mandt       = sy-mandt.
     wa_zsdt0192-data        = sy-datum.
     wa_zsdt0192-tipo        = 'V'.


**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0192-matnr       = |{ wa_matnr-matnr ALPHA = IN  }|.

     DATA gv_matcri18_16    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_matnr-matnr
       IMPORTING
         output = gv_matcri18_16.

     CLEAR: wa_zsdt0192-matnr.

     wa_zsdt0192-matnr = gv_matcri18_16.

     CLEAR: gv_matcri18_16.

* END CONVERT
**********************************************************************


     wa_zsdt0192-werks       = wa_matnr-werks.
     wa_zsdt0192-safra       = wa_matnr-safra.
     wa_zsdt0192-tp_safra    = '2'.
     wa_zsdt0192-qtd_t       = wa_vd_cons_sf-menge + wa_vd_ndd-vsaldo.
     wa_zsdt0192-qtd_u       = wa_vd_cons_sf-menge.
     wa_zsdt0192-qtd_n       = wa_vd_ndd-vsaldo.
     wa_zsdt0192-um_ov       = wa_vd_ndd-meins.
     IF wa_zsdt0192-um_ov IS INITIAL.
       wa_zsdt0192-um_ov    = wa_vd_cons_sf-meins.    "CS2019001220 - Sara Oikawa - Jun/2020
     ENDIF.
     wa_zsdt0192-usnam       = sy-uname.
     wa_zsdt0192-data_atual  = sy-datum.
     wa_zsdt0192-hora_atual  = sy-uzeit.

     APPEND wa_zsdt0192 TO it_zsdt0192.
     CLEAR: wa_matnr, wa_vd_cons_sf, wa_vd_ndd, wa_zsdt0192.
   ENDLOOP.

   MODIFY zsdt0192  FROM TABLE it_zsdt0192.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
*  Gravar Tabela ZSDT0256 - Processamento necessidades Matéria-Prima - OV
   SORT it_vd_ndd_ov     BY vbeln posnr matnr werks.
   SORT it_vd_cons_sf_ov BY vbeln posnr matnr werks.

   LOOP AT it_vd_cons_sf_ov INTO wa_vd_cons_sf.
     wa_matnr_ov-vbeln  = wa_vd_cons_sf-vbeln.
     wa_matnr_ov-posnr  = wa_vd_cons_sf-posnr.


**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr_ov-matnr  = |{ wa_vd_cons_sf-matnr ALPHA = IN }|.

     DATA gv_matcri18_17    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_cons_sf-matnr
       IMPORTING
         output = gv_matcri18_17.

     CLEAR: wa_matnr_ov-matnr.

     wa_matnr_ov-matnr = gv_matcri18_17.

     CLEAR: gv_matcri18_17.

* END CONVERT
**********************************************************************

     wa_matnr_ov-werks  = wa_vd_cons_sf-werks.
     wa_matnr_ov-safra  = wa_vd_cons_sf-safra.
     COLLECT  wa_matnr_ov INTO it_matnr_ov.
     CLEAR wa_vd_cons_sf.
   ENDLOOP.

   LOOP AT it_vd_ndd_ov INTO wa_vd_ndd_ov.
     wa_matnr_ov-vbeln  = wa_vd_ndd_ov-vbeln.
     wa_matnr_ov-posnr  = wa_vd_ndd_ov-posnr.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_matnr_ov-matnr  = |{ wa_vd_ndd_ov-matnr ALPHA = IN }|.

     DATA gv_matcri18_18    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_vd_ndd_ov-matnr
       IMPORTING
         output = gv_matcri18_18.

     CLEAR: wa_matnr_ov-matnr.

     wa_matnr_ov-matnr = gv_matcri18_18.

     CLEAR: gv_matcri18_18.

* END CONVERT
**********************************************************************

     wa_matnr_ov-werks  = wa_vd_ndd_ov-werks.
     wa_matnr_ov-safra  = wa_vd_ndd_ov-safra.
     COLLECT  wa_matnr_ov INTO it_matnr_ov.
     CLEAR wa_vd_ndd_ov.
   ENDLOOP.

   SORT it_matnr_ov BY  matnr vbeln posnr werks safra.
   SORT it_safrinha BY vbeln posnr werks safra.
   SORT it_saldos BY vbeln posnr matnr werks safra.
   SORT it_vd_cons_sf_ov BY vbeln posnr matnr werks safra.
   SORT it_vd_ndd_ov BY vbeln posnr matnr werks safra.

   " Para cada Matéria-Prima
   LOOP AT it_matnr_ov INTO wa_matnr_ov.

     " Obtém Material da OV
     CLEAR wa_safra.
     READ TABLE it_safrinha INTO wa_safra WITH KEY vbeln = wa_matnr_ov-vbeln
                                                   posnr = wa_matnr_ov-posnr
                                                   werks = wa_matnr_ov-werks
                                                   safra = wa_matnr_ov-safra
                                          BINARY SEARCH.
     " Obtém Saldo da OV
     CLEAR wa_saldos.
     READ TABLE it_saldos INTO wa_saldos WITH KEY vbeln = wa_safra-vbeln
                                                  posnr = wa_safra-posnr
                                                  matnr = wa_safra-matnr
                                                  werks = wa_safra-werks
                                                  safra = wa_safra-safra
                                         BINARY SEARCH.

     " Obtém Qtde Consumida
     CLEAR wa_vd_cons_sf.
     READ TABLE it_vd_cons_sf_ov INTO wa_vd_cons_sf WITH KEY  vbeln = wa_matnr_ov-vbeln
                                                              posnr = wa_matnr_ov-posnr
                                                              matnr = wa_matnr_ov-matnr
                                                              werks = wa_matnr_ov-werks
                                                              safra = wa_matnr_ov-safra
                                                    BINARY SEARCH.
     " Obtém Qtde a Consumir
     CLEAR wa_vd_ndd_ov.
     READ TABLE it_vd_ndd_ov INTO wa_vd_ndd_ov WITH KEY  vbeln = wa_matnr_ov-vbeln
                                                         posnr = wa_matnr_ov-posnr
                                                         matnr = wa_matnr_ov-matnr
                                                         werks = wa_matnr_ov-werks
                                                         safra = wa_matnr_ov-safra
                                               BINARY SEARCH.

     wa_zsdt0256-mandt       = sy-mandt.
     wa_zsdt0256-data        = sy-datum.
     wa_zsdt0256-tipo        = 'V'.
     wa_zsdt0256-tp_safra    = '2'.
     wa_zsdt0256-usnam       = sy-uname.
     wa_zsdt0256-data_atual  = sy-datum.
     wa_zsdt0256-hora_atual  = sy-uzeit.

     " Dados da OV
     wa_zsdt0256-vbeln     = wa_safra-vbeln.
     wa_zsdt0256-posnr     = wa_safra-posnr.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0256-matnr     = |{ wa_safra-matnr ALPHA =  IN  }|.

     DATA gv_matcri18_04    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_safra-matnr
       IMPORTING
         output = gv_matcri18_04.

     CLEAR: wa_zsdt0256-matnr.

     wa_zsdt0256-matnr = gv_matcri18_04.

     CLEAR: gv_matcri18_04.

* END CONVERT
**********************************************************************

     wa_zsdt0256-werks     = wa_safra-werks.
     wa_zsdt0256-safra     = wa_safra-safra.
     wa_zsdt0256-kunnr     = wa_safra-kunnr.
     wa_zsdt0256-cultura   = wa_safra-cultura.
     wa_zsdt0256-qtd_ov    = wa_safra-kwmeng.

     wa_zsdt0256-qtd_sald  = wa_saldos-vsaldo.

     " Dados de Consumo e Necessidade
     wa_zsdt0256-fator_mp  =  wa_vd_ndd_ov-fator.

**********************************************************************
* PSA CONVERT MATNR 18
     "wa_zsdt0256-matnr_mp  = |{ wa_matnr_ov-matnr ALPHA =  IN  }|.

     DATA gv_matcri18_05    TYPE matnr18.

*** Formata o código do material
     CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
         input  = wa_matnr_ov-matnr
       IMPORTING
         output = gv_matcri18_05.

     CLEAR: wa_zsdt0256-matnr_mp.

     wa_zsdt0256-matnr_mp = gv_matcri18_05.

     CLEAR: gv_matcri18_05.

* END CONVERT
**********************************************************************

     wa_zsdt0256-um_ov     =  wa_vd_ndd_ov-meins.
     IF wa_zsdt0256-um_ov IS INITIAL.
       wa_zsdt0256-um_ov  =  wa_vd_cons_sf-meins.
     ENDIF.

     wa_zsdt0256-qtd_cons  =  wa_vd_cons_sf-menge.

     wa_zsdt0256-qtd_ac    =  wa_vd_ndd_ov-vsaldo.


     APPEND wa_zsdt0256 TO it_zsdt0256.
     CLEAR: wa_zsdt0256.

   ENDLOOP.

   MODIFY zsdt0256  FROM TABLE it_zsdt0256.

* Fim - CS2019001220 - Sara Oikawa - Jun/2020

   REFRESH: it_stpo, it_zsdt0192, it_vd_cons_sf, it_vd_ndd, it_aufm_p, it_aufm_p_aux, it_saldos, it_saldos_aux, it_saldos_mat.
   CLEAR: wa_aufm, wa_safrinha, wa_vd_cons_sf, wa_zsdt0192 ,wa_stpo, wa_saldos, vsaldo, wa_vd_ndd, vtotal, vsaldo.

   REFRESH: it_zsdt0256, it_vd_cons_sf_ov, it_vd_ndd_ov, it_matnr_ov, it_safrinha.
   CLEAR:   wa_zsdt0256, wa_matnr_ov, wa_safra.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS_PR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM z_busca_dados_pr.
   FREE: it_dados_aux[].
   DATA: cont           TYPE p,
         ws_zsdt0041    TYPE ty_zsdt0041_proc,
         vg_vbelv       TYPE vbeln,
         lv_key_sim_old TYPE char16.

   "SAFRA
   SELECT  b~vbeln,
           a~posnr,
           a~matnr,
           a~werks,
           a~kwmeng,
           a~vrkme,
           d~safra_apl AS safra,
           b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
           d~cultura_apl AS cultura
     FROM vbap AS  a
     INNER JOIN vbak AS b ON b~vbeln = a~vbeln
     INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
     INNER JOIN zsdt0041 AS d ON d~vbeln = f~vbeln AND d~matnr = a~matnr
     INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
     INNER JOIN mara  AS  c ON c~matnr = a~matnr
     APPENDING CORRESPONDING FIELDS OF TABLE @it_dados
   WHERE  a~werks   IN @it_werks
     AND  b~spart   EQ '02'
     AND  b~auart   NOT IN @it_auart
     AND  c~mtart   EQ 'ZHAW'
     AND  d~safra_apl   IN @r_safra
     AND  d~cultura_apl IN @it_cultura
     AND  f~lifsp   NE '12'
   GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  d~safra_apl,  b~kunnr, d~cultura_apl.

   FREE: it_dados_aux[].
   SELECT  b~vbeln,
           a~posnr,
           a~matnr,
           a~werks,
           a~kwmeng,
           a~vrkme,
*           e~safra,
           b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
*           e~cultura,
           d~doc_simulacao,
           d~matnrv
     FROM vbap AS  a
     INNER JOIN vbak AS b ON b~vbeln = a~vbeln
     INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
     INNER JOIN zsdt0090 AS d ON d~vbeln = f~vbeln
     INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
     INNER JOIN mara  AS  c ON c~matnr = a~matnr
     APPENDING CORRESPONDING FIELDS OF TABLE @it_dados_aux
   WHERE  a~werks   IN @it_werks
     AND  b~spart   EQ '02'
     AND  b~auart   NOT IN @it_auart
     AND  c~mtart   EQ 'ZHAW'
*     AND  e~safra   IN @r_safra
*     AND  e~cultura IN @it_cultura
     AND  f~lifsp   NE '12'
   GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  e~safra,  b~kunnr, e~cultura, d~doc_simulacao, d~matnrv.

   SORT it_dados BY vbeln posnr.
   DELETE ADJACENT DUPLICATES FROM it_dados COMPARING vbeln posnr.


   "Verifica se safra foi preenchida, filtrar por safra / USER STORY 106316 / AOENNING.

   DATA(lit_dados_sim) = it_dados_aux[].
   SORT lit_dados_sim BY doc_simulacao.
   DELETE ADJACENT DUPLICATES FROM lit_dados_sim COMPARING doc_simulacao.

   IF lit_dados_sim[] IS NOT INITIAL.
     SELECT  doc_simulacao, vbeln ,posnn, vbelv, posnv, estorno, matnrv
        FROM zsdt0090 INTO TABLE @tg_zsdt0090_proc
        FOR ALL ENTRIES IN @lit_dados_sim
      WHERE doc_simulacao EQ @lit_dados_sim-doc_simulacao.

     IF tg_zsdt0090_proc[] IS NOT INITIAL.
       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 INTO TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbelv.

       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 APPENDING TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbeln.

       IF tg_zsdt0041_proc[] IS NOT INITIAL.

         SELECT doc_simulacao , safra
          FROM zsdt0040 INTO TABLE @DATA(tg_zsdt0040_proc)
          FOR ALL ENTRIES IN @tg_zsdt0041_proc
        WHERE doc_simulacao EQ @tg_zsdt0041_proc-doc_simulacao.

       ENDIF.

       SORT tg_zsdt0041_proc.
       DELETE ADJACENT DUPLICATES FROM tg_zsdt0041_proc COMPARING ALL FIELDS.

     ENDIF.
   ENDIF.


   IF r_safra[] IS NOT INITIAL.
     DELETE tg_zsdt0090_proc WHERE estorno EQ abap_true.
     DELETE tg_zsdt0090_proc WHERE vbeln   EQ space AND posnn EQ space.
     DELETE tg_zsdt0090_proc WHERE vbelv   EQ space OR  posnv EQ space.

     SORT: tg_zsdt0090_proc BY vbeln posnn matnrv,
           tg_zsdt0041_proc BY vbeln,
           tg_zsdt0040_proc BY doc_simulacao,
           it_dados_aux[]   BY vbeln posnr.

     LOOP AT it_dados_aux ASSIGNING FIELD-SYMBOL(<ls_dados>).
       lv_key_sim_old = <ls_dados>-vbeln && <ls_dados>-posnr.
       DATA(l_count) = 0.

       WHILE lv_key_sim_old IS NOT INITIAL.

         ADD 1 TO l_count.
         DATA(_key_simulador_busca) = lv_key_sim_old.

         PERFORM f_get_safra_zsdt0090 USING _key_simulador_busca <ls_dados>-matnrv
                                   CHANGING lv_key_sim_old
                                            ws_zsdt0041.

         IF ws_zsdt0041-vbeln IS NOT INITIAL.
           <ls_dados>-safra   = ws_zsdt0041-safra_apl.
           <ls_dados>-cultura = ws_zsdt0041-cultura_apl.

           IF <ls_dados>-safra EQ 0.
             READ TABLE tg_zsdt0040_proc ASSIGNING FIELD-SYMBOL(<fs_zsdt0040_proc>) WITH KEY  doc_simulacao = ws_zsdt0041-doc_simulacao BINARY SEARCH.
             IF sy-subrc EQ 0.
               <ls_dados>-safra   = <fs_zsdt0040_proc>-safra.

             ENDIF.
           ENDIF.

           EXIT.
         ENDIF.
       ENDWHILE.
     ENDLOOP.

     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux BY safra.
       DELETE it_dados_aux[] WHERE safra NOT IN r_safra OR cultura NOT IN it_cultura.

       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados[].
     ENDIF.

   ELSE.
     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados[].
     ENDIF.
   ENDIF.

   IF it_dados[] IS NOT INITIAL.
     MOVE it_dados TO it_safra.
   ENDIF.

   PERFORM z_safra_pr.

   REFRESH it_dados[].

   "SAFRINHA
   SELECT   b~vbeln,
            a~posnr,
            a~matnr,
            a~werks,
            a~kwmeng,
            a~vrkme,
            d~safra_apl AS safra,
            b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
            d~cultura_apl AS cultura
    FROM vbap AS  a
    INNER JOIN vbak AS b ON b~vbeln = a~vbeln
    INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
    INNER JOIN zsdt0041 AS d ON d~vbeln = f~vbeln AND d~matnr = a~matnr
    INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
    INNER JOIN mara  AS  c ON c~matnr = a~matnr
   APPENDING CORRESPONDING FIELDS OF TABLE @it_dados
  WHERE  a~werks   IN @it_werks
    AND  b~spart   EQ '02'
    AND  b~auart   NOT IN  @it_auart
    AND  c~mtart   EQ 'ZHAW'
    AND  d~safra_apl   IN @r_safra
    AND  d~cultura_apl IN @it_cultura_sf
    AND  f~lifsp   NE '12'
  GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  d~safra_apl,  b~kunnr, d~cultura_apl.


   "Verifica se safra foi preenchida, filtrar por safra / USER STORY 106316 / AOENNING.
   FREE: it_dados_aux[].
   SELECT   b~vbeln,
            a~posnr,
            a~matnr,
            a~werks,
            a~kwmeng,
            a~vrkme,
*            e~safra,
            b~kunnr,              "CS2019001220 - Sara Oikawa - Jun/2020
*            e~cultura,
            d~doc_simulacao,
            d~matnrv
    FROM vbap AS  a
    INNER JOIN vbak AS b ON b~vbeln = a~vbeln
    INNER JOIN vbep AS f ON f~vbeln = b~vbeln  AND f~posnr = a~posnr
    INNER JOIN zsdt0090 AS d ON d~vbeln = f~vbeln
    INNER JOIN zsdt0040 AS e ON e~doc_simulacao = d~doc_simulacao
    INNER JOIN mara  AS  c ON c~matnr = a~matnr
   APPENDING CORRESPONDING FIELDS OF TABLE @it_dados_aux
  WHERE  a~werks   IN @it_werks
    AND  b~spart   EQ '02'
    AND  b~auart   NOT IN  @it_auart
    AND  c~mtart   EQ 'ZHAW'
*    AND  e~safra   IN @r_safra
*    AND  e~cultura IN @it_cultura_sf
    AND  f~lifsp   NE '12'
  GROUP BY  b~vbeln, a~posnr, a~matnr, a~werks, a~kwmeng, a~vrkme,  e~safra, b~kunnr, e~cultura, d~doc_simulacao, d~matnrv.

   SORT it_dados BY vbeln posnr.
   DELETE ADJACENT DUPLICATES FROM it_dados COMPARING vbeln posnr.

   FREE: lit_dados_sim.
   lit_dados_sim = it_dados_aux[].
   SORT lit_dados_sim BY doc_simulacao.
   DELETE ADJACENT DUPLICATES FROM lit_dados_sim COMPARING doc_simulacao.

   IF lit_dados_sim[] IS NOT INITIAL.
     FREE: tg_zsdt0090_proc.
     SELECT  doc_simulacao, vbeln ,posnn, vbelv, posnv, estorno, matnrv
        FROM zsdt0090 INTO TABLE @tg_zsdt0090_proc
        FOR ALL ENTRIES IN @lit_dados_sim
      WHERE doc_simulacao EQ @lit_dados_sim-doc_simulacao.

     IF tg_zsdt0090_proc[] IS NOT INITIAL.
       FREE: tg_zsdt0041_proc.
       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 INTO TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbelv.

       SELECT doc_simulacao, vbeln , safra_apl, matnr, cultura_apl
          FROM zsdt0041 APPENDING TABLE @tg_zsdt0041_proc
          FOR ALL ENTRIES IN @tg_zsdt0090_proc
        WHERE vbeln EQ @tg_zsdt0090_proc-vbeln.

       IF tg_zsdt0041_proc[] IS NOT INITIAL.

         FREE: tg_zsdt0040_proc.
         SELECT doc_simulacao , safra
          FROM zsdt0040 INTO TABLE @tg_zsdt0040_proc
          FOR ALL ENTRIES IN @tg_zsdt0041_proc
        WHERE doc_simulacao EQ @tg_zsdt0041_proc-doc_simulacao.

       ENDIF.

       SORT tg_zsdt0041_proc.
       DELETE ADJACENT DUPLICATES FROM tg_zsdt0041_proc COMPARING ALL FIELDS.

     ENDIF.
   ENDIF.

   "Verifica se safra foi preenchida, filtrar por safra / USER STORY 106316 / AOENNING.
   IF r_safra[] IS NOT INITIAL.

     DELETE tg_zsdt0090_proc WHERE estorno EQ abap_true.
     DELETE tg_zsdt0090_proc WHERE vbeln   EQ space AND posnn EQ space.
     DELETE tg_zsdt0090_proc WHERE vbelv   EQ space OR posnv EQ space.

     SORT: tg_zsdt0090_proc BY vbeln posnn matnrv,
           tg_zsdt0041_proc BY vbeln matnr,
           tg_zsdt0040_proc BY doc_simulacao,
           it_dados_aux[]  BY vbeln posnr.

     CLEAR: _key_simulador_busca, lv_key_sim_old, l_count.

     LOOP AT it_dados_aux ASSIGNING <ls_dados>.
       lv_key_sim_old = <ls_dados>-vbeln && <ls_dados>-posnr.
       l_count = 0.

       WHILE lv_key_sim_old IS NOT INITIAL.

         ADD 1 TO l_count.
         _key_simulador_busca = lv_key_sim_old.

         PERFORM f_get_safra_zsdt0090 USING _key_simulador_busca <ls_dados>-matnrv
                                   CHANGING lv_key_sim_old
                                            ws_zsdt0041.

         IF ws_zsdt0041-vbeln IS NOT INITIAL.
           <ls_dados>-safra   = ws_zsdt0041-safra_apl.
           <ls_dados>-cultura = ws_zsdt0041-cultura_apl.

           IF <ls_dados>-safra EQ 0.
             READ TABLE tg_zsdt0040_proc ASSIGNING <fs_zsdt0040_proc> WITH KEY  doc_simulacao = ws_zsdt0041-doc_simulacao BINARY SEARCH.
             IF sy-subrc EQ 0.
               <ls_dados>-safra = <fs_zsdt0040_proc>-safra.
             ENDIF.
           ENDIF.
           EXIT.
         ENDIF.
       ENDWHILE.
     ENDLOOP.

     IF it_dados_aux[] IS NOT INITIAL.

       SORT it_dados_aux[] BY safra.
       DELETE it_dados_aux[] WHERE safra   NOT IN r_safra.
       DELETE it_dados_aux[] WHERE cultura NOT IN it_cultura_sf.

       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados[].
     ENDIF.

   ELSE.
     IF it_dados_aux[] IS NOT INITIAL.
       SORT it_dados_aux[] BY vbeln posnr.
       DELETE ADJACENT DUPLICATES FROM it_dados_aux[] COMPARING vbeln posnr.

       APPEND LINES OF it_dados_aux[] TO it_dados[].
     ENDIF.
   ENDIF.

   IF it_dados[] IS NOT INITIAL.
     MOVE it_dados TO it_safrinha.
   ENDIF.

   PERFORM z_safrinha_pr.
   REFRESH: it_safra, it_safrinha, it_zsdt0192, it_dados,  it_dados_aux.

 ENDFORM.
*    &---------------------------------------------------------------------*
*    &      Form  Z_SAFRA_PR
*    &---------------------------------------------------------------------*
*           text
*    ----------------------------------------------------------------------*
*      -->  p1        text
*      <--  p2        text
*    ----------------------------------------------------------------------*
 FORM  z_safra_pr.

   REFRESH it_zsdt0192.

   SELECT *
     FROM vbfa
     INTO TABLE @DATA(it_vbfa)
     FOR ALL ENTRIES IN @it_safra
    WHERE  vbelv EQ @it_safra-vbeln
     AND   posnv EQ @it_safra-posnr
     AND   vbtyp_n IN ('R','h')
     AND   vbtyp_v EQ 'C'.

   LOOP AT it_safra INTO wa_safra.

     LOOP AT it_vbfa INTO DATA(wa_vbfa)
         WHERE vbelv = wa_safra-vbeln AND
               posnv = wa_safra-posnr.

       IF wa_vbfa-vbtyp_n = 'R'.
         IF wa_vbfa-bwart EQ '601'.
           wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue +  wa_vbfa-rfmng.
           wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue +  wa_vbfa-rfmng.           "CS2019001220 - Sara Oikawa - Jun/2020
         ELSEIF wa_vbfa-bwart EQ '651'.
           wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue - wa_vbfa-rfmng.
           wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue - wa_vbfa-rfmng.            "CS2019001220 - Sara Oikawa - Jun/2020
           tot_devol = tot_devol + wa_vbfa-rfmng.
           tot_devol_ov = tot_devol_ov + wa_vbfa-rfmng.                                          "CS2019001220 - Sara Oikawa - Jun/2020
         ENDIF.
*IR176371  - PQ
       ELSEIF wa_vbfa-vbtyp_n = 'h'.
         wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue + wa_vbfa-rfmng.
         wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue + wa_vbfa-rfmng.              "CS2019001220 - Sara Oikawa - Jun/2020
         tot_devol = tot_devol - wa_vbfa-rfmng.
         tot_devol_ov = tot_devol_ov - wa_vbfa-rfmng.                                            "CS2019001220 - Sara Oikawa - Jun/2020
*IR176371  - PQ
       ENDIF.

     ENDLOOP.

     wa_prod_rev-saldo_entregar = ( wa_safra-kwmeng - tot_devol ) - wa_prod_rev-qtd_entregue.
     wa_prod_rev-vrkme = wa_safra-vrkme.
     wa_prod_rev-matnr = wa_safra-matnr.
     wa_prod_rev-safra = wa_safra-safra.
     wa_prod_rev-werks = wa_safra-werks.

     COLLECT  wa_prod_rev INTO it_prod_rev.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
     wa_prod_rev_ov-saldo_entregar = ( wa_safra-kwmeng - tot_devol_ov ) - wa_prod_rev_ov-qtd_entregue.
     wa_prod_rev_ov-vbeln = wa_safra-vbeln.
     wa_prod_rev_ov-posnr = wa_safra-posnr.
     wa_prod_rev_ov-vrkme = wa_safra-vrkme.
     wa_prod_rev_ov-matnr = wa_safra-matnr.
     wa_prod_rev_ov-safra = wa_safra-safra.
     wa_prod_rev_ov-werks = wa_safra-werks.

     COLLECT  wa_prod_rev_ov INTO it_prod_rev_ov.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

     CLEAR: wa_prod_rev, wa_safra, wa_vbfa, tot_devol.

     CLEAR: wa_prod_rev_ov, tot_devol_ov.

   ENDLOOP.

   LOOP AT it_prod_rev INTO wa_prod_rev.
*     READ TABLE IT_SAFRA INTO WA_SAFRA WITH KEY MATNR = WA_PROD_REV-MATNR.
     wa_zsdt0192-mandt         = sy-mandt.
     wa_zsdt0192-data          = sy-datum.
     wa_zsdt0192-tipo          = 'V'.
     wa_zsdt0192-matnr         = wa_prod_rev-matnr.
     wa_zsdt0192-werks         = wa_prod_rev-werks.
     wa_zsdt0192-safra         = wa_prod_rev-safra.
     wa_zsdt0192-tp_safra      = '1'.
     wa_zsdt0192-qtd_t         = wa_prod_rev-qtd_entregue + wa_prod_rev-saldo_entregar.
     wa_zsdt0192-qtd_u         = wa_prod_rev-qtd_entregue.
     wa_zsdt0192-qtd_n         = wa_prod_rev-saldo_entregar.
     wa_zsdt0192-um_ov         = wa_prod_rev-vrkme.
     wa_zsdt0192-usnam         = sy-uname.
     wa_zsdt0192-data_atual    = sy-datum.
     wa_zsdt0192-hora_atual    = sy-uzeit.

     APPEND wa_zsdt0192 TO it_zsdt0192.
     CLEAR: wa_prod_rev, wa_safra, wa_zsdt0192.
   ENDLOOP.

   MODIFY zsdt0192 FROM TABLE it_zsdt0192.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
* Armazena OV´s dos Produtos Revenda
   LOOP AT it_prod_rev_ov INTO wa_prod_rev_ov.

     READ TABLE it_safra INTO wa_safra WITH KEY  vbeln = wa_prod_rev_ov-vbeln
                                                 posnr = wa_prod_rev_ov-posnr
                                                 matnr = wa_prod_rev_ov-matnr
                                                 werks = wa_prod_rev_ov-werks
                                                 safra = wa_prod_rev_ov-safra.
     IF sy-subrc IS INITIAL.
       wa_zsdt0256-mandt    = sy-mandt.
       wa_zsdt0256-data     = sy-datum.
       wa_zsdt0256-tipo     = 'V'.
       wa_zsdt0256-vbeln    = wa_prod_rev_ov-vbeln.
       wa_zsdt0256-posnr    = wa_prod_rev_ov-posnr.

**********************************************************************
* PSA CONVERT MATNR 18
       "wa_zsdt0256-matnr    = |{ wa_prod_rev_ov-matnr ALPHA =  IN  }|.

       DATA gv_matcri18_06    TYPE matnr18.

*** Formata o código do material
       CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
           input  = wa_prod_rev_ov-matnr
         IMPORTING
           output = gv_matcri18_06.

       CLEAR: wa_zsdt0256-matnr.

       wa_zsdt0256-matnr = gv_matcri18_06.

       CLEAR: gv_matcri18_06.

* END CONVERT
**********************************************************************


       wa_zsdt0256-werks    = wa_prod_rev_ov-werks.
       wa_zsdt0256-safra    = wa_prod_rev_ov-safra.

       wa_zsdt0256-matnr_mp = wa_zsdt0256-matnr.

       wa_zsdt0256-tp_safra = '1'.

       wa_zsdt0256-kunnr    = wa_safra-kunnr.
       wa_zsdt0256-cultura  = wa_safra-cultura.

       wa_zsdt0256-fator_mp = 0.
       wa_zsdt0256-qtd_ov   = wa_safra-kwmeng.
       wa_zsdt0256-qtd_sald = wa_prod_rev_ov-saldo_entregar.

       wa_zsdt0256-qtd_cons = wa_prod_rev_ov-qtd_entregue.
       wa_zsdt0256-qtd_ac   = wa_prod_rev_ov-saldo_entregar.

       wa_zsdt0256-um_ov    = wa_safra-vrkme.

       wa_zsdt0256-usnam       = sy-uname.
       wa_zsdt0256-data_atual  = sy-datum.
       wa_zsdt0256-hora_atual  = sy-uzeit.

       APPEND wa_zsdt0256 TO it_zsdt0256.
       CLEAR: wa_prod_rev_ov, wa_safra, wa_zsdt0256.
     ENDIF.
   ENDLOOP.

   MODIFY zsdt0256 FROM TABLE it_zsdt0256.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

 ENDFORM.
*    &---------------------------------------------------------------------*
*    &      Form  Z_SAFRINHA_PR
*    &---------------------------------------------------------------------*
*           text
*    ----------------------------------------------------------------------*
*      -->  p1        text
*      <--  p2        text
*    ----------------------------------------------------------------------*
 FORM z_safrinha_pr.

   REFRESH: it_zsdt0192, it_prod_rev.

   SELECT *
     FROM vbfa
     INTO TABLE @DATA(it_vbfa)
     FOR ALL ENTRIES IN @it_safrinha
    WHERE  vbelv EQ @it_safrinha-vbeln
     AND   posnv EQ @it_safrinha-posnr
     AND   vbtyp_n IN ('R','h')
     AND   vbtyp_v EQ 'C'.

   LOOP AT it_safrinha INTO wa_safrinha.

     LOOP AT it_vbfa INTO DATA(wa_vbfa)
         WHERE vbelv = wa_safrinha-vbeln AND
               posnv = wa_safrinha-posnr.

       IF wa_vbfa-vbtyp_n = 'R'.
         IF wa_vbfa-bwart EQ '601'.
           wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue +  wa_vbfa-rfmng.
           wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue +  wa_vbfa-rfmng.           "CS2019001220 - Sara Oikawa - Jun/2020
         ELSEIF wa_vbfa-bwart EQ '651'.
           wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue - wa_vbfa-rfmng.
           wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue - wa_vbfa-rfmng.            "CS2019001220 - Sara Oikawa - Jun/2020
           tot_devol = tot_devol + wa_vbfa-rfmng.
           tot_devol_ov = tot_devol_ov + wa_vbfa-rfmng.                                          "CS2019001220 - Sara Oikawa - Jun/2020
         ENDIF.
*IR176371  - PQ
       ELSEIF wa_vbfa-vbtyp_n = 'h'.
         wa_prod_rev-qtd_entregue = wa_prod_rev-qtd_entregue + wa_vbfa-rfmng.
         wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue + wa_vbfa-rfmng.              "CS2019001220 - Sara Oikawa - Jun/2020
         tot_devol = tot_devol - wa_vbfa-rfmng.
*         wa_prod_rev_ov-qtd_entregue = wa_prod_rev_ov-qtd_entregue - wa_vbfa-rfmng.              "CS2019001220 - Sara Oikawa - Jun/2020
         tot_devol_ov = tot_devol_ov - wa_vbfa-rfmng.
*IR176371  - PQ
       ENDIF.
     ENDLOOP.

     wa_prod_rev-saldo_entregar = ( wa_safrinha-kwmeng - tot_devol ) - wa_prod_rev-qtd_entregue.
     wa_prod_rev-vrkme    = wa_safrinha-vrkme.
     wa_prod_rev-matnr    = wa_safrinha-matnr.
     wa_prod_rev-safra    = wa_safrinha-safra.
     wa_prod_rev-werks    = wa_safrinha-werks.

     COLLECT  wa_prod_rev INTO it_prod_rev.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
     wa_prod_rev_ov-saldo_entregar = ( wa_safrinha-kwmeng - tot_devol_ov ) - wa_prod_rev_ov-qtd_entregue.
     wa_prod_rev_ov-vbeln = wa_safrinha-vbeln.
     wa_prod_rev_ov-posnr = wa_safrinha-posnr.
     wa_prod_rev_ov-vrkme = wa_safrinha-vrkme.
     wa_prod_rev_ov-matnr = wa_safrinha-matnr.
     wa_prod_rev_ov-safra = wa_safrinha-safra.
     wa_prod_rev_ov-werks = wa_safrinha-werks.

     COLLECT  wa_prod_rev_ov INTO it_prod_rev_ov.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

     CLEAR: wa_prod_rev, wa_safrinha, wa_vbfa, tot_devol.

     CLEAR: wa_prod_rev_ov, tot_devol_ov.
   ENDLOOP.


   LOOP AT it_prod_rev INTO wa_prod_rev.
*     READ TABLE IT_SAFRINHA INTO WA_SAFRINHA WITH KEY MATNR = WA_PROD_REV-MATNR.
     wa_zsdt0192-mandt         = sy-mandt.
     wa_zsdt0192-data          = sy-datum.
     wa_zsdt0192-tipo          = 'V'.
     wa_zsdt0192-matnr         = wa_prod_rev-matnr.
     wa_zsdt0192-werks         = wa_prod_rev-werks.
     wa_zsdt0192-safra         = wa_prod_rev-safra.
     wa_zsdt0192-tp_safra      = '2'.
     wa_zsdt0192-qtd_t         = wa_prod_rev-qtd_entregue + wa_prod_rev-saldo_entregar.
     wa_zsdt0192-qtd_u         = wa_prod_rev-qtd_entregue.
     wa_zsdt0192-qtd_n         = wa_prod_rev-saldo_entregar.
     wa_zsdt0192-um_ov         = wa_prod_rev-vrkme.
     wa_zsdt0192-usnam         = sy-uname.
     wa_zsdt0192-data_atual    = sy-datum.
     wa_zsdt0192-hora_atual    = sy-uzeit.
     APPEND wa_zsdt0192 TO it_zsdt0192.
     CLEAR: wa_prod_rev, wa_safrinha, wa_zsdt0192.
   ENDLOOP.

   MODIFY zsdt0192 FROM TABLE it_zsdt0192.

* Início - CS2019001220 - Sara Oikawa - Jun/2020
* Armazena OV´s dos Produtos Revenda
   LOOP AT it_prod_rev_ov INTO wa_prod_rev_ov.

     READ TABLE it_safrinha INTO wa_safrinha WITH KEY  vbeln = wa_prod_rev_ov-vbeln
                                                       posnr = wa_prod_rev_ov-posnr
                                                       matnr = wa_prod_rev_ov-matnr
                                                       werks = wa_prod_rev_ov-werks
                                                       safra = wa_prod_rev_ov-safra.
     IF sy-subrc IS INITIAL.
       wa_zsdt0256-mandt    = sy-mandt.
       wa_zsdt0256-data     = sy-datum.
       wa_zsdt0256-tipo     = 'V'.
       wa_zsdt0256-vbeln    = wa_prod_rev_ov-vbeln.
       wa_zsdt0256-posnr    = wa_prod_rev_ov-posnr.


**********************************************************************
* PSA CONVERT MATNR 18
       "wa_zsdt0256-matnr    = |{ wa_prod_rev_ov-matnr ALPHA =  IN  }|.

       DATA gv_matcri18_07    TYPE matnr18.

*** Formata o código do material
       CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
           input  = wa_prod_rev_ov-matnr
         IMPORTING
           output = gv_matcri18_07.

       CLEAR: wa_zsdt0256-matnr.

       wa_zsdt0256-matnr = gv_matcri18_07.

       CLEAR: gv_matcri18_07.

* END CONVERT
**********************************************************************


       wa_zsdt0256-werks    = wa_prod_rev_ov-werks.
       wa_zsdt0256-safra    = wa_prod_rev_ov-safra.

       wa_zsdt0256-matnr_mp = wa_zsdt0256-matnr.

       wa_zsdt0256-tp_safra = '2'.

       wa_zsdt0256-kunnr    = wa_safrinha-kunnr.
       wa_zsdt0256-cultura  = wa_safrinha-cultura.

       wa_zsdt0256-fator_mp = 0.
       wa_zsdt0256-qtd_ov   = wa_safrinha-kwmeng.
       wa_zsdt0256-qtd_sald = wa_prod_rev_ov-saldo_entregar.

       wa_zsdt0256-qtd_cons = wa_prod_rev_ov-qtd_entregue.
       wa_zsdt0256-qtd_ac   = wa_prod_rev_ov-saldo_entregar.

       wa_zsdt0256-um_ov    = wa_safrinha-vrkme.

       wa_zsdt0256-usnam       = sy-uname.
       wa_zsdt0256-data_atual  = sy-datum.
       wa_zsdt0256-hora_atual  = sy-uzeit.

       APPEND wa_zsdt0256 TO it_zsdt0256.
       CLEAR: wa_prod_rev_ov, wa_safrinha, wa_zsdt0256.
     ENDIF.
   ENDLOOP.

   MODIFY zsdt0256 FROM TABLE it_zsdt0256.
* Fim - CS2019001220 - Sara Oikawa - Jun/2020

 ENDFORM.
*    &---------------------------------------------------------------------*
*    &      Form  Z_BUSCA_DADOS_PC
*    &---------------------------------------------------------------------*
*           text
*    ----------------------------------------------------------------------*
*      -->  p1        text
*      <--  p2        text
*    ----------------------------------------------------------------------*
 FORM z_busca_dados_pc.
   DATA: xtotal TYPE zsdt0192-qtd_n.
   DATA total TYPE zsdt0192-qtd_n.

   REFRESH: it_zsdt0192, it_zsdt0193.

   SELECT  b~ebeln,  a~ebelp,
           b~bsart,  b~aedat,
           a~matnr,  a~txz01,
           a~mtart,  a~bukrs,
           a~werks,  b~reswk,
           a~menge,  a~meins,
           a~elikz
     FROM ekpo AS a
     INNER JOIN ekko AS b ON a~ebeln = b~ebeln
     INTO TABLE @it_pedido_compra
   WHERE b~bsart IN @it_bsart
     AND b~aedat >= @vaedat
     AND a~werks IN @it_werks
     AND a~mtart IN ('ZHAW','ZROH')
     AND a~loekz NOT IN ('L', 'S')
     AND a~matkl IN ('658440', '700150', '658450', '200120', '609210').


   SELECT ebeln, ebelp, bewtp, menge, shkzg FROM ekbe INTO TABLE @DATA(it_ekbe)
     FOR ALL ENTRIES IN @it_pedido_compra
          WHERE ebeln EQ @it_pedido_compra-ebeln AND
                ebelp EQ @it_pedido_compra-ebelp AND
                bewtp EQ 'E'.

   MOVE-CORRESPONDING it_pedido_compra TO it_pdd_cmp_aux.

   SORT it_pdd_cmp_aux BY matnr.
   SORT it_pedido_compra BY matnr.

   DELETE ADJACENT DUPLICATES FROM it_pdd_cmp_aux COMPARING matnr.

   LOOP AT it_pdd_cmp_aux INTO wa_pdd_cmp_aux.

     LOOP AT it_pedido_compra INTO wa_pedido_compra
        WHERE matnr = wa_pdd_cmp_aux-matnr.


       wa_zsdt0192-mandt       = sy-mandt.
       wa_zsdt0192-data        = sy-datum.
       wa_zsdt0192-tipo        = 'C'.
       wa_zsdt0192-matnr       = wa_pedido_compra-matnr.
       wa_zsdt0192-werks       = wa_pedido_compra-werks.

       IF wa_pedido_compra-elikz NE 'X'.
         wa_zsdt0192-qtd_t       = wa_pedido_compra-menge.
       ELSE.
         LOOP AT it_ekbe INTO DATA(wa_ekbe) WHERE ebeln EQ wa_pedido_compra-ebeln AND
                                                  ebelp EQ wa_pedido_compra-ebelp.

           IF wa_ekbe-shkzg EQ 'S'.
             total = total + wa_ekbe-menge.
           ELSEIF wa_ekbe-shkzg EQ 'H'.
             total = total - wa_ekbe-menge.

             wa_zsdt0192-qtd_t       = total.

           ENDIF.
         ENDLOOP.
       ENDIF.

       wa_zsdt0192-um_ov       = wa_pedido_compra-meins.
       wa_zsdt0192-usnam       = sy-uname.
       wa_zsdt0192-data_atual  = sy-datum.
       wa_zsdt0192-hora_atual  = sy-uzeit.

       COLLECT wa_zsdt0192 INTO it_zsdt0192.

       wa_zsdt0193-mandt      = sy-mandt.
       wa_zsdt0193-ebeln      = wa_pedido_compra-ebeln.
       wa_zsdt0193-ebelp      = wa_pedido_compra-ebelp.
       wa_zsdt0193-matnr      = wa_pedido_compra-matnr.
       wa_zsdt0193-werks      = wa_pedido_compra-werks.
       wa_zsdt0193-usnam      = sy-uname.
       wa_zsdt0193-data_atual = sy-datum.
       wa_zsdt0193-hora_atual = sy-uzeit.

       COLLECT wa_zsdt0193 INTO it_zsdt0193.
       CLEAR:  wa_zsdt0192, wa_pedido_compra,  wa_zsdt0193.
     ENDLOOP.
     CLEAR wa_pdd_cmp_aux.
   ENDLOOP.

   REFRESH it_pdd_cmp_aux.
   CLEAR wa_pdd_cmp_aux.

   SELECT  b~ebeln,  a~ebelp,
           b~bsart,  b~aedat,
           a~matnr,  a~txz01,
           a~mtart,  a~bukrs,
           a~werks,  b~reswk,
           a~menge,  a~meins,
           a~elikz
  FROM ekpo AS a
  INNER JOIN ekko AS b ON a~ebeln = b~ebeln
  INTO  TABLE @DATA(it_pedido_compra_aux)
 WHERE b~bsart EQ 'ZUB'
   AND b~aedat >= @vaedat
   AND b~reswk IN @it_werks
   AND a~mtart IN ('ZHAW','ZROH')
   AND a~loekz NOT IN ('L', 'S')
   AND a~matkl IN ('658440', '700150', '658450', '200120', '609210').

   MOVE-CORRESPONDING it_pedido_compra_aux TO it_pdd_cmp_aux.

   SORT it_pdd_cmp_aux BY matnr.
   SORT it_pedido_compra_aux BY matnr.

   DELETE ADJACENT DUPLICATES FROM it_pdd_cmp_aux COMPARING matnr.

   SELECT ebeln, ebelp, bewtp, menge, shkzg FROM ekbe INTO TABLE @DATA(it_ekbe_aux)
 FOR ALL ENTRIES IN @it_pedido_compra_aux
      WHERE ebeln EQ @it_pedido_compra_aux-ebeln AND
            ebelp EQ @it_pedido_compra_aux-ebelp AND
            bewtp EQ 'E'.

   LOOP AT it_pdd_cmp_aux INTO wa_pdd_cmp_aux.

     LOOP AT it_pedido_compra_aux INTO DATA(wa_pedido_compra_aux)
       WHERE matnr = wa_pdd_cmp_aux-matnr.

       wa_zsdt0192-mandt       = sy-mandt.
       wa_zsdt0192-data        = sy-datum.
       wa_zsdt0192-tipo        = 'C'.
       wa_zsdt0192-matnr       = wa_pedido_compra_aux-matnr.
       wa_zsdt0192-werks       = wa_pedido_compra_aux-reswk.

       IF wa_pedido_compra_aux-elikz NE 'X'.
         wa_zsdt0192-qtd_t       = wa_pedido_compra_aux-menge * -1.
       ELSE.
         LOOP AT it_ekbe_aux INTO DATA(wa_ekbe_aux) WHERE ebeln EQ wa_pedido_compra_aux-ebeln AND
                                                           ebelp EQ wa_pedido_compra_aux-ebelp.

           IF wa_ekbe_aux-shkzg EQ 'S'.
             total = total + wa_ekbe_aux-menge.
           ELSEIF wa_ekbe_aux-shkzg EQ 'H'.
             total = total - wa_ekbe_aux-menge.

             wa_zsdt0192-qtd_t       = total * -1.

           ENDIF.
         ENDLOOP.
       ENDIF.

       wa_zsdt0192-um_ov       = wa_pedido_compra_aux-meins.
       wa_zsdt0192-usnam       = sy-uname.
       wa_zsdt0192-data_atual  = sy-datum.
       wa_zsdt0192-hora_atual  = sy-uzeit.

       COLLECT wa_zsdt0192 INTO it_zsdt0192.


       wa_zsdt0193-mandt      = sy-mandt.
       wa_zsdt0193-ebeln      = wa_pedido_compra_aux-ebeln.
       wa_zsdt0193-ebelp      = wa_pedido_compra_aux-ebelp.
       wa_zsdt0193-matnr      = wa_pedido_compra_aux-matnr.
       wa_zsdt0193-werks      = wa_pedido_compra_aux-reswk.
       wa_zsdt0193-tp_ped     = 'T'. "PEDIDO DE TRANSFERENCIA
       wa_zsdt0193-usnam      = sy-uname.
       wa_zsdt0193-data_atual = sy-datum.
       wa_zsdt0193-hora_atual = sy-uzeit.

       COLLECT wa_zsdt0193 INTO it_zsdt0193.
       CLEAR:  wa_zsdt0193, wa_pedido_compra_aux.

     ENDLOOP.
     CLEAR wa_pdd_cmp_aux.
   ENDLOOP.

   MODIFY zsdt0192 FROM TABLE it_zsdt0192.

   MODIFY zsdt0193 FROM TABLE it_zsdt0193.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_PEDIDOS_MOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM check_pedidos_mod .

****Selecionar dados.
   SELECT *
   FROM zsdt0193
   INTO TABLE @DATA(t_zsdt0193)
    WHERE tp_ped NE 'T'.

   LOOP AT   t_zsdt0193 ASSIGNING FIELD-SYMBOL(<w_zsdt0193>).
**** Check pedido.
     SELECT SINGLE *
     FROM ekpo
     INTO @DATA(w_ekpo)
        WHERE matnr EQ @<w_zsdt0193>-matnr
          AND ebeln EQ @<w_zsdt0193>-ebeln
          AND ebelp EQ @<w_zsdt0193>-ebelp
          AND werks EQ @<w_zsdt0193>-werks
          AND loekz NOT IN ('L', 'S').

     IF w_ekpo IS INITIAL.
       "Se não encontrar o pedido é porque houve alteração, ai o procedimento é eliminar esse item na tabela zsdt0193 e zsdt0194
       DELETE FROM  zsdt0193 WHERE matnr EQ @<w_zsdt0193>-matnr
                               AND ebeln EQ @<w_zsdt0193>-ebeln
                               AND ebelp EQ @<w_zsdt0193>-ebelp
                               AND werks EQ @<w_zsdt0193>-werks.

       DELETE FROM  zsdt0194 WHERE matnr EQ @<w_zsdt0193>-matnr
                               AND ebeln EQ @<w_zsdt0193>-ebeln
                               AND ebelp EQ @<w_zsdt0193>-ebelp
                               AND werks EQ @<w_zsdt0193>-werks.

       COMMIT WORK.

     ENDIF.
     CLEAR: w_ekpo.
   ENDLOOP.

   FREE: t_zsdt0193.
 ENDFORM.
*
 FORM f_get_safra_zsdt0090  USING p_key_sim_new TYPE char16
                                  p_matnr       TYPE matnr
                         CHANGING c_key_sim_old TYPE char16
                                  c_zsdt0041    TYPE ty_zsdt0041_proc.


   CLEAR: c_key_sim_old, c_zsdt0041.

   READ TABLE tg_zsdt0041_proc INTO c_zsdt0041 WITH KEY vbeln = p_key_sim_new+00(10) matnr = p_matnr.
   CHECK sy-subrc NE 0.

   READ TABLE tg_zsdt0090_proc ASSIGNING FIELD-SYMBOL(<fs_zsdt0090_proc>) WITH KEY vbeln = p_key_sim_new+00(10)
                                                                                   posnn = p_key_sim_new+10(06)." BINARY SEARCH.
   CHECK sy-subrc EQ 0.

   c_key_sim_old = <fs_zsdt0090_proc>-vbelv && <fs_zsdt0090_proc>-posnv.
   READ TABLE tg_zsdt0041_proc INTO c_zsdt0041 WITH KEY vbeln = <fs_zsdt0090_proc>-vbelv matnr =  <fs_zsdt0090_proc>-matnrv BINARY SEARCH.


 ENDFORM.
