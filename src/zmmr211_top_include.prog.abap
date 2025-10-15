*&---------------------------------------------------------------------*
*& Include          ZMMR211_TOP_INCLUDE
*&---------------------------------------------------------------------*
TABLES: zsdt0001.

TYPES: BEGIN OF ty_saida,

         bukrs        TYPE zsdt0001-bukrs,
         branch       TYPE zsdt0001-branch,
         dt_movimento TYPE zsdt0001-dt_movimento,
         docdat       TYPE zsdt0001-docdat,
         parid        TYPE zsdt0001-parid,
         name1        TYPE lfa1-name1,
         chave_nfe    TYPE zsdt0001-chave_nfe,
         tpinsumo     TYPE c LENGTH 40,
         stpedido     TYPE c LENGTH 40,
         stmatnr      TYPE c LENGTH 40,
         stmatnr2     TYPE c LENGTH 40,
         xmlreceb     TYPE char10,
         nfnum        TYPE zsdt0001-nfnum,
         nr_romaneio  TYPE zsdt0001-nr_romaneio,
         nr_safra     TYPE zsdt0001-nr_safra,
         placa_cav    TYPE zsdt0001-placa_cav,
         peso_liq     TYPE zsdt0001-peso_liq,
         pedatrib     TYPE ekko-ebeln,
         matnr        TYPE mara-matnr,
         maktx        TYPE makt-maktx,
         mblnr        TYPE mblnr,
         belnr_ft     TYPE zib_nfe_dist_itm-belnr_ft,
         cpudt        TYPE mkpf-cpudt,
         cpudt_rb     TYPE rbkp-cpudt,
         lgort        TYPE mseg-lgort,

       END OF ty_saida.
