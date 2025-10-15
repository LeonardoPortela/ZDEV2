FUNCTION-POOL zles0002                     MESSAGE-ID zles.

***********************************************************************
* TIPOS
***********************************************************************
TYPE-POOLS: icon, zlesi.

***********************************************************************
* CONSTANTES
***********************************************************************
CONSTANTS:
  cc_a                                     VALUE 'A',
  cc_b                                     VALUE 'B',
  cc_e                                     VALUE 'E',
  cc_i                                     VALUE 'I',
  cc_l                                     VALUE 'L',
  cc_s                                     VALUE 'S',
  cc_x                                     VALUE 'X',
  cc_1                                     VALUE '1',
  cc_2                                     VALUE '2',
  cc_3                                     VALUE '3',
  cc_importado                             VALUE 'I',
  cc_confirmado                            VALUE 'C',
  cc_a_confirmar                           VALUE 'A',
  cc_existe_acdc                           VALUE '1',
  cc_aplcdo_acdc                           VALUE '3',
  cc_kappl                 TYPE c          VALUE 'F',
  cc_eq(2)                 TYPE c          VALUE 'EQ',
  cc_cp(2)                 TYPE c          VALUE 'CP',
* ---> S4 Migration - 07/07/2023 - GB - Inicio
*  cc_refty_8               TYPE vbtyp      VALUE '8',
  cc_refty_8               TYPE vbtypl      VALUE '8',
* <--- S4 Migration - 07/07/2023 - GB - Fim
  cc_unid_peso_default     TYPE gewei      VALUE 'KG',
  cc_fkpty_z001            TYPE fkpty      VALUE 'Z001',
  cc_vbtypv_j              TYPE vbtypl     VALUE 'J',
  cc_vbtypn_m              TYPE vbtypl     VALUE 'M',
  cc_vbtypn_r              TYPE vbtypl     VALUE 'R',
  cc_add02_03              TYPE vttk_add02 VALUE '0000000003',
  cc_reftyp_bi             TYPE j_1breftyp VALUE 'BI',
  cc_reftyp_md             TYPE j_1breftyp VALUE 'MD',
  cc_idinter_l1            TYPE zidinter   VALUE 'L1',
  cc_tp_reg_30             TYPE ztp_reg    VALUE '30',
  cc_kschl_zmrg            TYPE kscha      VALUE 'ZMRG',
  cc_kschl_zadm            TYPE kscha      VALUE 'ZADM',
  cc_kschl_ziof            TYPE kscha      VALUE 'ZIOF',
  cc_kschl_zfre            TYPE kscha      VALUE 'ZFRE',
  cc_kschl_zseg            TYPE kscha      VALUE 'ZSEG'.

***********************************************************************
* Ranges
***********************************************************************
RANGES: rc_kschl           FOR konv-kschl       OCCURS 15,
        rc_stat_013        FOR zlest0013-status OCCURS 0,
        rc_stat_015        FOR zlest0015-status OCCURS 0.

***********************************************************************
* Ponteiros
***********************************************************************
DATA: vg_dataref_rtransp           TYPE REF TO data,
      vg_dataref_rposto            TYPE REF TO data,
      vg_dataref_lote              TYPE REF TO data,
      vg_dias                      TYPE c LENGTH 2.

* Parâmetros da função
FIELD-SYMBOLS: <rc_transportador>  TYPE ANY TABLE,
               <rc_posto>          TYPE ANY TABLE,
               <rc_lote>           TYPE ANY TABLE,
               <rc_conhecimento>   TYPE ANY TABLE,
               <rc_carta_frete>    TYPE ANY TABLE,
               <rc_periodo>        TYPE ANY TABLE,
               <rc_fechamento>     TYPE ANY TABLE,
               <rc_vencimento>     TYPE ANY TABLE,
               <rc_status_lote>    TYPE ANY TABLE,
               <ti_lotes>          TYPE STANDARD TABLE,
               <ti_lanctos>        TYPE STANDARD TABLE,
               <ti_deltas>         TYPE STANDARD TABLE,
               <ti_confer>         TYPE STANDARD TABLE,
               <ti_acrdcr>         TYPE STANDARD TABLE,
               <ti_msg>            TYPE STANDARD TABLE,
               <vc_msg_erro>       TYPE c.

TYPES: BEGIN OF ty_zlest0013,
         cnpj_trp                  TYPE stcd1,
         cnpj_posto                TYPE stcd1,
         lote                      TYPE char10,
         ctafrete                  TYPE zctafrete,
         conhec                    TYPE zconhec,
         chvid                     TYPE zchvid,
         codtrp                    TYPE zcodtrp,
         codposto                  TYPE zcodposto,
         datalote                  TYPE zdatalote,
         vlrlote                   TYPE zvlrlote,
         vlrconhec                 TYPE zvlrconhec,
         qtde                      TYPE zqtde1,
         dtacheg                   TYPE zdtacheg,
         status                    TYPE ztatuslote,
       END   OF ty_zlest0013,

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

       BEGIN OF ty_transposto,
         lifnr                     TYPE lifnr,
         cnpj    	                 TYPE stcd1,
         name1                     TYPE name1_gp,
       END   OF ty_transposto,

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
         kinak                     TYPE kinak,
       END   OF ty_konv,

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

       BEGIN OF ty_zsdt0001,
         ch_referencia             TYPE zch_ref,
         tp_movimento              TYPE ztp_mov,
         nr_romaneio               TYPE znr_romaneio,
         vbeln                     TYPE vbeln,
         dt_movimento              TYPE zdt_mov,
         nr_safra                  TYPE znr_safra,
         bukrs                     TYPE bukrs,
         branch                    TYPE j_1bbranc_,
         parid                     TYPE zparid,
         id_cli_dest               TYPE zid_cli_dest,
         tp_frete                  TYPE ztp_frete,
         matnr                     TYPE matnr,
         peso_liq                  TYPE ntgew,
         peso_fiscal               TYPE brgew,
         nfnum                     TYPE znfnum,
         series                    TYPE j_1bseries,
         docdat                    TYPE j_1bdocdat,
         netwr                     TYPE znetwr,
         nfe                       TYPE znfe,
         doc_rem                   TYPE zdoc_rem,
         id_interface              TYPE zid_interf,
       END   OF ty_zsdt0001,

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

       BEGIN OF ty_lanctoconfer,
         codtrp                    TYPE zcodtrp,
         codposto                  TYPE zcodposto,
         lote                      TYPE char10,
         chvid                     TYPE zchvid,
         conhec                    TYPE exti1,
       END   OF ty_lanctoconfer,

       BEGIN OF ty_docacrdcr,
         bukrs                     TYPE bukrs,
         belnr                     TYPE belnr_d,
         gjahr                     TYPE gjahr,
         wrbtr                     TYPE wrbtr,
         sgtxt                     TYPE sgtxt,
         budat                     TYPE budat,
         waers                     TYPE waers,
       END   OF ty_docacrdcr.

***********************************************************************
* TABELAS Internas
***********************************************************************
DATA: ti_zlest0008           TYPE STANDARD TABLE OF zlest0008
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0015           TYPE STANDARD TABLE OF zlest0015
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0016           TYPE STANDARD TABLE OF zlest0016
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0020           TYPE STANDARD TABLE OF zlest0020
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0025           TYPE STANDARD TABLE OF zlest0025
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zlest0013           TYPE STANDARD TABLE OF ty_zlest0013
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_conhec              TYPE STANDARD TABLE OF ty_conhecimento
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_transporte          TYPE STANDARD TABLE OF ty_transporte
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_transposto          TYPE STANDARD TABLE OF ty_transposto
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_lctoconfer          TYPE STANDARD TABLE OF ty_lanctoconfer
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_docacrdcr           TYPE STANDARD TABLE OF ty_docacrdcr
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_remessa             TYPE STANDARD TABLE OF ty_remessa
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_vfkp                TYPE STANDARD TABLE OF ty_vfkp
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_konv                TYPE STANDARD TABLE OF ty_konv
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
      ti_tolerancia          TYPE STANDARD TABLE OF ty_tolerancia
                                  WITH HEADER LINE INITIAL SIZE 0,
      ti_zsdt0001            TYPE STANDARD TABLE OF ty_zsdt0001
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
      ti_margtoler           TYPE STANDARD TABLE OF ty_margtoler
                                  WITH HEADER LINE INITIAL SIZE 0.

***********************************************************************
* VARIÁVEIS
***********************************************************************
DATA: w_lote                TYPE zles_cockpit_lote,
      w_lancto              TYPE zles_cockpit_lancto,
      w_delta               TYPE zles_cockpit_delta,
      w_confer              TYPE zles_cockpit_confer,
      w_acrdcr              TYPE zles_cockpit_acrescdecres,
      w_qbr_conhec          TYPE ty_conhecimento,
      w_old_conhec          TYPE ty_conhecimento,
      w_docheader           TYPE j_1bnfdoc,
      w_ext_header          TYPE j_1bindoc.
