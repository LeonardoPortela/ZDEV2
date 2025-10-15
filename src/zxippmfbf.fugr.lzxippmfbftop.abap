FUNCTION-POOL zxippmfbf                    MESSAGE-ID z01.

***********************************************************************
* CONSTANTES
***********************************************************************
CONSTANTS:
  cc_a                                     VALUE 'A',
  cc_c                                     VALUE 'C',
  cc_e                                     VALUE 'E',
  cc_i                                     VALUE 'I',
  cc_s                                     VALUE 'S',
  cc_n                                     VALUE 'N',
  cc_x                                     VALUE 'X',
  cc_w                                     VALUE 'W',
  cc_16                    TYPE i          VALUE 16,
  cc_eq(2)                 TYPE c          VALUE 'EQ',
  cc_ge(2)                 TYPE c          VALUE 'GE',

  cc_classe_mensagem       TYPE arbgb      VALUE 'Z01',
  cc_tpexec_01(2)          TYPE c          VALUE '01',
  cc_tpexec_02(2)          TYPE c          VALUE '02',
  cc_mfbf                  TYPE sy-tcode   VALUE 'MFBF',
  cc_pdc_number            TYPE sa_bdenr   VALUE '15',
  cc_deposito_pr01         TYPE alort      VALUE 'PR01',
  cc_deposito_in01         TYPE alort      VALUE 'IN01',
  cc_bwart_131             TYPE bwart      VALUE '131',
  cc_bwart_261             TYPE bwart      VALUE '261',
  cc_bwart_262             TYPE bwart      VALUE '262',
  cc_tipo_conf_01          TYPE bckfltype  VALUE '01',
  cc_tipo_conf_11          TYPE bckfltype  VALUE '11'.

***********************************************************************
* TIPOS
***********************************************************************
TYPES: BEGIN OF ty_execretrn,
         type              TYPE bapi_mtype,
         id                TYPE symsgid,
         number            TYPE symsgno,
         message           TYPE bapi_msg,
         message_v1        TYPE symsgv,
         message_v2        TYPE symsgv,
         message_v3        TYPE symsgv,
         message_v4        TYPE symsgv,
       END    OF ty_execretrn.

***********************************************************************
* TABELAS Internas
***********************************************************************
DATA: yt_execretrn         TYPE TABLE OF ty_execretrn
                           WITH HEADER LINE INITIAL SIZE 0,
      yt_log_mfpf          TYPE TABLE OF zfie_ret_document
                           WITH HEADER LINE INITIAL SIZE 0,
      yt_geo_oubound_ok    TYPE TABLE OF zmme_return_sucess
                           WITH HEADER LINE INITIAL SIZE 0,
      yt_geo_oubound_erro  TYPE TABLE OF zmme_return_error
                           WITH HEADER LINE INITIAL SIZE 0,
      yt_goodsmovements    TYPE TABLE OF bapi2017_gm_item_create
                           WITH HEADER LINE INITIAL SIZE 0,
      yt_return            TYPE TABLE OF bapiret2
                           WITH HEADER LINE INITIAL SIZE 0,
      ti_bdc           type table of bdcdata,
      ti_msg           type table of bdcmsgcoll.

***********************************************************************
* VARIÁVEIS
***********************************************************************
DATA: w_xi_mfbf            TYPE zpps_ximfbf,
      w_bflushflags        TYPE bapi_rm_flg,
      w_bflushdatagen      TYPE bapi_rm_datgen,
      w_bapiret2           TYPE bapiret2,
      wa_bdc               type bdcdata,
      wa_msg               type bdcmsgcoll.

DATA: vc_confirmation      TYPE prtnr,
      vc_cancconfirm       TYPE canc_prtnr,
      vc_ult_doc_gerado    TYPE mblnr,
      vc_dt_lacto_gerado   TYPE d,
      vc_ano_doc_contabil  TYPE mjahr,
      vc_empr_doc_gerado   TYPE bukrs,
      vc_aufnr_pai         TYPE aufnr,
      vc_matnr_pai         TYPE matnr,
      vc_werks_pai         TYPE werks_d,
      vc_lgort_pai         TYPE lgort_d,
      vc_verid_pai         TYPE verid,
      vf_quebra,
      vf_muda_bol,
      vf_erro.
