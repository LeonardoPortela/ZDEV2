*&---------------------------------------------------------------------*
*& Report  ZGL036
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zgl036.

TYPES: BEGIN OF ty_zmmt0024.
         INCLUDE STRUCTURE zmmt0024.
TYPES:   perc TYPE zrc_perc2,
         qtde TYPE i,
       END OF ty_zmmt0024.
************************************************************************
* Tabelas Internas
************************************************************************
DATA: BEGIN OF requisition_items OCCURS 10.
        INCLUDE STRUCTURE bapiebanc.
DATA: END OF requisition_items.

DATA: BEGIN OF requisition_account_assignment OCCURS 10.
        INCLUDE STRUCTURE bapiebkn.
DATA: END OF requisition_account_assignment.

DATA: BEGIN OF extension OCCURS 10.
        INCLUDE STRUCTURE bapiparex.
DATA: END OF extension.

DATA: BEGIN OF requisition_services	OCCURS 10.
        INCLUDE STRUCTURE bapiesllc.
DATA: END OF requisition_services.

DATA: BEGIN OF requisition_srv_accass_values  OCCURS 10.
        INCLUDE STRUCTURE bapiesklc.
DATA: END OF requisition_srv_accass_values.

DATA: BEGIN OF return OCCURS 10.
        INCLUDE STRUCTURE bapireturn.
DATA: END OF return.

DATA: BEGIN OF return_po OCCURS 10.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF return_po.

DATA:
  it_zmmt0024       TYPE TABLE OF ty_zmmt0024,
  it_zmmt0024_tot   TYPE TABLE OF ty_zmmt0024,
  it_zmmt0024_obj   TYPE TABLE OF ty_zmmt0024,
  it_zmmt0024_tot_2 TYPE TABLE OF ty_zmmt0024,
  it_mmt24_pg       TYPE TABLE OF ty_zmmt0024 WITH HEADER LINE,
  it_mmt24_pg_tot   TYPE TABLE OF ty_zmmt0024 WITH HEADER LINE,
  t_zmm_aprov_rcc   TYPE zmm_aprov_rcc,
  ti_zlest0100      TYPE TABLE OF zlest0100  WITH HEADER LINE,
  tg_zglt081        TYPE TABLE OF zglt081 WITH HEADER LINE,
  tg_zglt080        TYPE TABLE OF zglt080 WITH HEADER LINE,
  wa_zmmt0024       TYPE ty_zmmt0024,
  wa_zmmt0024_tot   TYPE ty_zmmt0024,
  wa_zmmt0024_obj   TYPE ty_zmmt0024,
  wa_zmmt0024_tot_2 TYPE ty_zmmt0024,
  wa_lfa1_forn      TYPE lfa1,
  wa_lfa1           TYPE lfa1,
  wa_zlest0100      TYPE zlest0100,

  wa_bapiebanc      TYPE STANDARD TABLE OF bapiebanc WITH HEADER LINE,
  wa_bapiebkn       TYPE STANDARD TABLE OF bapiebkn  WITH HEADER LINE,
  wa_req_services   TYPE bapiesllc,
  wa_req_serv_rat   TYPE bapiesklc,

  it_outreturn      TYPE TABLE OF zfie_ret_document,
  wa_outreturn      TYPE zfie_ret_document,

  wa_return         TYPE bapireturn,
  vg_index          TYPE sy-tabix,
  vtype             TYPE bapireturn-type,

  v_lifnr           TYPE lfa1-lifnr,
  v_preq_item       TYPE bapiebanc-preq_item,
  x_header          TYPE thead,
  it_lines          TYPE STANDARD TABLE OF tline WITH HEADER LINE,
  wa_lines          LIKE LINE OF it_lines,
  wl_texto          TYPE LINE OF catsxt_longtext_itab,
  zid               TYPE thead-tdid,
  zname             TYPE thead-tdname,
  wa_mara           TYPE mara,
  wa_makt           TYPE makt,
  wa_asmd           TYPE asmd,
  wa_asmdt          TYPE asmdt,
  wa_t001w          TYPE t001w,
  lc_mat_serv       TYPE c LENGTH 1,
  lc_spras          TYPE spras.

DATA: w_number     LIKE  bapiebanc-preq_no,
      vg_brtwr     TYPE  zmmt0024-brtwr,
      vserial      TYPE  dzebkn,
      vl_ponteiro  TYPE zlest0100-cont,
      ca_brtwr(16).

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  c_e   TYPE c VALUE 'I',
  c_x   TYPE c VALUE 'X',
  c_mm  TYPE zfie_ret_document-id         VALUE 'MM',
  c_899 TYPE zfie_ret_document-num        VALUE '899',
  c_29  TYPE zfie_ret_document-interface  VALUE '29',
  c_40  TYPE zfie_ret_document-interface  VALUE '40',
  c_41  TYPE zfie_ret_document-interface  VALUE '41'.

DATA vmsg(50).

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_objkey TYPE zmmt0024-objkey .
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.

  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'ZGL036_JOB'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).
    PERFORM: z_processa_pgto,          " Lançamentos de Pgtos ZGL059
             z_envia_log_legado.       " Envia retorno de msg para o legado
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
FORM z_prepara_mensagem USING pobj_key
                              ptype
                              interface
                              pmessage
                              pmessage_v1 .

  CLEAR wa_outreturn.

  wa_outreturn-obj_key        = pobj_key.
  wa_outreturn-interface      = interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = ptype.
  wa_outreturn-id             = c_mm.
  wa_outreturn-num            = c_899.
  wa_outreturn-message        = pmessage.
  wa_outreturn-message_v1     = pmessage_v1.

  APPEND wa_outreturn TO it_outreturn.


ENDFORM.                    "z_prepara_mensagem

*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*       Envia log para o legado
*----------------------------------------------------------------------*
FORM z_envia_log_legado .

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  IF NOT it_outreturn[] IS INITIAL.
    SORT it_outreturn BY obj_key interface.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        OUTRETURN = IT_OUTRETURN.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    COMMIT WORK.

  ENDIF.

ENDFORM.                    " Z_ENVIA_LOG_LEGADO

FORM z_processa_pgto .

  DATA: tg_zglt087    TYPE TABLE OF zglt087 WITH HEADER LINE,
        vl_message_v1 TYPE zfie_ret_document-message_v1.

  REFRESH: it_mmt24_pg[], it_mmt24_pg_tot[], tg_zglt080[], tg_zglt081[].

  "Gera NF para lançamentos com Doc. Ctb. já gerado
  SET PARAMETER ID 'OBJKEY' FIELD ''.
  SET PARAMETER ID 'GERMOV' FIELD 'S'.
  CALL TRANSACTION 'ZGL059' AND SKIP FIRST SCREEN.

  IF p_objkey IS INITIAL.
    p_objkey = '%'.
  ENDIF.

  "Leitura para Lançamento Pgto Telefonia (ZGL059)
  SELECT *
    FROM zmmt0024 INTO TABLE it_mmt24_pg
   WHERE banfn         EQ ''
     AND bsart         IN ('RCC')
     AND objkey        LIKE p_objkey
     AND rg_atualizado EQ 'N'
     AND id_interface  EQ 1
     AND nr_documento  NE ''.

  CHECK it_mmt24_pg[] IS NOT INITIAL.

  it_mmt24_pg_tot[] = it_mmt24_pg[].
  SORT: it_mmt24_pg_tot BY objkey,
        it_mmt24_pg     BY objkey.

  DELETE ADJACENT DUPLICATES FROM it_mmt24_pg_tot COMPARING objkey.

  "Seleção dados Pagamentos.
  SELECT *
    FROM zglt081 INTO TABLE tg_zglt081
     FOR ALL ENTRIES IN it_mmt24_pg
    WHERE objkey = it_mmt24_pg-objkey.

  IF tg_zglt081[] IS NOT INITIAL.
    SELECT *
      FROM zglt080 INTO TABLE tg_zglt080
       FOR ALL ENTRIES IN tg_zglt081
     WHERE seq_lcto = tg_zglt081-seq_lcto.
  ENDIF.

  UPDATE zmmt0024 SET rg_atualizado = 'S'
   WHERE banfn          EQ ''
     AND bsart          IN ('RCC')
     AND objkey         LIKE p_objkey
     AND rg_atualizado  EQ 'N'
     AND id_interface   EQ 1.

  LOOP AT it_mmt24_pg_tot.

    REFRESH: tg_zglt087.

    READ TABLE tg_zglt081 WITH KEY objkey = it_mmt24_pg_tot-objkey.
    IF sy-subrc = 0.
      READ TABLE tg_zglt080 WITH KEY seq_lcto = tg_zglt081-seq_lcto.
      "LOEKZ    = ''. " Marcação para exclusão
      IF sy-subrc = 0.
        PERFORM z_prepara_mensagem USING it_mmt24_pg_tot-objkey
                                       vtype
                                       c_29
                                       'Lançamento já criado na ZGL059'
                                       '' .
        CONTINUE.
      ENDIF.
    ENDIF.

    SET PARAMETER ID 'OBJKEY' FIELD it_mmt24_pg_tot-objkey.
    SET PARAMETER ID 'GERMOV' FIELD ''.
    CALL TRANSACTION 'ZGL059' AND SKIP FIRST SCREEN.

    SELECT *
      FROM zglt087 INTO TABLE tg_zglt087
     WHERE objkey = it_mmt24_pg_tot-objkey.

    "Monta Log Retorno
    LOOP AT tg_zglt087.
      vl_message_v1 = tg_zglt087-seq_lcto.
      PERFORM z_prepara_mensagem USING it_mmt24_pg_tot-objkey
                                       tg_zglt087-type
                                       c_29
                                       tg_zglt087-message
                                       vl_message_v1 .
    ENDLOOP.

  ENDLOOP.


ENDFORM.
