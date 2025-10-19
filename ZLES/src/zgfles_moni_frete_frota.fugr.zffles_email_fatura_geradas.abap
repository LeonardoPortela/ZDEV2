FUNCTION zffles_email_fatura_geradas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_COD_CLIENTE) TYPE  ZLEST0194-KUNNR_OV
*"     REFERENCE(I_NRO_FATURA) TYPE  ZLEST0194-NR_FATURA
*"  EXPORTING
*"     REFERENCE(E_ENVIADO) TYPE  SOFOLENTI1-OBJECT_ID
*"  TABLES
*"      GT_SAIDA STRUCTURE  ZLEST0194
*"----------------------------------------------------------------------

  TABLES: adr6.

*  SELECT-OPTIONS: MAIL_REC FOR ADR6-SMTP_ADDR.

*----------------------------------------------------------------------
* Declaração de Constantes
*----------------------------------------------------------------------
  CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

*----------------------------------------------------------------------
* Declaração de tipos
*----------------------------------------------------------------------
  TYPES: BEGIN OF y_files,
           fname(10) TYPE c,
         END OF y_files.

  TYPES: BEGIN OF y_fatura,
           nr_fatura(20)        TYPE c, "ZLEST0194-NR_fatura,
           nr_cte_sub(20)       TYPE c, "ZLEST0194-NR_CTE_SUB,
           qt_descarga_cte(20)  TYPE c, "ZLEST0194-QT_DESCARGA_CTE,
           valor_prestacao(20)  TYPE c, "ZLEST0194-VALOR_PRESTACAO,
           zvlr_quebra(20)      TYPE c, "ZLEST0194-ZVLR_quebra
           zvlr_perda(20)       TYPE c, "ZLEST0194-ZVLR_PERDA
           zvlr_liq_receber(20) TYPE c, "ZLEST0194-ZVLR_LIQ_RECEBER,
           data_vencimento(20)  TYPE c, "ZLEST0194-DATA_VENCIMENTO,
         END OF y_fatura.

  TYPES: BEGIN OF y_soma,
           nr_fatura(20)        TYPE c,
           qt_descarga_cte(20)  TYPE c,
           valor_prestacao(20)  TYPE c,
           zvlr_liq_receber(20) TYPE c,
         END OF y_soma.

  TYPES: BEGIN OF y_collect,
           nr_fatura        TYPE zlest0194-nr_fatura,
           qt_descarga_cte  TYPE zlest0194-qt_descarga_cte,
           valor_prestacao  TYPE zlest0194-valor_prestacao,
           zvlr_liq_receber TYPE zlest0194-zvlr_liq_receber,
         END OF y_collect.

  TYPES: BEGIN OF y_email,
           kunnr     TYPE kna1-kunnr,
           adrnr     TYPE kna1-adrnr,
           smtp_addr TYPE adr6-smtp_addr,
         END OF y_email.

*----------------------------------------------------------------------
* Declaração de tabela Interna
*----------------------------------------------------------------------
  DATA: gt_objtxt   TYPE TABLE OF solisti1,
        gt_objpack  TYPE TABLE OF sopcklsti1,
        gt_fatura   TYPE TABLE OF y_fatura,
        gt_collect  TYPE TABLE OF y_collect,
        gt_soma     TYPE TABLE OF y_soma,
        gt_receiver TYPE TABLE OF somlreci1,
        gt_email    TYPE TABLE OF y_email.

*----------------------------------------------------------------------
* Declaração de Estrutura
*----------------------------------------------------------------------
  DATA: gs_fatura   LIKE LINE OF gt_fatura,
        gs_soma     LIKE LINE OF gt_soma,
        gs_collect  LIKE LINE OF gt_collect,
        gs_docdata  TYPE sodocchgi1,
        gs_objtxt   LIKE LINE OF gt_objtxt,
        gs_objpack  LIKE LINE OF gt_objpack,
        gs_receiver LIKE LINE OF gt_receiver.

*----------------------------------------------------------------------
* Declaração de Variáveis
*----------------------------------------------------------------------
  DATA: gv_nro_fatura           TYPE i,
        gv_rec_cnt              TYPE c LENGTH 10,
        gv_lines                TYPE i,
        gv_sentall              TYPE sonv-flag,
        gv_nobjid               TYPE sofolenti1-object_id,
        gv_qt_descarga_cte(20)  TYPE c,
        gv_zvlr_liq_receber(20) TYPE c.


*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

  gv_nro_fatura  = i_nro_fatura.
  gv_rec_cnt = gv_nro_fatura.

* Busca endereço de e-mail
  SELECT k~kunnr k~adrnr a~smtp_addr
    INTO TABLE gt_email
      FROM kna1 AS k INNER JOIN adr6 AS a
         ON a~addrnumber  = k~adrnr
       WHERE k~kunnr = i_cod_cliente.

  "CHECK GT_EMAIL[] IS NOT INITIAL AND GT_SAIDA[] IS NOT INITIAL.
  IF gt_email IS INITIAL AND gt_saida[] IS INITIAL.

    e_enviado = i_cod_cliente.
    EXIT.
  ENDIF.


  LOOP AT gt_saida INTO DATA(gs_saida_fg).

    gs_fatura-nr_fatura         = gs_saida_fg-nr_fatura.
    PACK gs_fatura-nr_fatura TO gs_fatura-nr_fatura.

    gs_fatura-nr_cte_sub        = gs_saida_fg-nr_cte_sub.
    PACK gs_fatura-nr_cte_sub TO gs_fatura-nr_cte_sub.

    gs_fatura-qt_descarga_cte   = gs_saida_fg-qt_descarga_cte.
    REPLACE '.'  WITH ',' INTO  gs_fatura-qt_descarga_cte.
    CONDENSE gs_fatura-qt_descarga_cte.

    gs_fatura-valor_prestacao   = gs_saida_fg-valor_prestacao.
    REPLACE '.'  WITH ',' INTO  gs_fatura-valor_prestacao.
    CONDENSE gs_fatura-valor_prestacao.

    gs_fatura-zvlr_quebra   = gs_saida_fg-zvlr_quebra.
    REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_quebra.
    CONDENSE gs_fatura-zvlr_quebra.

    gs_fatura-zvlr_perda   = gs_saida_fg-zvlr_perda.
    REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_perda.
    CONDENSE gs_fatura-zvlr_perda.

    gs_fatura-zvlr_liq_receber  = gs_saida_fg-zvlr_liq_receber.
    REPLACE '.'  WITH ',' INTO  gs_fatura-zvlr_liq_receber.
    CONDENSE gs_fatura-zvlr_liq_receber.

    CONCATENATE gs_saida_fg-data_vencimento+6(2) '/'
                gs_saida_fg-data_vencimento+4(2) '/'
                gs_saida_fg-data_vencimento(4) INTO  gs_fatura-data_vencimento.

    APPEND gs_fatura TO gt_fatura.

    MOVE-CORRESPONDING gs_saida_fg TO gs_collect.
    COLLECT gs_collect INTO gt_collect.

  ENDLOOP.

* ******************Prepare Corpo do E-mail*******************************************
*MOVE '<body style="background-color: #D7ECF3;">' TO GS_OBJTXT-LINE. " Plano de Fundo
*APPEND GS_OBJTXT TO GT_OBJTXT.

  MOVE '<BR>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.
  CLEAR gs_objtxt.

  CONCATENATE 'Aos  Cuidados do  depto de  contas a  pagar.'(011) '<BR><BR>'
  INTO gs_objtxt-line SEPARATED BY space.
  APPEND gs_objtxt TO gt_objtxt.
  CLEAR gs_objtxt.

*MOVE GV_TOT_CNT TO GV_REC_CNT.

  CONCATENATE 'Segue  fatura: '(012) gv_rec_cnt '<BR><BR>' INTO gs_objtxt-line SEPARATED BY space.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

  MOVE '<TABLE BORDER=1>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Documento </TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col2
  MOVE '<TH> Peso Descarga </TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col3
  MOVE '<TH> Valor Frete</TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col4
  MOVE '<TH> Valor Quebra</TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col5
  MOVE '<TH> Valor Perda</TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col6
  MOVE '<TH> Valor Líquido </TH>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col7
  MOVE '<TH> Vencimento </TH></TR>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.


  LOOP AT gt_fatura INTO gs_fatura.

*col1
    IF gs_fatura-nr_cte_sub IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' gs_fatura-nr_cte_sub '</TD>' INTO gs_objtxt-line.
      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO gs_objtxt-line.
      APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.
    ENDIF.

*col2
    CONCATENATE '<TD>' gs_fatura-qt_descarga_cte '</TD>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col3
    CONCATENATE '<TD>' gs_fatura-valor_prestacao '</TD>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col4
    CONCATENATE '<TD>' gs_fatura-zvlr_quebra '</TD>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col5
    CONCATENATE '<TD>' gs_fatura-zvlr_perda '</TD>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col6
    CONCATENATE '<TD>' gs_fatura-zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.   CLEAR gs_objtxt.

*col7
    CONCATENATE '<TD>' gs_fatura-data_vencimento '</TD></TR>' INTO gs_objtxt-line.
    APPEND gs_objtxt TO gt_objtxt.CLEAR gs_objtxt.

  ENDLOOP.

  READ TABLE gt_collect INTO gs_collect INDEX 1.

  gs_soma-nr_fatura          = gs_collect-nr_fatura.
  gs_soma-qt_descarga_cte    = gs_collect-qt_descarga_cte.
  gs_soma-valor_prestacao    = gs_collect-valor_prestacao.
  gs_soma-zvlr_liq_receber   = gs_collect-zvlr_liq_receber.

* Monta Linha de Totalização
*col1
  MOVE '<TR style="background-color: #FFFF00;"> <TH> &Sigma; Total </TH> ' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

*col2
  CLEAR gv_qt_descarga_cte.
  gv_qt_descarga_cte = gs_soma-qt_descarga_cte.

  REPLACE '.' WITH ',' INTO gv_qt_descarga_cte.
  CONDENSE gv_qt_descarga_cte.
  CONCATENATE '<TD>' gv_qt_descarga_cte '</TD>' INTO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

*col3
  MOVE '<TD>&nbsp;</TD>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

*col4
  MOVE '<TD>&nbsp;</TD>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

*col5
  MOVE '<TD>&nbsp;</TD>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

*col6
  CLEAR gv_zvlr_liq_receber.
  gv_zvlr_liq_receber = gs_soma-zvlr_liq_receber.
  REPLACE '.' WITH ',' INTO gv_zvlr_liq_receber.
  CONDENSE gv_zvlr_liq_receber.

  CONCATENATE '<TD>' gv_zvlr_liq_receber '</TD>' INTO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

  MOVE '<TD>&nbsp;</TD></TR>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.

  MOVE '</TABLE>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt.  CLEAR gs_objtxt.

  CONCATENATE '<BR>' 'Atenciosamente'(013) '<BR>'
  text-018 INTO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.

  MOVE '<BR><BR>' TO gs_objtxt-line.
  APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.

* MOVE ' <TABLE><TH>*** This is an auto generated mail from SAP-HCM, Do not reply ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' TO GS_OBJTXT-LINE.
  CONCATENATE '<TABLE><TH>*** This is an auto generated mail from ' sy-sysid sy-mandt 'system, Do not reply ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO gs_objtxt-line SEPARATED BY space..
  APPEND gs_objtxt TO gt_objtxt. CLEAR gs_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE text-015 TO gs_docdata-obj_descr.
  gs_docdata-sensitivty = 'F'.

  DESCRIBE TABLE gt_objtxt LINES gv_lines.
  IF NOT gv_lines IS INITIAL.
    gs_docdata-doc_size = gv_lines * 255.
  ENDIF.

  CLEAR gs_objpack.
  gs_objpack-transf_bin = ''.
  gs_objpack-head_start = 1.
  gs_objpack-head_num = 0.
  gs_objpack-body_start = 1.
  gs_objpack-body_num = gv_lines.
  gs_objpack-doc_type = 'HTM'.
  gs_objpack-doc_size = ( gv_lines - 1 ) * 255 + strlen( gs_objtxt ).
  APPEND gs_objpack TO gt_objpack.

  IF gt_email[] IS NOT INITIAL.

    LOOP AT gt_email INTO DATA(gs_email).
      gs_receiver-receiver = gs_email-smtp_addr.
      gs_receiver-rec_type = 'U'.
      gs_receiver-com_type = c_comtyp_int.
      APPEND gs_receiver TO gt_receiver.
      CLEAR gs_receiver.
    ENDLOOP.
    else.
      e_enviado = i_cod_cliente.
    EXIT.
  ENDIF.

  CHECK gt_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gs_docdata
*     put_in_outbox              = c_x
      commit_work                = 'X'
    IMPORTING
      sent_to_all                = gv_sentall
      new_object_id              = gv_nobjid
    TABLES
      packing_list               = gt_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = gt_contents
      contents_txt               = gt_objtxt[]
*     CONTENTS_HEX               = gt_contents_hex
      receivers                  = gt_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF gv_nobjid IS NOT INITIAL.
    e_enviado = 'SUCESSO'.
    LOOP AT gt_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      <fs_saida>-email = '@1S@'.
    ENDLOOP.

    MODIFY zlest0194 FROM TABLE gt_saida.
    COMMIT WORK.

  ENDIF.

ENDFUNCTION.
