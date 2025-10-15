*&-------- N I N J A S   E V O L U T I O N  -  A M A G G I ------------*
* Programa   : ZFIS42                                                  *
* Descrição  : Automação Conferência Fiscal                            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Ronaldo Freitas                        Data: 02.01.2023 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      : XXXXXXXXXXXX                          Data: XX.XX.XXXX  *
* Observações: XXXXXXXXXXXX                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report  ZFIS42
*&---------------------------------------------------------------------*
REPORT zfis42.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

TABLES : j_1bnfdoc.

*----------------------------------------------------------------------*
* OBJECTS                                                              *
*----------------------------------------------------------------------*
DATA: lo_log TYPE REF TO zhrst_sto_cl_bal_log.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_j_1bnfdoc,
    belnr  TYPE j_1bnfdoc-belnr,
    docnum TYPE j_1bnfdoc-docnum,
    pstdat TYPE j_1bnfdoc-pstdat,
    bukrs  TYPE j_1bnfdoc-bukrs,
    series TYPE j_1bnfdoc-series,
    nftype TYPE j_1bnfdoc-nftype,
    docdat TYPE j_1bnfdoc-docdat,
    crenam TYPE j_1bnfdoc-crenam,
    model  TYPE j_1bnfdoc-model,
    nfnum  TYPE j_1bnfdoc-nfnum,
    branch TYPE j_1bnfdoc-branch,
    parid  TYPE j_1bnfdoc-parid,
    nfe    TYPE j_1bnfdoc-nfe,
    nfenum TYPE j_1bnfdoc-nfenum,
    partyp TYPE j_1bnfdoc-partyp,
    nftot  TYPE j_1bnfdoc-nftot,
    direct TYPE j_1bnfdoc-direct,
    cancel TYPE j_1bnfdoc-cancel,
    ntgew  TYPE j_1bnfdoc-ntgew,
  END   OF ty_j_1bnfdoc.

TYPES: BEGIN OF y_zdados_saida.
         INCLUDE STRUCTURE zdados_saida.
         TYPES: ebelp TYPE zfiwrt0008-ebelp,
       END OF  y_zdados_saida.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: tg_saida  TYPE TABLE OF  y_zdados_saida,
      gs_messag TYPE bal_s_msg,
      ls_line   TYPE solisti1-line,
      wg_saida  TYPE y_zdados_saida.

DATA: l_data            TYPE REF TO data,
      lv_dats           TYPE sy-datum,
      l_obj             TYPE balobj_d,
      l_c               TYPE i,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

* Data for sending e-mail:
DATA: it_packing_list TYPE TABLE OF sopcklsti1,
      it_header       TYPE TABLE OF solisti1,
      it_contents_txt TYPE TABLE OF solisti1,
      it_contents_txb TYPE TABLE OF solisti1,
      it_contents_bin TYPE TABLE OF solisti1,
      it_receivers    TYPE TABLE OF somlreci1.

DATA: wa_packing_list TYPE sopcklsti1,
      wa_contents_txt TYPE solisti1,
      wa_receivers    TYPE somlreci1.

DEFINE new_line.
  CLEAR  WA_CONTENTS_TXT.
         WA_CONTENTS_TXT = &1.
  APPEND WA_CONTENTS_TXT TO IT_CONTENTS_TXT.
END-OF-DEFINITION.

DEFINE add_receiver.
  CLEAR  WA_RECEIVERS.
         WA_RECEIVERS-RECEIVER   = &1.
         WA_RECEIVERS-REC_TYPE   = 'U'.
         WA_RECEIVERS-BLIND_COPY = &2.
  APPEND WA_RECEIVERS TO IT_RECEIVERS[].
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_bukrs  FOR  j_1bnfdoc-bukrs,                    "OBLIGATORY.
                p_branch FOR  j_1bnfdoc-branch.                   "OBLIGATORY.
PARAMETER:      p_direct TYPE ty_j_1bnfdoc-direct.                "OBLIGATORY.

SELECT-OPTIONS: p_pstdat FOR  j_1bnfdoc-pstdat NO-EXTENSION." OBLIGATORY
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_tmiss   TYPE j_1bnflin-tmiss RADIOBUTTON GROUP r1,
           p_tmpro   TYPE j_1bnflin-tmiss RADIOBUTTON GROUP r1 DEFAULT 'X',
           p_tmtod   TYPE c RADIOBUTTON GROUP r1. "DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETER: p_ativas TYPE ty_j_1bnfdoc-cancel DEFAULT 'X',
           p_nativa  TYPE ty_j_1bnfdoc-cancel.
SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETER: p_autor  TYPE ty_j_1bnfdoc-cancel,
           p_rejeit TYPE ty_j_1bnfdoc-cancel DEFAULT 'X',
           p_recus  TYPE ty_j_1bnfdoc-cancel DEFAULT 'X',
           p_cancel TYPE ty_j_1bnfdoc-cancel DEFAULT 'X',
           p_agres  TYPE ty_j_1bnfdoc-cancel DEFAULT 'X',
           p_nenv   TYPE ty_j_1bnfdoc-cancel DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b4.

INITIALIZATION.

  PERFORM f_initial.

*-----------------------------------------------------------------------------------*
START-OF-SELECTION.
*-----------------------------------------------------------------------------------*

  CREATE OBJECT lo_log.

  IF p_pstdat[] IS INITIAL.
    CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO lv_dats.
    p_pstdat-sign   = 'I'.
    p_pstdat-option = 'BT'.
    p_pstdat-low    = lv_dats.
    p_pstdat-high   = sy-datum - 1.
    APPEND p_pstdat.
    CLEAR p_pstdat.
  ENDIF.

  l_obj = sy-cprog.
  lo_log->create_log( im_object    = l_obj
                      im_subobject = l_obj ).

  IF p_direct IS INITIAL.
    DATA(lv_t) = 2.
  ELSE.
    lv_t = 1.
  ENDIF.

  DO lv_t TIMES.
    IF p_direct IS INITIAL OR l_c IS NOT INITIAL.
      l_c = l_c + 1.
      p_direct = l_c.
      FREE: wg_saida, tg_saida.
    ENDIF.

    TRY.
        PERFORM f_prepare_run_time_info USING abap_false.

        SUBMIT zfis12    WITH p_bukrs   IN p_bukrs
                         WITH p_branch  IN p_branch
                         WITH p_direct  EQ p_direct
                         WITH p_pstdat  IN p_pstdat
                         WITH p_tmiss   EQ p_tmiss
                         WITH p_tmpro   EQ p_tmpro
                         WITH p_tmtod   EQ p_tmtod
                         WITH p_ativas  EQ p_ativas
                         WITH p_nativa  EQ p_nativa
                         WITH p_autor   EQ p_autor
                         WITH p_rejeit  EQ p_rejeit
                         WITH p_recus   EQ p_recus
                         WITH p_cancel  EQ p_cancel
                         WITH p_agres   EQ p_agres
                         WITH p_nenv    EQ p_nenv
                         AND RETURN.

        PERFORM f_get_runtime_info.

        IF <t_data> IS ASSIGNED.
          LOOP AT <t_data> ASSIGNING <w_data>.

            MOVE-CORRESPONDING <w_data> TO wg_saida.
            APPEND wg_saida TO tg_saida.

            IF p_direct EQ '1'.
              IF sy-tabix EQ 1.
                CONCATENATE 'Documento' 'Filial' 'Fornecedor' 'Status NFE' 'Status SAP'
                       INTO ls_line SEPARATED BY ';'.
                APPEND ls_line TO it_contents_txb.
                CLEAR ls_line.
              ENDIF.
              CONCATENATE wg_saida-docnum wg_saida-branch wg_saida-nome_clifor wg_saida-status wg_saida-nf_status
              INTO ls_line SEPARATED BY ';'.
            ELSE.
              IF sy-tabix EQ 1.
                CONCATENATE 'Documento' 'Filial' 'Status NFE' 'Status SAP'
                       INTO ls_line SEPARATED BY ';'.
                APPEND ls_line TO it_contents_txb.
                CLEAR ls_line.
              ENDIF.
              CONCATENATE wg_saida-docnum wg_saida-branch wg_saida-status wg_saida-nf_status
              INTO ls_line SEPARATED BY ';'.
            ENDIF.
            APPEND ls_line TO it_contents_txb.
            CLEAR: ls_line, wg_saida.
          ENDLOOP.

          IF tg_saida[] IS NOT INITIAL.
* Envio e-mail entrada e saída
            DATA(vl_data)    = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum+0(4) }|.
            CASE p_direct.
              WHEN '1'. "Entrada
                DATA(vl_assunto) = |AMAGGI – Documentos de Entrada ZCONF - { vl_data }|.
                PERFORM: corpo_email_e USING wg_saida, send_email_e USING vl_assunto wg_saida.
              WHEN '2'. "Saida
                vl_assunto = |AMAGGI – Documentos de Saída ZCONF - { vl_data }|.
                PERFORM: corpo_email_e USING wg_saida, send_email_e USING vl_assunto wg_saida.
            ENDCASE.
          ENDIF.
        ENDIF.

      CATCH cx_salv_bs_sc_runtime_info.
        gs_messag-msgid      = sy-msgid.
        gs_messag-msgty      = sy-msgty.
        gs_messag-probclass  = '2'.
        gs_messag-msgv1      = sy-msgv1.
        gs_messag-msgv2      = sy-msgv2.
        gs_messag-msgv3      = sy-msgv3.
        lo_log->add_msg( gs_messag ).
        IF lo_log IS NOT INITIAL.
          lo_log->save_log( ). "Save Log transaction SLG1
        ENDIF.
        IF sy-batch IS INITIAL.
          MESSAGE 'Erro no envio(Log SLG1)!'(e02) TYPE 'E'.
        ENDIF.
    ENDTRY.

  ENDDO.
*-----------------------------------------------------------------------------------*
END-OF-SELECTION.
*-----------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_FALSE  text
*----------------------------------------------------------------------*
*FORM f_prepare_run_time_info  USING    p_abap_false.
FORM f_prepare_run_time_info USING p_display TYPE c.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line, l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = p_display
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM f_get_runtime_info .
FORM f_get_runtime_info.
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr  = l_data_descr
                r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data>
                                                   t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CORPO_EMAIL_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_ENVIA  text
*----------------------------------------------------------------------*
FORM corpo_email_e  USING wg_envia TYPE any."ty_saida.

* CRIA TABELA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line'<STYLE>'.
  new_line' TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  new_line' TH, TD { PADDING: 5PX; }'.
  new_line' TH { TEXT-ALIGN: LEFT; }'.
  new_line' TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; }'.
  new_line' TABLE#T02 TH { BACKGROUND-COLOR:#218bdb; COLOR: WHITE; }'.
  new_line'</STYLE>'.

  new_line' <H4 ALIGN=LEFT>Prezado, Segue a lista:</H4>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:70%" ID="T02">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:10%">Documento</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:10%">Filial</FONT></TH>'.
  IF p_direct EQ '1'.
    new_line'   <TH STYLE="WIDTH:30%"><FONT SIZE="2">Fornecedor</FONT></TH>'.
  ENDIF.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:10%">Status NFE</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:10%">Status SAP</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT tg_saida[] INTO DATA(wl_saida).
    new_line' <TR>'.
    new_line'  <TD style="width:10%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_saida-docnum.
    new_line'  </FONT></TD>'.

    new_line'  <TD style="width:10%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_saida-branch.
    new_line'  </FONT></TD>'.

    IF p_direct EQ '1'.
      new_line'  <TD style="width:30%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
      new_line      wl_saida-nome_clifor.
      new_line'  </FONT></TD>'.
    ENDIF.
    new_line'   <TD style="width:10%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_saida-status.
    new_line'   </FONT></TD>'.
    new_line'   <TD style="width:10%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_saida-nf_status.
    new_line'   </FONT></TD>'.
    new_line ' </TR>'.
  ENDLOOP.
  new_line ' </TABLE>'.
  new_line'  <H6>Atenciosamente,</H6><H6><b>ADM-AMAGGI</b></H6><H6>https://www.amaggi.com.br/</H6>'.
  new_line '</BODY>'.
  new_line '</HTML>'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_ASSUNTO  text
*      -->P_WG_ENVIA  text
*----------------------------------------------------------------------*
FORM send_email_e  USING p_vl_assunto wg_envia TYPE any."ty_saida.

  DATA: lv_lines    LIKE sy-tabix,
        wa_doc_data LIKE sodocchgi1,
        wa_zmail    TYPE zmail,
        it_zmail    TYPE TABLE OF zmail.

  FREE: wa_zmail, it_zmail.
  SELECT *
    FROM zmail
    INTO TABLE it_zmail
    WHERE tcode EQ sy-cprog.

  IF it_zmail[] IS NOT INITIAL.
    LOOP AT it_zmail INTO wa_zmail.
      IF wa_zmail-email IS INITIAL.
        wa_zmail-email = text-006.
      ELSE.
        add_receiver wa_zmail-email 'X'.
      ENDIF.
    ENDLOOP.
  ELSE.
    wa_zmail-email = text-006.
*    wa_zmail-email = 'ronaldo.freitas@amaggi.com.br'.
    add_receiver wa_zmail-email 'X'.
  ENDIF.

  lv_lines = lines( it_contents_txt ).

  wa_packing_list-transf_bin = space.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 0.
  wa_packing_list-body_start = 1.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'HTM'.
  APPEND wa_packing_list TO it_packing_list.

* Anexo
  wa_packing_list-transf_bin = abap_true.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 1.
  wa_packing_list-body_start = 1.
  wa_packing_list-doc_size   = lv_lines * 255.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'CSV'.
  wa_packing_list-obj_name   = 'Anexo.csv'.
  wa_packing_list-obj_descr  = 'Documentos'.
  APPEND wa_packing_list TO it_packing_list.

  READ TABLE it_contents_txt INTO wa_contents_txt INDEX lv_lines.

  wa_doc_data-obj_descr = p_vl_assunto.
  wa_doc_data-doc_size  = ( lv_lines - 1 ) * 255 + strlen( wa_contents_txt ).

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = it_contents_txb
      binary_tab = it_contents_bin
    EXCEPTIONS
      failed     = 1
      OTHERS     = 2.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = wa_doc_data
      put_in_outbox              = 'X'
      sender_address             = ''
      sender_address_type        = ''
      commit_work                = 'X'
    TABLES
      packing_list               = it_packing_list
      object_header              = it_header
      contents_bin               = it_contents_bin
      contents_txt               = it_contents_txt
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc IS INITIAL.
    CLEAR: wa_packing_list, wg_envia, wg_saida, it_contents_bin[], it_packing_list[], it_header[], it_contents_txt[], it_receivers[].
    IF sy-batch IS INITIAL.
      MESSAGE 'Mensagem enviada com sucesso!'(005) TYPE 'S'.
    ENDIF.
  ELSE.
    IF sy-batch IS INITIAL.
      MESSAGE 'Erro no envio!'(e01) TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_initial .

  IF p_bukrs[] IS INITIAL.
    p_bukrs-sign   = 'I'.
    p_bukrs-option = 'EQ'.
    p_bukrs-low    = '0001'.
    APPEND p_bukrs.
    CLEAR p_bukrs.
  ENDIF.

  IF p_branch[] IS INITIAL.
    p_branch-sign   = 'I'.
    p_branch-option = 'BT'.
    p_branch-low    = '0101'.
    p_branch-high   = '9121'.
    APPEND p_branch.
    CLEAR p_branch.
  ENDIF.

  IF p_pstdat[] IS INITIAL.
    CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO lv_dats.
    p_pstdat-sign   = 'I'.
    p_pstdat-option = 'BT'.
    p_pstdat-low    = lv_dats.
    p_pstdat-high   = sy-datum - 1.
    APPEND p_pstdat.
    CLEAR p_pstdat.
  ENDIF.

ENDFORM.
