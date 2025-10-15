*&---------------------------------------------------------------------*
*& Report ZSDR0047
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Enio Jesus                                              &*
*& Data.....: 30/04/2015                                              &*
*& Descrição: Serviço de E-mail CND (JOB)                             &*
*& Transação: SD                                                      *&
*&                                                                    &*
*&--------------------------------------------------------------------&*

REPORT zsdr0047.

*&---------------------------------------------------------------------*
*& TABELAS INTERNAS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         dias           TYPE zsdt0099-dias,
         dt_validade    TYPE zjcnd_branch-dt_validade,
         empresa        TYPE zjcnd_branch-bukrs,
         data           TYPE zsdt0099-data,
         cod_cnd        TYPE zjcnd_branch-cd_cnd,
         filial         TYPE zjcnd_branch-branch,
         status(10)     TYPE c,
         receiver_email TYPE zsdt0099-email.
TYPES: END OF ty_saida.

DATA: it_packing_list TYPE TABLE OF sopcklsti1,
      it_header       TYPE TABLE OF solisti1,
      it_contents_txt TYPE TABLE OF solisti1,
      it_contents_bin TYPE TABLE OF solisti1,
      it_receivers    TYPE TABLE OF somlreci1.

DATA: it_zjcnd_branch TYPE TABLE OF zjcnd_branch,
      it_zsdt0099     TYPE TABLE OF zsdt0099,
      it_saida        TYPE TABLE OF ty_saida WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& TABELAS WORK-ÁREAS
*&---------------------------------------------------------------------*
DATA: wa_zjcnd_branch TYPE zjcnd_branch,
      wa_zsdt0099     TYPE zsdt0099,
      wa_saida        TYPE ty_saida.

DATA: wa_packing_list TYPE sopcklsti1,
      wa_header       TYPE solisti1,
      wa_contents_txt TYPE solisti1,
      wa_contents_bin TYPE solisti1,
      wa_receivers    TYPE somlreci1.

DATA: qt_dias  TYPE i,
      qt_meses TYPE i,
      date(15) TYPE c.
DATA: lv_lines        LIKE sy-tabix.
DATA: wa_doc_data     LIKE sodocchgi1.

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
  APPEND WA_RECEIVERS TO IT_RECEIVERS.
END-OF-DEFINITION.

START-OF-SELECTION.
  PERFORM:seleciona_dados.

*&---------------------------------------------------------------------*
*& FORM SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.
  SELECT * FROM zjcnd_branch INTO TABLE it_zjcnd_branch
    WHERE dt_validade >= sy-datum.

  SELECT * FROM zsdt0099 INTO TABLE it_zsdt0099.

  IF it_zsdt0099[] IS NOT INITIAL.
    LOOP AT it_zjcnd_branch INTO wa_zjcnd_branch.
      READ TABLE it_zsdt0099 INTO wa_zsdt0099 INDEX 1.

      wa_saida-dias        = wa_zsdt0099-dias.
      wa_saida-dt_validade = wa_zjcnd_branch-dt_validade.
      wa_saida-empresa     = wa_zjcnd_branch-bukrs.
      wa_saida-filial      = wa_zjcnd_branch-branch.
      wa_saida-cod_cnd     = wa_zjcnd_branch-cd_cnd.
      wa_saida-data        = wa_zsdt0099-data.

*      LOOP AT it_zsdt0099 ASSIGNING FIELD-SYMBOL(<ws_zsdt009>).  " [IR063543] - AO
*        IF wa_saida-receiver_email IS INITIAL.
*          wa_saida-receiver_email = <ws_zsdt009>-email.
*        ELSE.
*          wa_saida-receiver_email = |{ wa_saida-receiver_email }; { <ws_zsdt009>-email }|.
*        ENDIF.
*      ENDLOOP.

      wa_saida-status      = 'À VENCER'.
      wa_saida-receiver_email = wa_zsdt0099-email.

      "DIFERENÇA DE UMA DATA PARA OUTRA E COLOCA NA VARIÁVEL QT_DIAS.
*  CALL FUNCTION 'HR_AUPBS_MONTH_DAY'
*    EXPORTING
*      BEG_DA = SY-DATUM
*      END_DA = WA_SAIDA-DT_VALIDADE
*    IMPORTING
*      NO_DAY         = QT_DIAS.

      qt_dias = wa_saida-dt_validade - sy-datum.

      IF qt_dias <= wa_saida-dias.
        APPEND wa_saida TO it_saida.
        SORT it_saida ASCENDING.
      ENDIF.

    ENDLOOP.

  ELSE.
    WRITE 'Não existem parâmetros na transação ZSDT0093!'.
  ENDIF.

  CHECK NOT it_saida[] IS INITIAL.
*  PERFORM: corpo_email,
*           send_email USING 'ALERTA - CADASTRO DE CERTIDÃO NEGATIVA (À VENCER)'.
  PERFORM dispara_lista_email.

ENDFORM.

FORM dispara_lista_email.

  PERFORM: corpo_email,
        send_email USING 'ALERTA - CADASTRO DE CERTIDÃO NEGATIVA (À VENCER)'.

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM F_SEND_EMAIL
*&---------------------------------------------------------------------*
FORM send_email USING p_subject.

  "ADICIONA DESTINATÁRIOS DO E-MAIL
  "add_receiver wa_saida-receiver_email 'X'.
  LOOP AT it_zsdt0099 INTO DATA(wa_email).
    add_receiver wa_email-email 'X'.
  ENDLOOP.

  lv_lines = lines( it_contents_txt ).

  CLEAR wa_packing_list.
  wa_packing_list-transf_bin = space.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 0.
  wa_packing_list-body_start = 1.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'HTM'.
  APPEND wa_packing_list TO it_packing_list.


  READ TABLE it_contents_txt INTO wa_contents_txt INDEX lv_lines.

  wa_doc_data-obj_descr = p_subject.
  wa_doc_data-doc_size  = ( lv_lines - 1 ) * 255 + strlen( wa_contents_txt ).

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

ENDFORM.

*&---------------------------------------------------------------------*
*& FORM PREPARA_EMAIL
*&---------------------------------------------------------------------*
FORM corpo_email.
  "CRIA TABELA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line'<STYLE>'.

  new_line' TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  new_line' TH, TD { PADDING: 5PX; }'.
  new_line' TH { TEXT-ALIGN: LEFT; }'.
  new_line' TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; }'.
  new_line'</STYLE>'.

  new_line' <H3 STYLE="COLOR:#191970" ALIGN=LEFT><b>Cadastro de Certidão Negativa (À vencer)<b></H3>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:50%" ID="T01">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2">Empresa</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Filial</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Data</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Código CND</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Status</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT it_saida INTO wa_saida.
    "CONVERTE A DATA PARA FORMATO XX/XX/XXXX.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = wa_saida-dt_validade
      IMPORTING
        output = date.

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2">'.
    new_line      wa_saida-empresa.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2">'.
    new_line      wa_saida-filial.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2">'.
    new_line      date.
    new_line'  </FONT></TD>'.

    new_line'   <TD><FONT SIZE="2">'.
    new_line      wa_saida-cod_cnd.
    new_line'   </FONT></TD>'.

    new_line'   <TD STYLE: BGCOLOR="YELLOW"><FONT SIZE="2">'.
    new_line      wa_saida-status.
    new_line'   </FONT></TD>'.
    new_line ' </TR>'.
  ENDLOOP.

  new_line ' </TABLE>'.
  new_line'  <h6>Este e-mail é automatizado</h6> '.
  new_line '</BODY>'.
  new_line '</HTML>'.

ENDFORM.
