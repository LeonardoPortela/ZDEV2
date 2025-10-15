FUNCTION zfpp_email_lista_tecnica .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_ENVIADO) TYPE  SOFOLENTI1-OBJECT_ID
*"  TABLES
*"      TL_LISTA STRUCTURE  ZPPT0025
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_ENVIADO) TYPE  SOFOLENTI1-OBJECT_ID
*"  TABLES
*"      TL_LISTA STRUCTURE  ZPPT0025
*----------------------------------------------------------------------
* Declaração de Constantes
*----------------------------------------------------------------------
  CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

*----------------------------------------------------------------------
* Declaração de tipos
*----------------------------------------------------------------------
  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt0025.
           TYPES: maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
         END OF y_itens.

*----------------------------------------------------------------------
* Declaração de tabela Interna
*----------------------------------------------------------------------
  DATA: tl_objtxt   TYPE TABLE OF solisti1,
        tl_objpack  TYPE TABLE OF sopcklsti1,
        tl_receiver TYPE TABLE OF somlreci1.

  DATA: tl_itens TYPE TABLE OF y_itens.


*----------------------------------------------------------------------
* Declaração de Estrutura
*----------------------------------------------------------------------
  DATA: wl_docdata  TYPE sodocchgi1,
        wl_objtxt   LIKE LINE OF tl_objtxt,
        wl_objpack  LIKE LINE OF tl_objpack,
        wl_receiver LIKE LINE OF tl_receiver.

  DATA: wl_itens LIKE LINE OF tl_itens.
*----------------------------------------------------------------------
* Declaração de Variáveis
*----------------------------------------------------------------------
  DATA: lv_bukrs            TYPE bukrs,
        lv_rec_cnt          TYPE c LENGTH 10,
        lv_lines            TYPE i,
        lv_sentall          TYPE sonv-flag,
        lv_nobjid           TYPE sofolenti1-object_id,
        lv_aedat(10)        TYPE c,
        lv_hr_alteracao(10) TYPE c,
        lv_qtd_antiga(20)   TYPE c,
        lv_qtd_atual(20)    TYPE c.

  DATA: send_request  TYPE REF TO cl_bcs,
        sent_to_all   TYPE os_boolean,
        document      TYPE REF TO cl_document_bcs,
        recipient     TYPE REF TO if_recipient_bcs,
        bcs_exception TYPE REF TO cx_bcs,

        wl_subject2   TYPE so_obj_des,
        wl_subject    TYPE string.

  CHECK tl_lista[] IS NOT INITIAL.

* Email cadastrados para controle de Modif. Lista Técnica
  SELECT * FROM zppt0024
    INTO TABLE @DATA(tl_email)
    FOR ALL ENTRIES IN @tl_lista
    WHERE werks = @tl_lista-werks.

  SORT tl_email BY email.
  DELETE ADJACENT DUPLICATES FROM tl_email COMPARING email.

  LOOP AT tl_lista INTO DATA(wl_lista).

    MOVE-CORRESPONDING wl_lista TO wl_itens.

* Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.

      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.

    ENDIF.

* Busca descrição do Material
    IF wl_itens-matnr IS NOT INITIAL.

      SELECT SINGLE maktx FROM makt
        INTO (wl_itens-maktx)
        WHERE matnr = wl_itens-matnr.

    ENDIF.

* Busca descrição de Lista Técnica
    IF wl_itens-idnrk IS NOT INITIAL.

      SELECT SINGLE maktx FROM makt
        INTO (wl_itens-stlnr_txt)
        WHERE matnr = wl_itens-idnrk.

    ENDIF.

    APPEND wl_itens TO tl_itens.
    CLEAR wl_itens.

  ENDLOOP.
*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
        INTO wl_objtxt-line SEPARATED BY space.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Cód. Lista técnica </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Util.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Item </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Componente </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH> Descrição </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Tipo </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Qtde Antiga </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col9
  MOVE '<TH> Qtde Atual </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col10
  MOVE '<TH> Resp. modif. </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col11
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col12
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  LOOP AT tl_itens INTO wl_itens.

*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    CONCATENATE '<TD>' wl_itens-matnr '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-stlan '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-posnr '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
    CONCATENATE '<TD>' wl_itens-idnrk '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
    CONCATENATE '<TD>' wl_itens-stlnr_txt '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
    CONCATENATE '<TD>' wl_itens-tipo '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
    CLEAR: lv_qtd_antiga.
    lv_qtd_antiga = wl_itens-qtd_antiga.

    REPLACE '.'  WITH ',' INTO  lv_qtd_antiga.
    CONDENSE lv_qtd_antiga.

    CONCATENATE '<TD>' lv_qtd_antiga '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col9
    CLEAR: lv_qtd_atual.
    lv_qtd_atual = wl_itens-qtd_atual.

    REPLACE '.'  WITH ',' INTO  lv_qtd_atual.
    CONDENSE lv_qtd_atual.

    CONCATENATE '<TD>'  lv_qtd_atual  '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col10
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col11
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col12
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.



  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  IF tl_email[] IS NOT INITIAL.

    LOOP AT tl_email INTO DATA(wl_email).
      wl_receiver-receiver = wl_email-email.
      wl_receiver-rec_type = 'U'.
      wl_receiver-com_type = c_comtyp_int.
      APPEND wl_receiver TO tl_receiver.
      CLEAR wl_receiver.
    ENDLOOP.

  ENDIF.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF lv_nobjid IS NOT INITIAL.
    e_enviado = abap_true.
  ENDIF.

ENDFUNCTION.
