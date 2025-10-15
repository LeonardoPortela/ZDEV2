FUNCTION zsd_email_baixas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(E_ENVIADO) TYPE  SOFOLENTI1-OBJECT_ID
*"  TABLES
*"      T_BAIXAS STRUCTURE  ZSTSD_EMAIL_BAIXAS
*"----------------------------------------------------------------------

*----------------------------------------------------------------------
* Declaração de Constantes
*----------------------------------------------------------------------
  CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

*----------------------------------------------------------------------
* Declaração de tabela Interna
*----------------------------------------------------------------------
  DATA: t_objtxt   TYPE TABLE OF solisti1,
        t_objpack  TYPE TABLE OF sopcklsti1,
        t_receiver TYPE TABLE OF somlreci1.

*----------------------------------------------------------------------
* Declaração de Estrutura
*----------------------------------------------------------------------
  DATA: w_docdata  TYPE sodocchgi1,
        w_objtxt   LIKE LINE OF t_objtxt,
        w_objpack  LIKE LINE OF t_objpack,
        w_receiver LIKE LINE OF t_receiver,
        w_address  TYPE usaddress.

*----------------------------------------------------------------------
* Declaração de Variáveis Local
*----------------------------------------------------------------------
  DATA: l_qtde(20)      TYPE c,
        l_aedat(10)     TYPE c,
        l_valor(20)     TYPE c,

        l_send_request  TYPE REF TO cl_bcs,
        l_sent_to_all   TYPE os_boolean,
        l_document      TYPE REF TO cl_document_bcs,
        l_recipient     TYPE REF TO if_recipient_bcs,
        l_bcs_exception TYPE REF TO cx_bcs,
        l_subject2      TYPE so_obj_des,
        l_subject       TYPE string,
        l_lines         TYPE i,

        l_sentall       TYPE sonv-flag,
        l_nobjid        TYPE sofolenti1-object_id.

  DATA: t_tline  TYPE STANDARD TABLE OF tline,
        w_header TYPE thead.

  CHECK t_baixas[] IS NOT INITIAL.

  "Buscar usuário de criação
  SELECT * FROM zsdt0277
   INTO TABLE @DATA(t_usuario)
   FOR ALL ENTRIES IN @t_baixas
    WHERE id_baixa = @t_baixas-id_baixa
      AND acao     = @abap_false.

  IF t_usuario[] IS NOT INITIAL.

    SORT t_usuario BY usuario id_baixa.
    DATA(t_user_criador) = t_usuario.

    DELETE ADJACENT DUPLICATES FROM t_usuario COMPARING usuario.

    "Validar se foi reprovado com sucesso
    SELECT * FROM zsdt0277
     INTO TABLE @DATA(t_log)
     FOR ALL ENTRIES IN @t_baixas
      WHERE id_baixa = @t_baixas-id_baixa
        AND docnum   = @t_baixas-docnum
        AND itmnum   = @t_baixas-itmnum
        AND acao     = 'R'.

  ENDIF.

  LOOP AT t_usuario INTO DATA(w_usuario).

    CLEAR: w_address.
    CALL FUNCTION 'SUSR_USER_READ'
      EXPORTING
        user_name            = w_usuario-usuario
      IMPORTING
        user_address         = w_address
      EXCEPTIONS
        user_name_not_exists = 1
        internal_error       = 2
        OTHERS               = 3.

    IF w_address-addrnumber IS NOT INITIAL.

      SELECT * FROM adr6
        INTO TABLE @DATA(t_email)
        WHERE addrnumber = @w_address-addrnumber
        AND   persnumber = @w_address-persnumber.

    ENDIF.

    IF t_email[] IS INITIAL.
      CONTINUE.
    ELSE.

      SORT t_email BY smtp_addr.
      DELETE ADJACENT DUPLICATES FROM t_email COMPARING smtp_addr.

      REFRESH: t_objpack, t_objtxt, t_receiver, t_tline.
      CLEAR w_header.
*----------------------------------------------------------------------
* Montar corpo do E-mail
*----------------------------------------------------------------------
      w_objtxt-line = '<BR>'.
      APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

      READ TABLE t_baixas INTO DATA(w_baixas) INDEX 1.

      w_objtxt-line = 'A(s) baixa(s) do Volume das NFs foram reprovadas.'.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      MOVE '<BR><BR>' TO  w_objtxt-line.
      APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

      "Busca a justificativa
      READ TABLE t_baixas INTO w_baixas INDEX 1.
      IF sy-subrc IS INITIAL.
        CLEAR: w_header.
        w_header-tdobject = 'ZOBSERVAC2'.
        CONCATENATE w_baixas-id_baixa w_baixas-docnum w_baixas-itmnum 'R' INTO w_header-tdname.

        w_header-tdid     = 'BXNF'.
        w_header-tdspras  = sy-langu.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id        = w_header-tdid
            language  = w_header-tdspras
            name      = w_header-tdname
            object    = w_header-tdobject
          TABLES
            lines     = t_tline
          EXCEPTIONS
            id        = 1
            language  = 2
            name      = 3
            not_found = 4
            OTHERS    = 5.

        MOVE '<TABLE>' TO w_objtxt-line.
        APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.


        DATA: l_motivo(10) TYPE c VALUE 'Motivo:'.

        LOOP AT t_tline INTO DATA(w_tline).

          CONCATENATE '<TR> <TH>' l_motivo '</TH>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

          CONCATENATE '<TD>' w_tline-tdline '</TD>' INTO w_objtxt-line SEPARATED BY space.
          APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

          CLEAR l_motivo.

        ENDLOOP.

        MOVE '</TABLE>' TO w_objtxt-line.
        APPEND w_objtxt TO t_objtxt.  CLEAR w_objtxt.

      ENDIF.

      MOVE '<BR><BR>' TO  w_objtxt-line.
      APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

      MOVE '<TABLE BORDER=1>' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col1
      MOVE '<TR style="background-color: #96A5AA;"> <TH> ID </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col2
      MOVE '<TH> Empresa </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col3
      MOVE '<TH> Centro </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col4
      MOVE '<TH> Docnum </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col5
      MOVE '<TH> Item </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col6
      MOVE '<TH> Nro. NF </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col7
      MOVE '<TH> Dt. Criação </TH>' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col8
      MOVE '<TH> Dt. Emissão'  TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col9
      MOVE '<TH> Parceiro </TH>' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      "col10
      MOVE '<TH> Nome parceiro </TH>' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col11
      MOVE '<TH> Estado </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col12
      MOVE '<TH> Material </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col123
      MOVE '<TH> Desc. Material </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col14
      MOVE '<TH> Qtde. NF (kg) </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col15
      MOVE '<TH> Qtde. Baixada </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col14
      MOVE '<TH> Baixa Volume ? </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col15
      MOVE '<TH> Valor Pago </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col16
      MOVE '<TH> Data Baixa </TH> ' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

      CLEAR w_baixas.
      LOOP AT t_baixas INTO w_baixas.

        READ TABLE t_user_criador TRANSPORTING NO FIELDS WITH KEY id_baixa = w_baixas-id_baixa
                                                                   usuario  = w_usuario-usuario.

        CHECK sy-subrc IS INITIAL.

        READ TABLE t_log ASSIGNING FIELD-SYMBOL(<fs_log>) WITH KEY id_baixa = w_baixas-id_baixa
                                                                   docnum   = w_baixas-docnum
                                                                   itmnum   = w_baixas-itmnum.

        IF sy-subrc IS INITIAL.

          "Preenche tabela de log de e-mail
          <fs_log>-email = abap_true.
*          APPEND <fs_zsdt0277> TO t_log.

          IF w_baixas-id_baixa IS NOT INITIAL.
            CONCATENATE '<TR> <TD>' w_baixas-id_baixa '</TD>' INTO w_objtxt-line.
            APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.
          ELSE.
            MOVE '<TR> <TD></TD>' TO w_objtxt-line.
            APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.
          ENDIF.

*col2
          CONCATENATE '<TD>' w_baixas-bukrs '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col3
          CONCATENATE '<TD>' w_baixas-branch '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col4
          CONCATENATE '<TD>' w_baixas-docnum '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col5
          CONCATENATE '<TD>' w_baixas-itmnum '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col7
          CONCATENATE '<TD>' w_baixas-nfenum '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col8
          WRITE  w_baixas-credat TO l_aedat USING EDIT MASK '__/__/____'.
          CONCATENATE '<TD>' l_aedat '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col9
          WRITE  w_baixas-docdat TO l_aedat USING EDIT MASK '__/__/____'.
          CONCATENATE '<TD>' l_aedat '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col11
          CONCATENATE '<TD>' w_baixas-parid '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col12
          CONCATENATE '<TD>' w_baixas-name1 '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col13
          CONCATENATE '<TD>' w_baixas-regio '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col14
          CONCATENATE '<TD>' w_baixas-matnr '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

*col15
          CONCATENATE '<TD>' w_baixas-maktx '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

**col7
          CLEAR: l_qtde.
          l_qtde = w_baixas-qtde_nf.
          REPLACE '.'  WITH ',' INTO  l_qtde.
          CONDENSE l_qtde.

          CONCATENATE '<TD>' l_qtde '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.
*
          CLEAR: l_qtde.
          l_qtde = w_baixas-qtde_baixa.
          REPLACE '.'  WITH ',' INTO  l_qtde.
          CONDENSE l_qtde.

          CONCATENATE '<TD>' l_qtde '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

          IF w_baixas-baixar IS NOT INITIAL.
            CONCATENATE '<TD>' 'Sim </TD>' INTO w_objtxt-line.
          ELSE.
            CONCATENATE '<TD>' 'Não </TD>' INTO w_objtxt-line.
          ENDIF.

          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

**col7
          l_valor = w_baixas-valor_pago.
          CONCATENATE '<TD>' l_valor '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.
*
**col7
          CLEAR l_aedat.
          WRITE  w_baixas-dt_baixa TO l_aedat USING EDIT MASK '__/__/____'.
          CONCATENATE '<TD>' l_aedat '</TD>' INTO w_objtxt-line.
          APPEND w_objtxt TO t_objtxt.   CLEAR w_objtxt.

        ENDIF.

      ENDLOOP.

      MOVE '</TABLE>' TO w_objtxt-line.
      APPEND w_objtxt TO t_objtxt.  CLEAR w_objtxt.



      MOVE '<BR><BR>' TO  w_objtxt-line.
      APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

      CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO w_objtxt-line SEPARATED BY space.
      APPEND w_objtxt TO t_objtxt. CLEAR w_objtxt.

*----------------------------------------------------------------------
* Cabeçalho do Email
*----------------------------------------------------------------------
      MOVE 'Baixa do Volume de NFs de Compra foi Reprovado'(015) TO w_docdata-obj_descr.
      w_docdata-sensitivty = 'F'.

      DESCRIBE TABLE t_objtxt LINES l_lines.
      IF NOT l_lines IS INITIAL.
        w_docdata-doc_size = l_lines * 255.
      ENDIF.

*----------------------------------------------------------------------
* Enviar e-mail
*----------------------------------------------------------------------
      CLEAR w_objpack.
      w_objpack-transf_bin = ''.
      w_objpack-head_start = 1.
      w_objpack-head_num = 0.
      w_objpack-body_start = 1.
      w_objpack-body_num = l_lines.
      w_objpack-doc_type = 'HTM'.
      w_objpack-doc_size = ( l_lines - 1 ) * 255 + strlen( w_objtxt ).
      APPEND w_objpack TO t_objpack.

      IF t_email[] IS NOT INITIAL.

        LOOP AT t_email INTO DATA(w_email).
          w_receiver-receiver = w_email-smtp_addr.
          w_receiver-rec_type = 'U'.
          w_receiver-com_type = c_comtyp_int.
          APPEND w_receiver TO t_receiver.
          CLEAR w_receiver.
        ENDLOOP.

      ENDIF.

      CHECK t_receiver[] IS NOT INITIAL.

      CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
        EXPORTING
          document_data              = w_docdata
          commit_work                = 'X'
        IMPORTING
          sent_to_all                = l_sentall
          new_object_id              = l_nobjid
        TABLES
          packing_list               = t_objpack[]
          contents_txt               = t_objtxt[]
          receivers                  = t_receiver[]
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.

      IF l_nobjid IS NOT INITIAL.
        e_enviado = abap_true.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF t_log[] IS NOT INITIAL.
    MODIFY zsdt0277 FROM TABLE t_log.
    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
