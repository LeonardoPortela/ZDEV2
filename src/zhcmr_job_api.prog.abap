*&---------------------------------------------------------------------*
*& Report  ZHCMR_JOB_API
*&
*&---------------------------------------------------------------------*
*& Verifica se a API no Servidor SRVVM162 esta ativo
*&
*&---------------------------------------------------------------------*

REPORT zhcmr_job_api.

DATA: i_cpf	TYPE string,
      value TYPE string.

SELECT SINGLE *
  FROM zmail
  INTO @DATA(vl_zmail)
  WHERE tcode = @sy-cprog.

TRY .
    zcl_active_directory=>zif_active_directory~get_instance(
      )->get_infos_usuario( EXPORTING i_cpf = i_cpf IMPORTING e_info = DATA(e_resposta_infos_usuario) ).
  CATCH zcx_active_directory INTO DATA(erro).

    value = erro->get_text( ).
    PERFORM envia_email USING value vl_zmail-email.

ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email USING p_value p_mail.

  DATA : obj_mime_helper TYPE REF TO cl_gbt_multirelated_service,
         obj_bcs         TYPE REF TO cl_bcs,
         obj_doc_bcs     TYPE REF TO cl_document_bcs,
         obj_recipient   TYPE REF TO if_recipient_bcs,
         w_status        TYPE bcs_rqst,
         is_soli         TYPE soli,
         it_soli         TYPE TABLE OF soli.

  CREATE OBJECT obj_mime_helper.


  MOVE '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 2.0//EN”>' TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE '<HTML>' TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE '<BODY>' TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE '<P> O servidor SRVVM162 de API AD nao esta respondendo!</P>' TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE p_value TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE '</BODY>' TO is_soli.
  APPEND is_soli TO it_soli.
  MOVE '</HTML>' TO is_soli.
  APPEND is_soli TO it_soli.

* Set the HTML body of the mail
  CALL METHOD obj_mime_helper->set_main_html
    EXPORTING
      content     = it_soli
*     filename    = ”
      description = 'Erro na API do Servidor ZSRVM162'.

* Set the subject of the mail.
  obj_doc_bcs = cl_document_bcs=>create_from_multirelated(
                  i_subject          = 'Sem Comunicação na Api python SRVVM162'
                  i_importance       = '9'"                ” 1 / 5 / 9
                  i_multirel_service = obj_mime_helper ).
  obj_bcs = cl_bcs=>create_persistent( ).
  obj_bcs->set_document(
             i_document = obj_doc_bcs ).

* Set the email address
  obj_recipient = cl_cam_address_bcs=>create_internet_address(
                    i_address_string =  CONV #( p_mail ) ).
  obj_bcs->add_recipient(
             i_recipient = obj_recipient ).

* Change the status.
  MOVE 'N' TO w_status.
  CALL METHOD obj_bcs->set_status_attributes
    EXPORTING
      i_requested_status = w_status.

* Send the mail.
  obj_bcs->send( ).

* Commit Work.
  IF sy-subrc IS INITIAL.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.
