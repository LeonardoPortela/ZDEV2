report  zcuia.

*&---------------------------------------------------------------------*
*&      ESTRUTURA
*&---------------------------------------------------------------------*
types:

       begin of ty_linha,
         linha(1000) type c,
       end of ty_linha,

        begin of ty_dados,
         nf(9)     type c,
         nf_r(10)  type c,
       end of ty_dados.

*&---------------------------------------------------------------------*
*&      VARIAVEIS
*&---------------------------------------------------------------------*
data: v_folder type string.

data: it_file     type table of ty_linha,
      wa_linha    like line of it_file,
      wa_dados    type ty_dados,
      v_file_name type string,
      v_file      type string,
     doc_number   like  j_1bnfdoc-docnum,
     doc_number_r like  j_1bnfdoc-docnum.

*&---------------------------------------------------------------------*
*&      PARAMETROS
*&---------------------------------------------------------------------*
selection-screen begin of block b with frame title text-001.
parameters: p_dir   type filename-fileextern,
            p_data  type sy-datum.
selection-screen end of block b.


*&---------------------------------------------------------------------*
*&      SELECTION SCREEN
*&---------------------------------------------------------------------*
at selection-screen on value-request for p_dir.
  perform: f_busca_arquivo changing p_dir.

start-of-selection.
  perform: f_get_files.


*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_ARQUIVO
*&---------------------------------------------------------------------*
form f_busca_arquivo  changing p_dir.
* Chamada da janela para busca de diretório
  call method cl_gui_frontend_services=>directory_browse
    exporting
      window_title         = 'Estorno de nota fiscal'
      initial_folder       = 'C:/nf/'
    changing
      selected_folder      = v_folder
    exceptions
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      others               = 4.

* Se não executou abertura com sucesso
  if sy-subrc ne 0.

*   Msg: Erro na busca do diretório.
    message i836(sd) with text-002.

* Retornando o diretório ao campo da tela de seleção
  else.
    move v_folder to p_dir.
  endif.
endform.                    " F_BUSCA_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  F_GET_FILES
*&---------------------------------------------------------------------*
form f_get_files .

  v_file_name = p_dir.

  concatenate v_file_name '\nota.txt' into v_file.

  call function 'GUI_UPLOAD'
    exporting
      filename = v_file
      filetype = 'ASC'
    tables
      data_tab = it_file.
  if sy-subrc <> 0.

    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  perform: f_estornar.
endform.                    " F_GET_FILES


*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR
*&---------------------------------------------------------------------*
form f_estornar .

  loop at it_file into wa_linha.
    move wa_linha to wa_dados.

    doc_number = wa_dados-nf.

    call function 'J_1B_NF_DOCUMENT_CANCEL'
      exporting
        doc_number               = doc_number
        ref_type                 = space
        ref_key                  = space
        can_dat                  = p_data
      importing
        doc_number               = doc_number_r
      exceptions
        document_not_found       = 1
        cancel_not_possible      = 2
        nf_cancel_type_not_found = 3
        database_problem         = 4
        docum_lock               = 5
        nfe_cancel_simulation    = 6
        others                   = 7.

    if not sy-subrc is initial.
      message id sy-msgid type 'W' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.

      wa_dados-nf_r = doc_number_r.

      if not wa_dados-nf_r is initial.
        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.
        "append wa_dados-nf to it_docnum_r.
      else.
        call function 'BAPI_TRANSACTION_ROLLBACK'.
      endif.
    endif.
  endloop.
endform.                    " F_ESTORNAR
