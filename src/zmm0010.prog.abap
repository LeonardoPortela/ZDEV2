*&---------------------------------------------------------------------*
*& Report  ZMM0010
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmm0010.

*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
types: begin of ty_arq,
        iblnr like rm07i-iblnr,
        matnr like mara-matnr,
*        qtde  like iseg-erfmg,
        qtde(13),
       end of ty_arq,

       begin of ty_entrada,
        arq(45),
       end of ty_entrada.

*----------------------------------------------------------------------*
* TABELAS INTERNAS                                                     *
*----------------------------------------------------------------------*
data : begin of it_bdc occurs 0.
        include structure bdcdata.
data : end of it_bdc.

data : begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data : end of it_msg.

data: t_arq     type ty_arq occurs 0 with header line,
      t_entrada type ty_entrada occurs 0 with header line.

*----------------------------------------------------------------------*
* VARIÁVEIS                                                            *
*----------------------------------------------------------------------*
data: v_iblnr(10),
      v_matnr(18),
      v_qtde(13),
      v_gjahr like rm07i-gjahr,
      v_sytabix like sy-tabix,
      v_sy(2),
      v_campo(14),
      v_msg like sy-lisel,
      v_message_id like  sy-msgid,
      v_message_number like  sy-msgno,
      v_message_var1   like  sy-msgv1,
      v_message_var2   like  sy-msgv2,
      v_message_var3   like  sy-msgv3,
      v_message_var4   like  sy-msgv4.

*----------------------------------------------------------------------*
* SELEÇÃO DO ARQUIVO                                                   *
*----------------------------------------------------------------------*

selection-screen begin of block b1 with frame title text-tb1.

parameters: p_dt    like sy-datum        obligatory,
            p_gjahr like rm07i-gjahr     obligatory,
            p_arq   like rlgrap-filename obligatory.

selection-screen end of block b1.

at selection-screen on value-request for p_arq.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = '*.TXT'
      mode             = 'O'
      title            = 'Busca de Arquivo'
    importing
      filename         = p_arq
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  if not p_arq is initial.
    call function 'WS_UPLOAD'
      exporting
        filename                = p_arq
        filetype                = 'ASC'
      tables
        data_tab                = t_entrada
      exceptions
        conversion_error        = 1
        file_open_error         = 2
        file_read_error         = 3
        invalid_type            = 4
        no_batch                = 5
        unknown_error           = 6
        invalid_table_width     = 7
        gui_refuse_filetransfer = 8
        customer_error          = 9
        no_authority            = 10
        others                  = 11.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

start-of-selection.
  perform bi.

*--------------------------------------------------------------------*
* PERFORM TELA                                                       *
*--------------------------------------------------------------------*
form tela using    value(p_0255)
                   value(p_0256)
                   value(p_0257)
                   value(p_0258)
                   value(p_0259).

  it_bdc-dynbegin = p_0255.
  it_bdc-program  = p_0256.
  it_bdc-dynpro   = p_0257.
  it_bdc-fnam     = p_0258.
  it_bdc-fval     = p_0259.

  append it_bdc. "ADICIONA UMA LINHA
  clear it_bdc.  "LIMPA O HEADER

endform.                    " tela
*--------------------------------------------------------------------*
* PERFORM BI                                                         *
*--------------------------------------------------------------------*
form bi .
  loop at t_entrada.
    split t_entrada at ';' into: v_iblnr
                                 v_matnr
                                 v_qtde.
    t_arq-iblnr   = v_iblnr.
    t_arq-matnr   = v_matnr.
    t_arq-qtde    = v_qtde.
    append t_arq.
    clear: t_arq.
  endloop.

  loop at t_arq.
  endloop.

  concatenate p_dt+6(2)
              p_dt+4(2)
              p_dt(4)
              into p_dt.

  perform tela using:
    'X'  'SAPMM07I' '0701' ' '              ' '               ,
    ' '  ' '        ' '    'RM07I-IBLNR'    t_arq-iblnr       ,
    ' '  ' '        ' '    'RM07I-GJAHR'    p_gjahr           ,
    ' '  ' '        ' '    'RM07I-ZLDAT'    p_dt              ,
    ' '  ' '        ' '    'BDC_OKCODE'     '/00'             ,

    'X'  'SAPMM07I' '0731' ' '              ' '               .

  clear v_sy.

  loop at t_arq.
*      V_SYTABIX = SY-TABIX.
    add 1 to v_sytabix.

    if not v_sytabix > 14.
      add 1 to v_sy.

    else.
      v_sytabix = 1.
      clear v_sy.
      add 1 to v_sy.
      perform tela using:
       'X'  'SAPMM07I' '0721' ' '              ' '               ,
       ' '  ' '        ' '    'BDC_OKCODE'     'P+'              .
    endif.
    concatenate 'ISEG-ERFMG('
                v_sy
                ')'
                into v_campo.

    perform tela using:
     ' '  ' '        ' '    v_campo          t_arq-qtde       .

  endloop.

  perform tela using:
       ' '  ' '        ' '    'BDC_OKCODE'     '/00'             ,

       'X'  'SAPMM07I' '0731' ' '              ' '               ,
       ' '  ' '        ' '    'BDC_OKCODE'     '=BU'             .

  data: opt type ctu_params.
  opt-dismode = 'A'.
  opt-defsize = 'X'.
  opt-defsize = 'X'.

  call transaction 'MI04'
  using it_bdc
  options from opt
  messages into it_msg.
  refresh it_bdc.

  loop at it_msg.
    if it_msg-msgtyp eq 'E' or
       it_msg-msgtyp eq 'S'.

      move: it_msg-msgid to v_message_id,
            it_msg-msgnr to v_message_number,
            it_msg-msgv1 to v_message_var1,
            it_msg-msgv2 to v_message_var2,
            it_msg-msgv3 to v_message_var3,
            it_msg-msgv4 to v_message_var4.

      call function 'RPY_MESSAGE_COMPOSE'
        exporting
          message_id        = v_message_id
          message_number    = v_message_number
          message_var1      = v_message_var1
          message_var2      = v_message_var2
          message_var3      = v_message_var3
          message_var4      = v_message_var4
        importing
          message_text      = v_msg
        exceptions
          message_not_found = 1
          others            = 2.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
      if it_msg-msgtyp eq 'E'.
        message id '00' type 'I' number '398'
            with v_msg.
      elseif it_msg-msgtyp eq 'S'.
        message id '00' type 'S' number '398'
            with v_msg.
      endif.

    endif.
  endloop.



endform.                    " BI
