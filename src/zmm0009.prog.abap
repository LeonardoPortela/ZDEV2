*&---------------------------------------------------------------------*
*& Report  ZMM0009
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report  zmm0009.

*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
types: begin of ty_mard,
        matnr like mard-matnr,
        werks like mard-werks,
        lgort like mard-lgort,
        lgpbe like mard-lgpbe,
       end of ty_mard,

       begin of ty_makt,
        matnr like makt-matnr,
        maktx like makt-maktx,
       end of ty_makt,

       begin of ty_arq,
        iblnr like rm07i-iblnr,
        werks like mard-werks,
        lgort like mard-lgort,
        matnr like mard-matnr,
        lgpbe like mard-lgpbe,
        maktx like makt-maktx,
       end of ty_arq,

       begin of ty_saida,
        dados(90),
       end of ty_saida.

*----------------------------------------------------------------------*
* TABELAS INTERNAS                                                     *
*----------------------------------------------------------------------*
data : begin of it_bdc occurs 0.
        include structure bdcdata.
data : end of it_bdc.

data : begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data : end of it_msg.

data: t_mard  type ty_mard   occurs 0 with header line,
      t_makt  type ty_makt   occurs 0 with header line,
      t_arq   type ty_arq    occurs 0 with header line,
      t_saida type ty_saida occurs 0 with header line.

*----------------------------------------------------------------------*
* VARIÁVEIS                                                            *
*----------------------------------------------------------------------*
data: v_sytabix like sy-tabix,
      v_sy(2) type n,
      v_campo(14),
      v_msg like sy-lisel,
      v_message_id like  sy-msgid,
      v_message_number like  sy-msgno,
      v_message_var1   like  sy-msgv1,
      v_message_var2   like  sy-msgv2,
      v_message_var3   like  sy-msgv3,
      v_message_var4   like  sy-msgv4,
      v_arq            like rlgrap-filename,
      v_cont type i.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO                                                      *
*----------------------------------------------------------------------*

selection-screen begin of block b1 with frame title text-tb1.

parameters: p_dtdoc  like sy-datum         obligatory,
            p_dtcont like sy-datum         obligatory,
            p_werks  like mard-werks       obligatory,
            p_lgort  like mard-lgort       obligatory,
            p_lgpbe  like rlgrap-filename  obligatory.

selection-screen end of block b1.

at selection-screen on value-request for p_lgpbe.

  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = '*.TXT'
      mode             = 'S'
      title            = 'Busca de Arquivo'
    importing
      filename         = p_lgpbe
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  perform seleciona.
  perform bi.
*--------------------------------------------------------------------*
* PERFORM SELECIONA                                                  *
*--------------------------------------------------------------------*
form seleciona .

  select matnr werks lgort lgpbe
  from mard
       into table t_mard
  where werks = p_werks
    and lgort = p_lgort.
  sort t_mard by matnr lgort werks.

  if not t_mard[] is initial.
    select matnr maktx
    from makt
         into table t_makt
         for all entries in t_mard
    where matnr = t_mard-matnr.
    sort t_makt by matnr maktx.

    loop at t_mard.
      read table t_makt with key matnr = t_mard-matnr
                                 binary search.
      if sy-subrc eq 0.
        move: t_mard-werks to t_arq-werks,
              t_mard-lgort to t_arq-lgort,
              t_mard-matnr to t_arq-matnr,
              t_mard-lgpbe to t_arq-lgpbe,
              t_makt-maktx to t_arq-maktx.
        append t_arq.
        clear t_arq.
      endif.
    endloop.
  endif.

endform.                    " SELECIONA

*--------------------------------------------------------------------*
* PERFORM BI                                                         *
*--------------------------------------------------------------------*
form bi .
  if not t_arq[] is initial.

    perform tela using:
      'X'  'SAPMM07I' '0700' ' '              ' '               ,
      ' '  ' '        ' '    'IKPF-WERKS'     p_werks           ,
      ' '  ' '        ' '    'IKPF-LGORT'     p_lgort           ,
      ' '  ' '        ' '    'BDC_OKCODE'     '/00'             .

    perform tela using:
     'X'  'SAPMM07I' '0721' ' '              ' '               .

    clear v_sy.

    loop at t_arq.
      if v_cont <= 900.
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
        concatenate 'ISEG-MATNR('
                    v_sy
                    ')'
                    into v_campo.
        perform tela using:
         ' '  ' '        ' '    v_campo          t_arq-matnr       .

      elseif v_cont > 900.

        perform tela using:
         ' '  ' '        ' '    'BDC_OKCODE'     '/00'             ,
         'X'  'SAPMM07I' '0721' ' '              ' '               ,
         ' '  ' '        ' '    'BDC_OKCODE'     '=BU'             .

        perform call_transaction.

        perform tela using:
         'X'  'SAPMM07I' '0700' ' '              ' '               ,
         ' '  ' '        ' '    'IKPF-WERKS'     p_werks           ,
         ' '  ' '        ' '    'IKPF-LGORT'     p_lgort           ,
         ' '  ' '        ' '    'BDC_OKCODE'     '/00'             .

        perform tela using:
         'X'  'SAPMM07I' '0721' ' '              ' '               .

        clear v_sy.
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
        concatenate 'ISEG-MATNR('
                    v_sy
                    ')'
                    into v_campo.
        perform tela using:
         ' '  ' '        ' '    v_campo          t_arq-matnr       .

        v_cont = 1.
      endif.
    endloop.

    perform tela using:
     ' '  ' '        ' '    'BDC_OKCODE'     '/00'               ,

     'X'  'SAPMM07I' '0721' ' '              ' '               ,
     ' '  ' '        ' '    'BDC_OKCODE'     '=BU'             .

    perform call_transaction.


  else.
    message id '00' type 'I' number '398'
        with 'Dados não encontrados.'.
  endif.
endform.                    " BI

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
* PERFORM GERA_SAIDA                                                 *
*--------------------------------------------------------------------*
form gera_saida .

  call function 'WS_DOWNLOAD'
    exporting
      filename                = p_lgpbe
      filetype                = 'ASC'
    tables
      data_tab                = t_saida
    exceptions
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
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


endform.                    " GERA_SAIDA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_transaction .
  data: opt type ctu_params.
  opt-dismode = 'E'.
  opt-defsize = 'X'.
  opt-defsize = 'X'.

  call transaction 'MI01'
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

        loop at t_arq.
          move: it_msg-msgv1 to t_arq-iblnr.
          modify t_arq.

          concatenate t_arq-iblnr
                      ';'
                      t_arq-werks
                      ';'
                      t_arq-lgort
                      ';'
                      t_arq-matnr
                      ';'
                      t_arq-lgpbe
                      ';'
                      t_arq-maktx
                      into t_saida-dados.
          append t_saida.
          clear  t_saida.
        endloop.
        perform gera_saida.
      endif.

    endif.
  endloop.
endform.                    " CALL_TRANSACTION
