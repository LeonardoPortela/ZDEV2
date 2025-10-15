************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 09.05.2007                                          *
* Tipo de prg ...: Carga de Dados
* Objetivo    ...: Coletar dados de bancos em excel ou arquivo TXT e   *
*                  efetuar a carga destes no R/3                       *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 09.05.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
report zfib006 line-size 150
               line-count 62(03)
               message-id z01 no standard page heading.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
*
include <icon>.

tables bnka.

field-symbols <icone> like icon_checked.

data: v_ctrlcol1         type alsmex_tabline-value,
      v_icon             type c.

*----------------------------------------------------------------------*
* Declaração para Batch_input de determinação de CFOP
*----------------------------------------------------------------------*
*
data: wa_bdcdata         like bdcdata,
      wa_messtab         like bdcmsgcoll,
      t_bdcdata          like standard table of wa_bdcdata,
      t_messtab          like standard table of wa_messtab.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
constants: c_ncol_begin  like sy-index value 01,
           c_ncol_end    like sy-index value 40,
           c_nline       like sy-index value 01.

data: t_planilha         like alsmex_tabline occurs 0 with header line.

data: begin of wa_bancos,
        line             like sy-tabix,
        banks            like t_planilha-value, "Código do país do banco
        bankl            like t_planilha-value, "Chave do banco
        banka            like t_planilha-value, "Nome inst. financeira
        provz            like t_planilha-value, "Região (país, estado)
        stras            like t_planilha-value, "Rua e nº
        ort01            like t_planilha-value, "Cidade
        brnch            like t_planilha-value, "Agência do banco
      end   of wa_bancos,

      begin of wa_arqtxt,
        banks(03),
        bankl(15),
        banka(60),
        provz(03),
        stras(35),
        ort01(35),
        brnch(40),
      end   of wa_arqtxt,

      t_bancos           like standard table of wa_bancos,
      t_arqtxt           like standard table of wa_arqtxt.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
*
selection-screen function key 1.
selection-screen begin of block b0 with frame title text-s01.
parameters: p_cami   like rlgrap-filename obligatory,  "Arquivo Excel
            p_nline  like sy-index        obligatory.  "Nro aprox Linhas
selection-screen end   of block b0.

selection-screen begin of block b2 with frame title text-s02.
parameters: p_optxt  radiobutton group arq default 'X',
            p_opxls  radiobutton group arq.
selection-screen end   of block b2.

include zbci001.

*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
initialization.
  set titlebar 'TITLE01'.
  if p_nline is initial.
    p_nline = 9999.
  endif.
  ini_modo_bi.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
at selection-screen.
  check_modo_bi.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
at selection-screen on value-request for p_cami.
  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = p_cami
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    importing
      filename         = p_cami
    exceptions
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.


*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
start-of-selection.
  perform f_processa_planilhas.
  sort t_bancos by line.

  loop at t_bancos into wa_bancos.
    perform f_bank_create.
  endloop.

*----------------------------------------------------------------------*
* Event end-of-selection
*----------------------------------------------------------------------*
*
end-of-selection.
  uline.

*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
top-of-page.
  uline.
  write: /01 sy-vline,
          30 'Log de erros de importação - Cargas de Bancos  ',
          99 'Data.:',
         106 sy-datum,
         119 'Hora.:',
         126 sy-uzeit,
         137 'Pag.:',
         143 sy-pagno,
         150 sy-vline.
  uline.
  write: /01 sy-vline,
          03 'Planilha de Bancos                    :',
          44 p_cami,
         136 'Linhas:',
         144 p_nline,
         150 sy-vline.
  write: /01 sy-vline,
          03 'Usuário:',
          12 sy-uname,
          44 'Programa:',
          54 sy-repid,
         150 sy-vline.
  uline.
  write: /01 sy-vline,
          05 'Linha do Excel',
          23 'Chave do banco',
          50 'Mensagem do log',
         150 sy-vline.
  uline.

*----------------------------------------------------------------------*
* Event END_OF_PAGE.
*----------------------------------------------------------------------*
*
end-of-page.
  uline.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilhas
*&---------------------------------------------------------------------*
form f_processa_planilhas.

*
* Carrega planilha com dados de clientes
*
  data: vl_subrc like sy-subrc,
        vl_cami  type string,
        vl_line  like sy-tabix.

  refresh t_bancos.


  if ( p_optxt eq 'X' ).
*
* Carrega arquivo texto com dados de clientes
*
    refresh t_arqtxt.
    vl_cami = p_cami.

    call function 'GUI_UPLOAD'
      exporting
        filename                = vl_cami
      tables
        data_tab                = t_arqtxt
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        others                  = 22.

    if ( sy-subrc ne 0 ).
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      exit.
    endif.

    clear vl_line.
    loop at t_arqtxt into wa_arqtxt.
      vl_line = vl_line + 1.
      check ( vl_line le p_nline ).
      clear wa_bancos.
      move-corresponding wa_arqtxt to wa_bancos.
      append wa_bancos to t_bancos.
    endloop.

  else.

*> Carrega planilha de roteiros

    clear   t_planilha.
    refresh t_planilha.
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = p_cami
        i_begin_col             = c_ncol_begin
        i_begin_row             = c_nline
        i_end_col               = c_ncol_end
        i_end_row               = p_nline
      tables
        intern                  = t_planilha
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.

    if sy-subrc <> 0.
      message e004 with text-001.
    endif.

    sort t_planilha by row col.

*> Move valores das células para ti de roteiros.
    loop at t_planilha.

      on change of t_planilha-row.
        v_ctrlcol1    = t_planilha-value.
        translate v_ctrlcol1 to upper case.
        clear wa_bancos.
        wa_bancos-line = t_planilha-row.
      endon.

      if v_ctrlcol1 eq 'CAB'.
        delete t_planilha.
        continue.
      endif.

      case t_planilha-col.
        when  2. wa_bancos-banks   = t_planilha-value.
        when  3. wa_bancos-bankl   = t_planilha-value.
        when  4.
          wa_bancos-banka   = t_planilha-value.
          translate wa_bancos-banka to upper case.
        when  5. wa_bancos-provz   = t_planilha-value.
        when  6.
          wa_bancos-stras   = t_planilha-value.
          translate wa_bancos-stras to upper case.
        when  7.
          wa_bancos-ort01   = t_planilha-value.
          translate wa_bancos-ort01 to upper case.
        when  8.
          wa_bancos-brnch   = t_planilha-value.
          translate wa_bancos-brnch to upper case.
      endcase.

      at end of row.
        append wa_bancos to t_bancos.
      endat.
    endloop.

  endif.
endform.                    " f_processa_planilha


*&---------------------------------------------------------------------*
*&      Form  f_bank_create
*&---------------------------------------------------------------------*
form f_bank_create.
  data: v_mess_tab(256) type c.

  clear   : wa_bdcdata, wa_messtab.
  refresh : t_bdcdata, t_messtab.

  perform f_bdc_field using: 'X' 'SAPMF02B'              '0100',
                             ' ' 'BDC_OKCODE'            '/00',
                             ' ' 'BNKA-BANKS'           wa_bancos-banks,
                             ' ' 'BNKA-BANKL'           wa_bancos-bankl.

  perform f_bdc_field using: 'X' 'SAPMF02B'              '0110',
                             ' ' 'BDC_OKCODE'            '/00',
                             ' ' 'BNKA-BANKA'           wa_bancos-banka,
                             ' ' 'BNKA-PROVZ'           wa_bancos-provz,
                             ' ' 'BNKA-STRAS'           wa_bancos-stras,
                             ' ' 'BNKA-ORT01'           wa_bancos-ort01,
                             ' ' 'BNKA-BRNCH'           wa_bancos-brnch.

  perform f_bdc_field using: 'X' 'SAPMF02B'              '0110',
                             ' ' 'BDC_OKCODE'            '=UPDA'.

  call transaction 'FI01' using  t_bdcdata
                           mode  p_mod1
                         update  'S'
                       messages  into t_messtab.

  loop at t_messtab into wa_messtab.
    if 'WAE' cs wa_messtab-msgtyp.
      assign icon_incomplete to <icone>.
    elseif wa_messtab-msgtyp eq 'S'.
      assign icon_check      to <icone>.
    endif.

    sy-msgid = wa_messtab-msgid.
    sy-msgno = wa_messtab-msgnr.
    sy-msgv1 = wa_messtab-msgv1.
    sy-msgv2 = wa_messtab-msgv2.
    sy-msgv3 = wa_messtab-msgv3.
    sy-msgv4 = wa_messtab-msgv4.
    call function 'CUTC_GET_MESSAGE'
      exporting
        msg_id      = sy-msgid
        msg_no      = sy-msgno
        msg_arg1    = sy-msgv1
        msg_arg2    = sy-msgv2
        msg_arg3    = sy-msgv3
        msg_arg4    = sy-msgv4
        language    = sy-langu
      importing
        raw_message = v_mess_tab.
    perform f_imprime_erros  using v_mess_tab.
  endloop.

endform.                    " f_bank_create

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
form f_imprime_erros using    p_message.
  write: /01 sy-vline,
          03 <icone> as icon,
          08 wa_bancos-line,
          23 wa_bancos-bankl,
          50 p_message,
         150 sy-vline.
endform.                    " f_imprime_erros
*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
form f_bdc_field using    value(p_flag)
                          value(p_fnam)
                          value(p_fval).
  clear wa_bdcdata.
  if not p_flag is initial.
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
    wa_bdcdata-dynbegin = 'X'.
  else.
    wa_bdcdata-fnam = p_fnam.
    wa_bdcdata-fval = p_fval.
  endif.
  append wa_bdcdata to t_bdcdata.

endform.                    "f_bdc_field
