************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 17.04.2007                                          *
* Tipo de prg ...: Carga de Dados com bapi                             *
* Objetivo    ...: Coletar dados de titulos a pagar de fornecedores    *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 17.04.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
report zfib004 line-size 150
               line-count 62(03)
               message-id z01 no standard page heading.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
include <icon>.

constants: c_ncoln       like sy-index value 01,
           c_nline       like sy-index value 01.

data: v_mess_tab(256)    type c,
      v_ctrlcol1         type alsmex_tabline-value,
      v_ncoln            like sy-index value 60.

field-symbols <icone>    like icon_checked.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
data: t_planilha         like alsmex_tabline occurs 0 with header line.


types: begin of tt_txtheader,
         seqlan(06),       "Sequencial
         tiplin(01),       "Tipo de registro
         awkey(20),        "Chave de referencia
         bukrs(04),        "Empresa
         bktxt(25),        "Texto de cabecalho
         bldat(10),        "Data no documento
         gjahr(04),        "Exercício
         monat(02),        "Mês do Exercício
         blart(02),        "Tipo de documento
         xblnr(16),        "No.documento referencia
         budat(10),        "Data do lançamento
         filler(209),
       end   of tt_txtheader,

       begin of tt_txtitem,
         seqlan(06) type c,       "Sequencial
         tiplin(01),       "Tipo de registro
         bschl(02),        "Chave de lançamento
         hkont(10),        "Conta/Cliente/Fornecedor
         wrbtr(20),        "Montante em moeda
         zfbdt(10),        "Data de vencimento
         zlspr(01),        "Chave de bloqueio pagamento
         zlsch(01),        "Forma de pagamento
         kidno(30),        "Referencia
         sgtxt(50),        "Texto do Item
         xref1(12),        "Chave referencia parceiro
         xref2(12),        "Chave referencia parceiro
         xref3(20),        "Chave referencia item parceiro
         bupla(04),        "Filial
         zuonr(18),        "Atribuição
         umskz(01),        "Cod. razão especial
         kostl(10),        "Centro de custo
         aufnr(12),        "No. Ordem
         prctr(10),        "Centro de lucro
         gsber(04),        "Divisão
         waers(05),        "Moeda
         waers_i(05),      "Moeda Interna
         dmbtr(20),        "Montante em moeda interna
         waers_f(05),      "Moeda Forte
         dmbe2(20),        "Montanta em moeda forte
         bvtyp(04),        "Tipo de banco do parceiro
         hbkid(05),        "Chv breve banco da empresa
       end   of tt_txtitem.

data: wa_txtheader type tt_txtheader,
      wa_txtitem   type tt_txtitem.

data: begin of wa_xlsheader,
         line        like sy-tabix.                   "linha planilha
include type tt_txtheader.
data: end   of wa_xlsheader.

data: begin of wa_xlsitem,
         line        like sy-tabix.                   "linha planilha
include type tt_txtitem.
data: end   of wa_xlsitem.


data: wa_header      like zfie_documentheader,
      wa_item        like zfie_documentitem,
      wa_returnobj   like zfie_returnobj,
      wa_return      like zfie_return.

data: t_xlsheader    like standard table of wa_xlsheader,
      t_xlsitem      like standard table of wa_xlsitem,
      t_arqtxt(300)  type c occurs 0 with header line,

      t_header       like standard table of wa_header,
      t_item         like standard table of wa_item,
      t_returnobj    like standard table of wa_returnobj,
      t_return       like standard table of wa_return.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s01.
parameters: p_cami  like rlgrap-filename,            "Arquivo Excel
            p_nline like sy-index.                   "Nro aprox Linhas
selection-screen end   of block b0.

selection-screen begin of block b1 with frame title text-s02.
parameters: p_optxt  radiobutton group arq default 'X',
            p_opxls  radiobutton group arq.
selection-screen end   of block b1.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
at selection-screen.

*----------------------------------------------------------------------*
* Event at selection-Screen on value-request
*----------------------------------------------------------------------*
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

  perform f_processa_planilha.
  sort t_xlsheader by line seqlan.
  sort t_xlsitem   by line seqlan.

  if ( not t_xlsheader[] is initial ) and
     ( not t_xlsitem[]   is initial ).
    perform f_document_create.
  else.
    message w000 with text-m01.
  endif.

*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
initialization.
  set titlebar 'TITLE01'.
  if p_nline is initial.
    p_nline = 9999.
  endif.

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
          02 'Maggi - Projeto Crescer',
          35 'Log de erros de importação - Cargas Titulos AP/AR',
          99 'Data.:',
         106 sy-datum,
         119 'Hora.:',
         126 sy-uzeit,
         137 'Pag.:',
         143 sy-pagno,
         150 sy-vline.
  uline.
  write: /01 sy-vline,
          03 'Nome do arquivo importado:',
          30 p_cami,
         150 sy-vline.
  write: /01 sy-vline,
          03 'Nro de linhas do arquivo :',
          30 p_nline,
          99 'Usuário:',
         108 sy-uname,
         119 'Programa:',
         129 sy-repid,
         150 sy-vline.
  uline.


  write: /01 sy-vline,
          16 'SeqLan',
          26 'Chave de referencia',
          46 'Doc referencia',
          63 'Mensagem do log',
         150 sy-vline.
  uline.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilha
*&---------------------------------------------------------------------*
form f_processa_planilha.
*
* Carrega planilha com dados de clientes
*
  data: vl_bukrs like bkpf-bukrs,
        vl_subrc like sy-subrc,
        vl_cami  type string,
        vl_line  like sy-tabix.


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

    refresh: t_xlsheader, t_xlsitem.
    clear vl_line.

    loop at t_arqtxt into wa_txtheader.
      clear: wa_xlsheader, wa_xlsitem.
      vl_line = vl_line + 1.
      check ( vl_line le p_nline ).

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_txtheader-seqlan
        importing
          output = wa_xlsheader-seqlan.

      if ( wa_txtheader-tiplin eq 'H' ).
        move-corresponding wa_txtheader to wa_xlsheader.
        wa_xlsheader-line = vl_line.


        if ( not wa_txtheader-bldat is initial ).
          translate wa_txtheader-bldat using '/.'.
          call function 'CONVERT_DATE_TO_INTERNAL'
            exporting
              date_external = wa_txtheader-bldat
            importing
              date_internal = wa_xlsheader-bldat.
        endif.

        if ( not wa_txtheader-budat is initial ).
          translate wa_txtheader-budat using '/.'.
          call function 'CONVERT_DATE_TO_INTERNAL'
            exporting
              date_external = wa_txtheader-budat
            importing
              date_internal = wa_xlsheader-budat.
        endif.

        append wa_xlsheader to t_xlsheader.
      else.
        move wa_txtheader to wa_txtitem.

        translate wa_txtitem-wrbtr using '. ,.'.
        condense  wa_txtitem-wrbtr no-gaps.
        translate wa_txtitem-dmbtr using '. ,.'.
        condense  wa_txtitem-dmbtr no-gaps.
        translate wa_txtitem-dmbe2 using '. ,.'.
        condense  wa_txtitem-dmbe2 no-gaps.

        move-corresponding wa_txtitem   to wa_xlsitem.
        wa_xlsitem-line = vl_line.

        if ( not wa_txtitem-zfbdt is initial ).
          translate wa_txtitem-zfbdt using '/.'.
          call function 'CONVERT_DATE_TO_INTERNAL'
            exporting
              date_external = wa_txtitem-zfbdt
            importing
              date_internal = wa_xlsitem-zfbdt.
        endif.

        append wa_xlsitem   to t_xlsitem.
      endif.
    endloop.

  else.

    clear   t_planilha.
    refresh t_planilha.
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = p_cami
        i_begin_col             = c_ncoln
        i_begin_row             = c_nline
        i_end_col               = v_ncoln
        i_end_row               = p_nline
      tables
        intern                  = t_planilha
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.
    if sy-subrc <> 0.
      message e000 with text-m02.
    endif.

    sort t_planilha by row col.
    refresh: t_xlsheader, t_xlsitem.

*> Move valores das células
    loop at t_planilha.

      on change of t_planilha-row.
        v_ctrlcol1 = t_planilha-value.
      endon.
*> Elimina linhas de cabeçalho e texto da tabela interna.
      if v_ctrlcol1 eq 'CAB'.
        delete t_planilha.
        continue.
      endif.

      at new row.
        clear: wa_xlsheader, wa_xlsitem.
        wa_xlsheader-line = wa_xlsitem-line = t_planilha-row.
      endat.


      if ( t_planilha-col eq 2 ).
        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = t_planilha-value
          importing
            output = wa_xlsheader-seqlan.

        wa_xlsitem-seqlan = wa_xlsheader-seqlan.

      elseif ( t_planilha-col eq 3 ).
        wa_xlsheader-tiplin = wa_xlsitem-tiplin = t_planilha-value.
      elseif ( t_planilha-col gt 3 ).

        if ( wa_xlsheader-tiplin eq 'H' ).

          case t_planilha-col.
            when  4. wa_xlsheader-awkey = t_planilha-value.
            when  5. wa_xlsheader-bukrs = t_planilha-value.
            when  6. wa_xlsheader-bktxt = t_planilha-value.
            when  7.
              translate t_planilha-value using '/.'.
              call function 'CONVERT_DATE_TO_INTERNAL'
                exporting
                  date_external = t_planilha-value
                importing
                  date_internal = wa_xlsheader-bldat.


            when  8. wa_xlsheader-gjahr = t_planilha-value.
            when  9. wa_xlsheader-monat = t_planilha-value.
            when 10. wa_xlsheader-blart = t_planilha-value.
            when 11. wa_xlsheader-xblnr = t_planilha-value.
            when 12.
              translate t_planilha-value using '/.'.
              call function 'CONVERT_DATE_TO_INTERNAL'
                exporting
                  date_external = t_planilha-value
                importing
                  date_internal = wa_xlsheader-budat.

          endcase.

        else.

          case t_planilha-col.
            when  4. wa_xlsitem-bschl   = t_planilha-value.
            when  5. wa_xlsitem-hkont   = t_planilha-value.
            when  6.
              translate wa_xlsitem-wrbtr using '.,'.
              wa_xlsitem-wrbtr   = t_planilha-value.
            when  7.
              translate t_planilha-value using '/.'.
              call function 'CONVERT_DATE_TO_INTERNAL'
                exporting
                  date_external = t_planilha-value
                importing
                  date_internal = wa_xlsitem-zfbdt.

            when  8. wa_xlsitem-zlspr   = t_planilha-value.
            when  9. wa_xlsitem-zlsch   = t_planilha-value.
            when 10. wa_xlsitem-kidno   = t_planilha-value.
            when 11. wa_xlsitem-sgtxt   = t_planilha-value.
            when 12. wa_xlsitem-xref1   = t_planilha-value.
            when 13. wa_xlsitem-xref2   = t_planilha-value.
            when 14. wa_xlsitem-xref3   = t_planilha-value.
            when 15. wa_xlsitem-bupla   = t_planilha-value.
            when 16. wa_xlsitem-zuonr   = t_planilha-value.
            when 17. wa_xlsitem-umskz   = t_planilha-value.
            when 18. wa_xlsitem-kostl   = t_planilha-value.
            when 19. wa_xlsitem-aufnr   = t_planilha-value.
            when 20. wa_xlsitem-prctr   = t_planilha-value.
            when 21. wa_xlsitem-gsber   = t_planilha-value.
            when 22. wa_xlsitem-waers   = t_planilha-value.
            when 23. wa_xlsitem-waers_i = t_planilha-value.
            when 24. wa_xlsitem-dmbtr   = t_planilha-value.
            when 25. wa_xlsitem-waers_f = t_planilha-value.
            when 26. wa_xlsitem-dmbe2   = t_planilha-value.
            when 27. wa_xlsitem-bvtyp   = t_planilha-value.
            when 28. wa_xlsitem-hbkid   = t_planilha-value.
          endcase.

        endif.
      endif.

      at end of row.
        if ( wa_xlsheader-tiplin eq 'H' ).
          append wa_xlsheader to t_xlsheader.
        else.
          append wa_xlsitem to t_xlsitem.
        endif.
      endat.

    endloop.
  endif.
endform.                    "f_processa_planilha

*&---------------------------------------------------------------------*
*&      Form  f_document_create
*&---------------------------------------------------------------------*
form f_document_create .

  refresh: t_header, t_item, t_return, t_returnobj.

  loop at t_xlsheader into wa_xlsheader.

    clear wa_header.
    move-corresponding wa_xlsheader to wa_header.
    move wa_xlsheader-awkey         to wa_header-obj_key.
    append wa_header to t_header.

    loop at t_xlsitem into wa_xlsitem
                     where ( seqlan eq wa_xlsheader-seqlan ).

      clear wa_item.
      move-corresponding wa_xlsitem to wa_item.
      append wa_item to t_item.

    endloop.
  endloop.

  call function 'Z_FI_DOCUMENT_POST'
    tables
      it_documentheader = t_header
      it_documentitem   = t_item
      it_return         = t_return
      it_returnobj      = t_returnobj
    exceptions
      bschl_not_found   = 1
      others            = 2.

  if ( sy-subrc ne 0 ).
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  sort t_return by seqlan.
  sort t_header by seqlan.
  loop at t_return into wa_return.
    if ( 'WAE' cs wa_return-type ).
      assign icon_incomplete to <icone>.
    elseif ( wa_return-type eq 'S' ).
      assign icon_checked to <icone>.
    elseif ( wa_return-type eq 'I' ).
      assign icon_failure to <icone>.
    endif.

    sy-msgid = wa_return-id.
    sy-msgno = wa_return-number.
    sy-msgv1 = wa_return-message_v1.
    sy-msgv2 = wa_return-message_v2.
    sy-msgv3 = wa_return-message_v3.
    sy-msgv4 = wa_return-message_v4.

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

    read table t_header into wa_header
                    with key seqlan = wa_return-seqlan binary search.
    if ( sy-subrc ne 0 ). clear wa_header. endif.

    perform f_imprime_erros  using v_mess_tab.
  endloop.



endform.                    " f_document_create

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
form f_imprime_erros using    p_message.

  read table t_xlsheader into wa_xlsheader
                     with key seqlan = wa_return-seqlan binary search.
  if ( sy-subrc ne 0 ). clear wa_xlsheader. endif.

  write: /01 sy-vline,
          03 <icone> as icon,
          16 wa_header-seqlan,
          26 wa_header-obj_key,
          46 wa_header-xblnr,
          63 p_message,
         150 sy-vline.

endform.                    " f_imprime_erros
