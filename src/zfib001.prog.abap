************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 05.04.2007                                          *
* Tipo de prg ...: Carga de Dados c/ BAPI
* Objetivo    ...: Coletar dados de clientes de planilha excel e efe-  *
*                  tuar a carga destes no R/3                          *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 05.04.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
report zfib001 line-size 150
               line-count 62(03)
               message-id z01 no standard page heading.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
*
data     : vg_ncoln      like sy-index value 60.

constants: c_ncoln       like sy-index value 01,
           c_nline       like sy-index value 01,
           c_path        type c length 200 value '/tmp/'.

include <icon>.

data: vg_message(220)    type c,
      vg_mess_tab(256)   type c,
      vg_ctrlcol1        type alsmex_tabline-value,
      vg_icon            type c,
      vg_cami            type string,
      vg_duplic_flg      type c,
      vg_codepage        type abap_encod,
      vg_sapcode         type cpcodepage,
      vg_kunnret         like kna1-kunnr,
      vg_filename        like rlgrap-filename.

field-symbols <icone>    like icon_checked.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
data: t_planilha         like standard table of alsmex_tabline,
      t_cabplan          like standard table of alsmex_tabline,

      wa_planilha        like alsmex_tabline,
      wa_cabplan         like alsmex_tabline.

data: begin of wa_arqtxt,
         kunnr(10),
         ktokd(4),
*>       bukrs(4),  "Campo ajustado para ampliação de empresas no R/3
         bukrs(60),
         name1(40),
         sortl(20),
         stras(60),
         house(10),
         ort02(40),
         pstlz(10),
         ort01(40),
         land1(03),
         regio(03),
         spras(01),
         txjcd(15),
         vbund(06),
         brsch(04),
         stcd1(16),
         stkzn(01),
         stcd2(18),
         stcd3(18),
         stcd4(18),
         akont(10),
         zuawa(03),
         fdgrv(10),
         zterm(04),
         togru(04),
         xzver(01),
         zwels(10),
         zahls(01),
         hbkid(05),
         altkn(10),
         lifnr(10),
      end   of wa_arqtxt,

      t_arqtxt           like standard table of wa_arqtxt.

 data:  wa_regcity         like j_1btreg_city,
        it_regcity         like standard table of wa_regcity.


data: begin of wa_datxls,
         line            like sy-tabix,             "linha da planilha
         kunnr           like kna1-kunnr,           "Id cliente R/3
         ktokd           like rf02d-ktokd,          "Grupo de contas
*> No campo BUKRS poderá conter várias empresas separadas por '/'.
*> O cliente criado para a primeira empresa, será o mesmo código usado
*> para ampliação nas demais informadas.
         bukrs(60)       type c,                    "Empresa
         name1           like kna1-name1,           "Nome
         sortl           like kna1-sortl,           "Conceito de pesquis
         stras           like kna1-stras,           "Rua
         house           like addr1_data-house_num1, "Numero
         ort02           like kna1-ort02,           "Bairro
         pstlz           like kna1-pstlz,           "CEP
         ort01           like kna1-ort01,           "Cidade
         land1           like kna1-land1,           "País
         regio           like kna1-regio,           "Estado
         spras           like kna1-spras,           "Idioma
         txjcd           like kna1-txjcd,           "Domicilio Fiscal
         vbund           like kna1-vbund,           "Nro Sociedade parc.
         brsch           like kna1-brsch,           "Cd Setor Industrial
         stcd1           like kna1-stcd1,           "CNPJ
         stkzn           like kna1-stkzn,           "Flag pessoa física
         stcd2           like kna1-stcd2,           "CPF
         stcd3           like kna1-stcd3,           "Insc. Estadual
         stcd4           like kna1-stcd4,           "Insc. Municipal
         akont           like knb1-akont,           "Cta conciliação
         zuawa           like knb1-zuawa,           "Chv p/ordenação
         fdgrv           like knb1-fdgrv,           "Grp prev tesouraria
         zterm           like knb1-zterm,           "Chv cond pagamento
         togru           like knb1-togru,           "Grp de tolerancia
         xzver           like knb1-xzver,           "Reg hist pagamentos
         zwels           like knb1-zwels,           "Lst meios pagtos
         zahls           like knb1-zahls,           "Chv bloq pagamentos
         hbkid           like knb1-hbkid,           "Chv abrev banc emp
         altkn           like knb1-altkn,           "No. antigo cliente
         lifnr           like kna1-lifnr,           "Id como fornecedor
       end   of wa_datxls,

       t_datxls          like standard table of wa_datxls.

data:  begin of wa_empresa,
         bukrs           like knb1-bukrs,
       end   of wa_empresa,

       t_empresa         like standard table of wa_empresa.


*----------------------------------------------------------------------*
* Declaração para objetos OLE para gravar planilha EXCEL
*----------------------------------------------------------------------*

type-pools ole2 .

data: h_excel            type ole2_object,        " Excel object
      h_workbooks        type ole2_object,        " list of workbooks
      h_work             type ole2_object,        " workbook
      h_cell             type ole2_object.        " cell

define m_message.
  case sy-subrc.
    when 0.
    when 1.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    when others. raise upload_ole.
  endcase.
end-of-definition.

field-symbols: <fs_wa>    type any,
               <fs_comp>  type any.

*----------------------------------------------------------------------*
* Declaração para função SD_CUSTOMER_MAINTAIN_ALL
*----------------------------------------------------------------------*
data: vg_kna1             like kna1,
      vg_knb1             like knb1,
      vg_bapiaddr1        like bapiaddr1,
      vg_kunnr            like kna1-kunnr,
      vg_return           like kna1-kunnr,
      vg_tabix            like sy-tabix.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
*
selection-screen comment /1(83) text-cm1.
selection-screen comment /1(83) text-cm2.

selection-screen begin of tabbed block blessed for 9 lines.
selection-screen:  tab (20) tab01 user-command fore,
                   tab (30) tab02 user-command back.
selection-screen end   of block blessed.

selection-screen begin of block b2 with frame title text-s03.
parameters: p_nline  like sy-index        obligatory.  "Nro aprox Linhas
selection-screen end   of block b2.

selection-screen begin of screen 100 as subscreen.
selection-screen begin of block b1 with frame title text-s02.
parameters  p_cami   like rlgrap-filename.  "Arq Excel/TXT
selection-screen comment /1(77) text-cm3.
parameters  p_proc   like rlgrap-filename.  "Arquivo Excel
selection-screen end   of block b1.

selection-screen begin of block b3 with frame title text-s04.
parameters: p_optxt  radiobutton group arq default 'X',
            p_opxls  radiobutton group arq.
selection-screen end   of block b3.

selection-screen end   of screen 100.

selection-screen begin of screen 200 as subscreen.
selection-screen begin of block b0 with frame title text-s01.
parameters: p_path   like rlgrap-filename modif id pat,
            p_entr   like rlgrap-filename,  "Arq APLIC.SERVER
            p_said   like rlgrap-filename.  "Arq APLIC.SERVER
selection-screen end   of block b0.
selection-screen end   of screen 200.

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
* Event at selection-Screen
*----------------------------------------------------------------------*
*
at selection-screen on value-request for p_proc.
  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = ' '
      def_path         = p_proc
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    importing
      filename         = p_proc
    exceptions
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.


*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
at selection-screen on p_cami.

  check ( sy-batch ne 'X' ) and ( p_cami is initial ).
  message e000 with 'Path/Nome do arq. entrada é campo obrigatório!'.


*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
at selection-screen on p_proc.

  check ( sy-batch ne 'X' ) and ( p_proc is initial ).
  message e000 with 'Path/Nome do arq. saída é campo obrigatório!'.


*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
at selection-screen.

  check ( sy-dynnr eq 1000 ).

  case sy-ucomm.
    when 'FORE'. blessed-dynnr = 100.
    when 'BACK'. blessed-dynnr = 200.


  endcase.
*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
initialization.

  loop at screen.
    if ( screen-group1 eq 'PAT' ).
      screen-input = 0.
      modify screen.
    endif.
  endloop.

  tab01             = 'Processamento Aberto'.
  tab02             = 'Processamento em Background'.
  blessed-prog      =  sy-repid.
  blessed-dynnr     = 200.
  blessed-activetab = 'BACK'.

  p_path            = c_path.

  set titlebar 'TITLE01'.
  if p_nline is initial.
    p_nline = 9999.
  endif.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
start-of-selection.

  perform f_processa_planilha.
  sort t_datxls by line.

select * from j_1btreg_city
           into table it_regcity
            for all entries in t_datxls
          where ( country    eq t_datxls-land1      )
            and ( region     eq t_datxls-regio      )
            and ( pstcd_from le t_datxls-pstlz      )
            and ( pstcd_to   ge t_datxls-pstlz      ).


  sort it_regcity by country region pstcd_from pstcd_to.


  loop at t_datxls into wa_datxls.

    assign icon_incomplete to <icone>.
    clear: vg_kna1, vg_knb1, vg_bapiaddr1.

    perform f_move_date_to_structure.
    perform f_customer_create.
    modify t_datxls from wa_datxls transporting kunnr.

  endloop.

  check ( not t_datxls[] is initial ).
  perform f_grava_planilha.

*----------------------------------------------------------------------*
* Event end-of-selection
*----------------------------------------------------------------------*
*
end-of-selection.
*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
top-of-page.

  uline.
  write: /01 sy-vline,
          02 'Maggi - Projeto Crescer',
          35 'Log de erros de importação - Cargas de Clientes',
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
          07 'Linha',
          15 'Empresa',
          23 'Nome do cliente',
          63 'Mensagem do log',
         150 sy-vline.
  uline.

*----------------------------------------------------------------------*
* Event END_OF_PAGE.
*----------------------------------------------------------------------*
*
end-of-page.
  uline.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_processa_planilha.
  data: vl_line            like sy-tabix.

  assign icon_incomplete to <icone>.

  if ( p_optxt eq 'X' ).
*
* Carrega arquivo texto com dados de clientes
*
    clear   t_arqtxt.
    refresh t_arqtxt.
    vg_cami = p_cami.

    if ( sy-batch eq 'X' ).

      if ( p_entr is initial ).
        message e000 with
        'Nome do arquivo de entrada é campo obrigatório!'.
      elseif ( p_said is initial ).
        message e000 with
        'Nome do arquivo de saida é campo obrigatório!'.
      endif.

*> Transporta o conteúdo do PATH informado
      concatenate p_path p_entr into vg_filename.

      call function 'Z_BC_OPENDATASET_WITH_MODE'
        exporting
          filename    = vg_filename
        tables
          table       = t_arqtxt
        exceptions
          open_error  = 1
          read_error  = 2
          write_error = 3
          close_error = 4
          others      = 5.

      if ( sy-subrc ne 0 ).
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
            raw_message = vg_mess_tab.

        perform f_imprime_erros  using vg_mess_tab.
      endif.

    else.
      call function 'GUI_UPLOAD'
        exporting
          filename                = vg_cami
          codepage                = vg_codepage
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
    endif.

    refresh t_datxls.
    clear:  t_datxls, vl_line.

    loop at t_arqtxt into wa_arqtxt.
      vl_line = vl_line + 1.
      check ( vl_line le p_nline ).
      move-corresponding wa_arqtxt to wa_datxls.
      append wa_datxls to t_datxls.
    endloop.

  else.
*
* Carrega planilha com dados de clientes
*
    clear   t_planilha.
    refresh t_planilha.
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      exporting
        filename                = p_cami
        i_begin_col             = c_ncoln
        i_begin_row             = c_nline
        i_end_col               = vg_ncoln
        i_end_row               = p_nline
      tables
        intern                  = t_planilha
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        others                  = 3.
    if sy-subrc <> 0.
      message e004 with text-002.
    endif.

    sort t_planilha by row col.
    refresh t_datxls.
    clear   t_datxls.

*> Move valores das células ti com dados de criação de clientes
    loop at t_planilha into wa_planilha.

      on change of wa_planilha-row.
        vg_ctrlcol1 = wa_planilha-value.
      endon.
*> Elimina linhas de cabeçalho e texto da tabela interna.
      if vg_ctrlcol1 eq 'CAB'.
        clear wa_cabplan.
        wa_cabplan = wa_planilha.
        append wa_cabplan to t_cabplan.
        delete t_planilha.
        continue.
      endif.

      at new row.
        clear wa_datxls.
        wa_datxls-line = wa_planilha-row.
      endat.

      case wa_planilha-col.
        when  2. wa_datxls-kunnr = wa_planilha-value.
        when  3. wa_datxls-ktokd = wa_planilha-value.
        when  4. wa_datxls-bukrs = wa_planilha-value.
        when  5. wa_datxls-name1 = wa_planilha-value.
        when  6. wa_datxls-sortl = wa_planilha-value.
        when  7. wa_datxls-stras = wa_planilha-value.
        when  8. wa_datxls-house = wa_planilha-value.
        when  9. wa_datxls-ort02 = wa_planilha-value.
        when 10. wa_datxls-pstlz = wa_planilha-value.
        when 11. wa_datxls-ort01 = wa_planilha-value.
        when 12. wa_datxls-land1 = wa_planilha-value.
        when 13. wa_datxls-regio = wa_planilha-value.
        when 14. wa_datxls-spras = wa_planilha-value.
        when 15. wa_datxls-txjcd = wa_planilha-value.
        when 16. wa_datxls-vbund = wa_planilha-value.
        when 17. wa_datxls-brsch = wa_planilha-value.
        when 18. wa_datxls-stcd1 = wa_planilha-value.
        when 19. wa_datxls-stkzn = wa_planilha-value.
        when 20. wa_datxls-stcd2 = wa_planilha-value.
        when 21. wa_datxls-stcd3 = wa_planilha-value.
        when 22. wa_datxls-stcd4 = wa_planilha-value.
        when 23. wa_datxls-akont = wa_planilha-value.
        when 24. wa_datxls-zuawa = wa_planilha-value.
        when 25. wa_datxls-fdgrv = wa_planilha-value.
        when 26. wa_datxls-zterm = wa_planilha-value.
        when 27. wa_datxls-togru = wa_planilha-value.
        when 28. wa_datxls-xzver = wa_planilha-value.
        when 29. wa_datxls-zwels = wa_planilha-value.
        when 30. wa_datxls-zahls = wa_planilha-value.
        when 31. wa_datxls-hbkid = wa_planilha-value.
        when 32. wa_datxls-altkn = wa_planilha-value.
        when 33. wa_datxls-lifnr = wa_planilha-value.
      endcase.

      at end of row.
        append wa_datxls to t_datxls.
      endat.
    endloop.
  endif.
endform.                    " f_processa_planilha

*&---------------------------------------------------------------------*
*&      Form  f_move_date_to_structure
*&---------------------------------------------------------------------*
form f_move_date_to_structure.

  vg_kna1-kunnr = wa_datxls-kunnr.
  vg_kna1-ktokd = wa_datxls-ktokd.

*> A It controla abaixo controla o nro de vezes que o mesmo Id será
*> usado para ampliação de empresas.
  refresh t_empresa.
  split wa_datxls-bukrs at '/' into table t_empresa.

  vg_kna1-locco = wa_datxls-regio.
  vg_kna1-vbund = wa_datxls-vbund.
  vg_kna1-brsch = wa_datxls-brsch.
  vg_kna1-stcd1 = wa_datxls-stcd1.
  vg_kna1-stkzn = wa_datxls-stkzn.
  vg_kna1-stcd2 = wa_datxls-stcd2.
  vg_kna1-stcd3 = wa_datxls-stcd3.
  vg_kna1-stcd4 = wa_datxls-stcd4.
  vg_kna1-lifnr = wa_datxls-lifnr.

  vg_knb1-akont = wa_datxls-akont.
  vg_knb1-zuawa = wa_datxls-zuawa.
  vg_knb1-fdgrv = wa_datxls-fdgrv.
  vg_knb1-zterm = wa_datxls-zterm.
  vg_knb1-togru = wa_datxls-togru.
  vg_knb1-xzver = wa_datxls-xzver.
  vg_knb1-zwels = wa_datxls-zwels.
  vg_knb1-zahls = wa_datxls-zahls.
  vg_knb1-hbkid = wa_datxls-hbkid.
  vg_knb1-altkn = wa_datxls-altkn.

  vg_bapiaddr1-name       = wa_datxls-name1.
  vg_bapiaddr1-city       = wa_datxls-ort01.
  vg_bapiaddr1-district   = wa_datxls-ort02.
  vg_bapiaddr1-postl_cod1 = wa_datxls-pstlz.
  vg_bapiaddr1-street     = wa_datxls-stras.
  vg_bapiaddr1-house_no   = wa_datxls-house.
  vg_bapiaddr1-country    = wa_datxls-land1.
  vg_bapiaddr1-langu      = wa_datxls-spras.
  vg_bapiaddr1-region     = wa_datxls-regio.
  vg_bapiaddr1-sort1      = wa_datxls-sortl.

loop at it_regcity into wa_regcity
                     where ( country    eq wa_datxls-land1      )
                       and ( region     eq wa_datxls-regio      )
                       and ( pstcd_from le wa_datxls-pstlz      )
                       and ( pstcd_to   ge wa_datxls-pstlz      ).

  vg_bapiaddr1-taxjurcode = wa_regcity-taxjurcode.
  exit.
endloop.

endform.                    " f_move_date_to_structure

*&---------------------------------------------------------------------*
*&      Form  f_customer_create
*&---------------------------------------------------------------------*
form f_customer_create.
*> Move o valor do Id cliente R/3 independente de ser uma operação de
*> inclusão ou modificação.
  vg_kunnr = vg_kna1-kunnr.

  loop at t_empresa into wa_empresa.
*> Em primeiro lugar verificar a duplicidade para o cliente que se
*> deseja carregar, seguindo a mesma regra da verificação de duplicidade
*> Já desenhada no projeto de ampliação "ZFI001". Para consulta use a
*> transação cmod.

    at first.
      clear vg_duplic_flg.
*  Não deixar duplicar CPF
      if ( 'ZCNF ZCFU ZCFF' cs vg_kna1-ktokd ).
        perform f_consistir_cpf(saplxf04) using vg_kna1-kunnr
                                                vg_kna1-stcd2
                                       changing vg_duplic_flg
                                                vg_kunnret.
      elseif ( 'ZCNJ ZCPJ ZCIC ZCFJ' cs vg_kna1-ktokd ).
*  Não deixar duplicar CNPJ

        perform f_consistir_cnpj(saplxf04) using vg_kna1-kunnr
                                                 vg_kna1-stcd1
                                        changing vg_duplic_flg
                                                 vg_kunnret.
      elseif ( vg_kna1-ktokd eq 'ZCPF' ).
*  Não deixar duplicar CPF + Inscrição estadual juntos
        perform f_consistir_cpf_insc(saplxf04) using vg_kna1-kunnr
                                                     vg_kna1-stcd2
                                                     vg_kna1-stcd3
                                            changing vg_duplic_flg
                                                     vg_kunnret.
      endif.

    endat.

    if ( not vg_duplic_flg is initial ).
      concatenate 'Duplicidade de CNPJ ou CPF com o cliente' vg_kunnret
                          into vg_message  separated by space.
      perform f_imprime_erros  using vg_message.
      exit.
    endif.

    vg_tabix = sy-tabix.
*> Move o valor do Id cliente R/3. Se houver algum valor os dados serão
*> modificados, caso contrário, será incluido um cliente novo e ampliado
*> em tantas quanto forem as empresas informadas na planilha ou arq. TXT
    vg_kna1-kunnr = vg_kunnr.
    vg_knb1-kunnr = vg_kunnr.
    vg_knb1-bukrs = wa_empresa-bukrs.


    call function 'SD_CUSTOMER_MAINTAIN_ALL'
      exporting
        i_kna1                  = vg_kna1
        i_knb1                  = vg_knb1
        i_bapiaddr1             = vg_bapiaddr1
      importing
        e_kunnr                 = vg_return
      exceptions
        client_error            = 1
        kna1_incomplete         = 2
        knb1_incomplete         = 3
        knb5_incomplete         = 4
        knvv_incomplete         = 5
        kunnr_not_unique        = 6
        sales_area_not_unique   = 7
        sales_area_not_valid    = 8
        insert_update_conflict  = 9
        number_assignment_error = 10
        number_not_in_range     = 11
        number_range_not_extern = 12
        number_range_not_intern = 13
        account_group_not_valid = 14
        parnr_invalid           = 15
        bank_address_invalid    = 16
        tax_data_not_valid      = 17
        no_authority            = 18
        company_code_not_unique = 19
        dunning_data_not_valid  = 20
        knb1_reference_invalid  = 21
        cam_error               = 22
        others                  = 23.



    if sy-subrc <> 0.
      case sy-subrc.
        when  1. vg_message = 'Erro nos dados do cliente'.
        when  2. vg_message = 'Dados da estrutura kna1 incompletos!'.
        when  3. vg_message = 'Dados da estrutura knb1 incompletos!'.
        when  4. vg_message = 'Dados da estrutura knb5 incompletos!'.
        when  5. vg_message = 'Dados da estrutura knvv incompletos!'.
        when  6. vg_message = 'Cliente já cadastrado'.
        when  7. vg_message = 'Área de vendas já cadastrada'.
        when  8. vg_message = 'Dados da área de vendas inválidos!'.
        when  9.
          vg_message =
                    'Erro durante o insert ou update dos dados!'.
        when 10. vg_message = 'Erro na atribuição do ID cliente!'.
        when 11. vg_message = 'ID cliente fora do range estipulado!'.
        when 12. vg_message = 'Range para ID Cliente não é externo!'.
        when 13. vg_message = 'Range para ID Cliente não é interno!'.
        when 14. vg_message = 'Grupo de contas inválido!'.
        when 15. vg_message = 'Parceiro de negócio inválido!'.
        when 16. vg_message = 'Dados de banco inválido !'.
        when 17. vg_message = 'Dados de Classificação Fisc inválidos!'.
        when 18. vg_message = 'Sem autorização!'.
        when 19. vg_message = 'Empresa já atribuida!'.
        when 20. vg_message = 'Dados inválidos!'.
        when 21. vg_message = 'Dados de referência knb1 inválidos!'.
        when 22. vg_message = 'CAM Error!'.
        when 23. vg_message = 'Erro desconhecido!'.
      endcase.

      concatenate text-003 vg_message into vg_message
                                     separated by space.
      perform f_imprime_erros  using vg_message.

      if ( vg_tabix eq 1 ).
        exit.
      endif.

    else.
      if ( vg_tabix eq 1 ).
        wa_datxls-kunnr = vg_kunnr = vg_return.
      endif.

      assign icon_checked to <icone>.
      concatenate 'Cliente' vg_kunnr 'criado com sucesso!'
                            into vg_message separated by space.
      perform f_imprime_erros using vg_message.
      commit work and wait.
      wait up to 1 seconds.

    endif.
  endloop.

  uline.

endform.                    " f_determinacao_cfop

*&---------------------------------------------------------------------*
*&      Form  f_grava_planilha
*&---------------------------------------------------------------------*
form f_grava_planilha .

  data: vl_ind     type i,
        vl_l       type i,
        vl_c       type i.

  if ( p_optxt eq 'X' ).
*
* Carrega arquivo texto com dados de clientes
*
    refresh t_arqtxt.

    loop at t_datxls into wa_datxls.
      clear  wa_arqtxt.
      move-corresponding wa_datxls to wa_arqtxt.
      append wa_arqtxt to t_arqtxt.
    endloop.

    if ( sy-batch eq 'X' ).
*> Transporta o conteúdo do PATH informado
      concatenate p_path p_said into vg_filename.

      call function 'Z_BC_OPENDATASET_WITH_MODE'
        exporting
          filename    = vg_filename
          acess       = 'O'
        tables
          table       = t_arqtxt
        exceptions
          open_error  = 1
          read_error  = 2
          write_error = 3
          close_error = 4
          others      = 5.

      if ( sy-subrc ne 0 ).
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
            raw_message = vg_mess_tab.

        perform f_imprime_erros  using vg_mess_tab.
      endif.


    else.

      vg_cami = p_proc.

      call function 'GUI_DOWNLOAD'
        exporting
          filename                = vg_cami
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
    endif.
  else.
*
* Carrega planilha com dados de clientes
*

* start Excel
    if h_excel-header = space or h_excel-handle = -1.
      create object h_excel 'EXCEL.APPLICATION'.
      m_message.
    endif.

    break-point.

*    set property of h_excel  'Visible' = 1.
*    m_message.
*
* get list of workbooks, initially empty
    call method of h_excel 'Workbooks' = h_workbooks.
    m_message.

* add a new workbook
    call method of h_workbooks 'Add' = h_work.
    m_message.

*>  Grava os dados do cabeçalho da planilha Excel carregada
    loop at t_cabplan into wa_cabplan.
      vl_l = wa_cabplan-row.
      vl_c = wa_cabplan-col.
      call method of h_excel 'Cells' = h_cell
        exporting
          #1 = vl_l
          #2 = vl_c.
      m_message.
      set property of h_cell  'Value' = wa_cabplan-value.
      m_message.
    endloop.

    loop at t_datxls into wa_datxls.
      assign wa_datxls to <fs_wa>.

      clear vl_c.
      do 32 times.
        vl_c = vl_c + 1.
        check ( vl_c gt 1 ).
        assign component sy-index of structure <fs_wa> to <fs_comp>.

        vl_l = wa_datxls-line.
        call method of h_excel 'Cells' = h_cell
          exporting
            #1 = vl_l
            #2 = vl_c.
        m_message.
        set property of h_cell  'Value' = <fs_comp>.
        m_message.

      enddo.
    endloop.

    break-point.

    set property of h_excel 'DisplayAlerts' = 0.
    m_message.
    call method of h_work 'SaveAs'
      exporting
        #1 = p_proc.
    m_message.
    call method of h_excel 'QUIT'.
    free object h_excel.
    free object h_work.
    free object h_workbooks.
    free object h_cell.
    m_message.

  endif.
endform.                    " f_grava_planilha

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
form f_imprime_erros using    p_message.
  write: /01 sy-vline,
          03 <icone> as icon,
          08(05) wa_datxls-line,
          15 wa_empresa-bukrs,
          23 wa_datxls-name1,
          63 p_message,
         150 sy-vline.
endform.                    " f_imprime_erros
