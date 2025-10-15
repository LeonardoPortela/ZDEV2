*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/07/2010                                              &*
*& Descrição: Automatização Nota Fiscal Writer                        &*
*& Transação: ZNFW0001                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   20.07.2010                            &*
*&--------------------------------------------------------------------&*

report  zwrr0001.
tables: zfiwrt0001, zfiwrt0004, zfiwrt0003, zfiwrt0002, zfiwrt0005,
        j_1baj, zfiwrt0006,zfiwrt0032.
include <icon>.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
types: begin of ty_fields,
         campo(30) type c,
         value     type sy-tabix,
         invisible type sy-tabix,
       end   of ty_fields,

       begin of ty_editor,
         line(72),
       end   of ty_editor,

       begin of ty_msg_ret,
         msg(100),
       end of ty_msg_ret,

       begin of ty_direitos,
         cfop    type zfiwrt0006-cfop,
         taxlw1  type zfiwrt0006-taxlw1,
         taxlw2  type zfiwrt0006-taxlw2,
         taxlw4  type zfiwrt0006-taxlw4,
         taxlw5  type zfiwrt0006-taxlw5,
         opertyp type zfiwrt0006-opertyp,
         taxcode type zfiwrt0006-taxcode,
         taxlw3  type zfiwrt0006-taxlw3,
       end of ty_direitos.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*-CS2023000043-09.02.2023-#102019-JT-inicio
types: begin of ty_dent_old,
         oprd_trib           type char1,
         oprd_isent          type char1,
         oprd_ntrib          type char1,
         cfop                type zfiwrt0006-cfop,
         taxlw1              type zfiwrt0006-taxlw1,
         taxlw2              type zfiwrt0006-taxlw2,
         taxlw4              type zfiwrt0006-taxlw4,
         taxlw5              type zfiwrt0006-taxlw5,
         taxcode             type zfiwrt0006-taxcode,
         taxlw3              type zfiwrt0006-taxlw3,
         wl_desc_den_cfop    type j_1bagnt-cfotxt,
         wl_desc_den_taxlw1  type j_1batl1t-descrip,
         wl_desc_den_taxlw2  type j_1batl2t-descrip,
         wl_desc_den_taxlw4  type j_1batl4t-descrip,
         wl_desc_den_taxlw5  type j_1batl5t-descrip,
         wl_desc_den_taxlw3  type j_1batl3t-descrip,
         wl_desc_den_taxcode type j_1btxsdct-txt.
types: end   of ty_dent_old.
*
types: begin of ty_fora_old,
         oprf_trib           type char1,
         oprf_isent          type char1,
         oprf_ntrib          type char1,
         cfop                type zfiwrt0006-cfop,
         taxlw1              type zfiwrt0006-taxlw1,
         taxlw2              type zfiwrt0006-taxlw2,
         taxlw4              type zfiwrt0006-taxlw4,
         taxlw5              type zfiwrt0006-taxlw5,
         taxcode             type zfiwrt0006-taxcode,
         taxlw3              type zfiwrt0006-taxlw3,
         wl_desc_for_cfop    type j_1bagnt-cfotxt,
         wl_desc_for_taxlw1  type j_1batl1t-descrip,
         wl_desc_for_taxlw2  type j_1batl2t-descrip,
         wl_desc_for_taxlw4  type j_1batl4t-descrip,
         wl_desc_for_taxlw5  type j_1batl5t-descrip,
         wl_desc_for_taxlw3  type j_1batl3t-descrip,
         wl_desc_for_taxcode type j_1btxsdct-txt.
types: end   of ty_fora_old.
*-CS2023000043-09.02.2023-#102019-JT-fim

""ALRS TESTE
******************************************************************************
data: begin of it_msg occurs 0.
        include structure bdcmsgcoll.
data: end of it_msg.
data: wl_mode(1).

data: ti_bdcdata       type standard table of bdcdata,   "Guarda o mapeamento
      wa_bdcdata       like line of ti_bdcdata,
      wl_erro(1),
      wg_documento(10).

data: gv_text_sd type char1.
data: flag_tab8 type char1.

types: begin of ty_disable,
         aba          type zfiwrt0016-aba,
         tela(4)      type c,
         desc         type string,
         permissao(1) type c,
       end of ty_disable.

data: it_list_fields type standard table of ty_disable with header line.
data: it_aba_lista type standard table of ty_disable initial size 0.
data: wa_tela type ty_disable.

******************************************************************************
*Class definition for ALV toolbar
*CLASS:      LCL_ALV_TOOLBAR   DEFINITION DEFERRED.
**---------------------------------------------------------------------*
**       CLASS lcl_alv_toolbar DEFINITION
**---------------------------------------------------------------------*
**       ALV event handler
**---------------------------------------------------------------------*
*CLASS LCL_ALV_TOOLBAR DEFINITION.
*  PUBLIC SECTION.
**Constructor
*    METHODS: CONSTRUCTOR
*                IMPORTING
*                 IO_ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
**Event for toolbar
*    ON_TOOLBAR
*       FOR EVENT TOOLBAR
*       OF  CL_GUI_ALV_GRID
*       IMPORTING
*         E_OBJECT,
*
*      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
*               IMPORTING
*                         E_UCOMM.
*ENDCLASS.                    "lcl_alv_toolbar DEFINITION
*&--------------------------------------------------------------------&*
*& Declarações de objetos/Classes                                    &*
*&--------------------------------------------------------------------&*
data: g_textbox          type scrfname value 'CC_TEXTBOX',
      g_custom_container type ref to cl_gui_custom_container,
      obg_textbox        type ref to cl_gui_textedit,
      g_descbox          type scrfname value 'CC_DESC',
      g_custom_cont_desc type ref to cl_gui_custom_container,
      obg_descbox        type ref to cl_gui_textedit.
** Docking Container
*      OBG_DOCKING         TYPE REF TO CL_GUI_DOCKING_CONTAINER,
*      OBG_GRID1           TYPE REF TO CL_GUI_ALV_GRID,
*      OBG_TOOLBAR         TYPE REF TO LCL_ALV_TOOLBAR,
*      C_ALV_TOOLBARMANAGER TYPE REF TO CL_ALV_GRID_TOOLBAR_MANAGER.
**Declaration for toolbar buttons
*DATA : TY_TOOLBAR TYPE STB_BUTTON..
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants: c_0               type c value '0',
           c_1               type c value '1',
           c_b               type c value 'B',
           c_l               type c value 'L',
           c_x               type c value 'X',
           c_d               type c value 'D',
           c_f               type c value 'F',
           c_t               type c value 'T',
           c_i               type c value 'I',
           c_n               type c value 'N',
           c_add(3)          type c value 'ADD',
           c_exit(4)         type c value 'EXIT',
           c_back(4)         type c value 'BACK',
           c_save(4)         type c value 'SAVE',
           c_desat(5)        type c value 'DESAT',
           c_modif(5)        type c value 'MODIF',
           c_cancel(6)       type c value 'CANCEL',
           c_dclick(6)       type c value 'DCLICK',
           c_search(6)       type c value 'SEARCH',
           c_atuali(6)       type c value 'ATUALI',
           c_add_msg(7)      type c value 'ADD_MSG',
           c_clos_msg(8)     type c value 'CLOS_MSG',
           c_show_msgret(11) type c value 'SHOW_MSGRET',
*&------Inicio CS2024000487 Histórico ZNFW0001 / AOENNING--&
           c_sel_tp_a(10)    type c value 'SEL_TP_A',
           c_sel_tp_b(10)    type c value 'SEL_TP_B',
           c_sel_tp_c(10)    type c value 'SEL_TP_C',
           c_sel_tp_d(10)    type c value 'SEL_TP_D',
           c_sel_tp_e(10)    type c value 'SEL_TP_E',
           c_sel_tp_f(10)    type c value 'SEL_TP_F'.
*&------Fim CS2024000487 Histórico ZNFW0001 / AOENNING--&


data: c_exib_log  type char01,
      c_sel_table type objs-objectname.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
constants: begin of c_tab_strip_nf,
             tab1 like sy-ucomm value 'TAB_STRIP_NF_FC1',
             tab2 like sy-ucomm value 'TAB_STRIP_NF_FC2',
             tab3 like sy-ucomm value 'TAB_STRIP_NF_FC3',
             tab4 like sy-ucomm value 'TAB_STRIP_NF_FC4',
             tab5 like sy-ucomm value 'TAB_STRIP_NF_FC5',
             tab6 like sy-ucomm value 'TAB_STRIP_NF_FC6',
             tab7 like sy-ucomm value 'TAB_STRIP_NF_FC7',
             tab8 like sy-ucomm value 'TAB_STRIP_NF_FC8',
           end of c_tab_strip_nf.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
controls:  tab_strip_nf type tabstrip.
data: begin of g_tab_strip_nf,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'ZWRR0001',
        pressed_tab like sy-ucomm value c_tab_strip_nf-tab1,
      end of g_tab_strip_nf.
data:      ok-code like sy-ucomm.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: tg_0001             type table of zfiwrt0001,
      wg_0001             type zfiwrt0001,
      tg_0002             type table of zfiwrt0002,
      wg_0002             type zfiwrt0002,
      tg_0003             type table of zfiwrt0003,
      wg_0003             type zfiwrt0003,
      tg_0004             type table of zfiwrt0004,
      wg_0004             type zfiwrt0004,
      tg_0005             type table of zfiwrt0005,
      wg_0005             type zfiwrt0005,
      tg_fields           type table of ty_fields   with header line,
      tg_editor           type table of ty_editor,
      wg_editor           type ty_editor,
      tg_msg_ret          type table of zfiwrs0002, "TY_MSG_RET,
      wg_msg_ret          type zfiwrs0002, "TY_MSG_RET,
      wa_estrutura        type ty_estrutura,
      it_estrutura        type table of ty_estrutura,
      wl_desc_nftype      type j_1baat-nfttxt,
      wl_desc_itmtyp      type j_1bitemtypest-text,
      wl_desc_parvw       type j_1badt-partxt,
      wl_desc_den_cfop    type j_1bagnt-cfotxt,
      wl_desc_den_taxlw1  type j_1batl1t-descrip,
      wl_desc_den_taxlw2  type j_1batl2t-descrip,
      wl_desc_den_taxlw4  type j_1batl4t-descrip,
      wl_desc_den_taxlw5  type j_1batl5t-descrip,
      wl_desc_den_taxlw3  type j_1batl3t-descrip,
      wl_desc_den_taxcode type j_1btxsdct-txt,
      wl_desc_for_cfop    type j_1bagnt-cfotxt,
      wl_desc_for_taxlw1  type j_1batl1t-descrip,
      wl_desc_for_taxlw2  type j_1batl2t-descrip,
      wl_desc_for_taxlw4  type j_1batl4t-descrip,
      wl_desc_for_taxlw5  type j_1batl5t-descrip,
      wl_desc_for_taxlw3  type j_1batl3t-descrip,
      wl_desc_for_taxcode type j_1btxsdct-txt,
      wg_mensagens(30)    value '@5C@ Messagens',
      tg_fieldcatalog     type lvc_t_fcat,
      wg_fieldcatalog     type lvc_s_fcat,
      wg_layout           type lvc_s_layo,
      wg_stable           type lvc_s_stbl,
      wg_dentro           type ty_direitos,
      wg_fora             type ty_direitos,
      oprd_trib           value space,
      oprf_trib,
      oprd_isent,
      oprf_isent,
      oprd_ntrib,
      oprf_ntrib,
      wg_acao(30),
      wg_tmv(1),
      wg_flag,
      wg_blq(4), "VALUE c_search.
      x_field(30),
      wl_dent_old         type ty_dent_old,  "*-CS2023000043-09.02.2023-#102019-JT
      wl_fora_old         type ty_fora_old.  "*-CS2023000043-09.02.2023-#102019-JT


data: tab_1(30) value 'Dados Gerais',
      tab_2(30) value 'Texto Compl. Operação',
      tab_3(30) value 'Impostos',
      tab_4(30) value 'Mensagem Nota',
      tab_5(30) value 'Contabilização',
      tab_6(30) value 'Movimento Estoque',
      tab_7(30) value 'CFOP',
      tab_8(30) value 'NCM - ZNFW0009'.


*      tg_impo type TABLE OF TY_IMPO,"ZFIWRS0001,
*      wg_impo type TY_IMPO. "ZFIWRS0001.
data: begin of tg_impo occurs 0,
*        INCLUDE STRUCTURE ZFIWRS0001.
        mark(1),
        taxtyp   type zfiwrs0001-taxtyp,
        ttypetxt type j_1bttytxt,
        taxgrp   type j_1btaxgrp,
      end of tg_impo.

data: begin of tg_mensagems occurs 0,
        seqnum  type zfiwrt0005-seqnum,
        message type zfiwrt0005-message,
      end of tg_mensagems,

      begin of tg_mensagems_aux occurs 0,
        seqnum  type zfiwrt0005-seqnum,
        linnum  type zfiwrt0005-linnum,
        message type zfiwrt0005-message,
      end of tg_mensagems_aux,

      begin of tg_contab occurs 0,
        mark(1),
        operacao   type zfiwrt0003-operacao,
        buzei      type zfiwrt0003-buzei,
        bschl      type zfiwrt0003-bschl,
        hkont      type zfiwrt0003-hkont,
        umskz      type zfiwrt0003-umskz,
        taxtyp     type zfiwrt0003-taxtyp,
        newbw      type zfiwrt0003-newbw,
        estorno    type zfiwrt0003-estorno,
        data_reg   type zfiwrt0003-data_reg,
        hora_reg   type zfiwrt0003-hora_reg,
        usname_reg type zfiwrt0003-usname_reg,
      end of tg_contab,

      begin of tg_zfiwrt0032 occurs 0,
        mark(1),
        ncm      type zfiwrt0032-ncm,
        material type zfiwrt0032-material,
        deposito type zfiwrt0032-deposito,
        lote     type zfiwrt0032-lote,
        lote_aut type zfiwrt0032-lote_aut,
*        data     TYPE zfiwrt0032-data,
*        hora     TYPE zfiwrt0032-hora,
*        usuario  TYPE zfiwrt0032-usuario,
      end of tg_zfiwrt0032,

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
      begin of tg_cfop occurs 0,
        mark(1),
        operacao type zfiwrt0028-operacao,
        uf       type zfiwrt0028-uf,
        cfop     type zfiwrt0028-cfop,
      end of tg_cfop,
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893


      begin of tg_movest occurs 0,
        mark(1),
        bwart   type zfiwrt0004-bwart,
        tcode   type zfiwrt0004-tcode,
        mwskz1  type zfiwrt0004-mwskz1,
        estorno type zfiwrt0004-estorno,
      end of tg_movest.

data: tg_contab_log like tg_contab occurs 0 with header line.
data: filter_ncm type zfiwrt0032-ncm.

*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
call screen 100.

**---------------------------------------------------------------------*
**       CLASS lcl_alv_toolbar IMPLEMENTATION
**---------------------------------------------------------------------*
**       ALV event handler
**---------------------------------------------------------------------*
*CLASS LCL_ALV_TOOLBAR IMPLEMENTATION.
*  METHOD CONSTRUCTOR.
**   Create ALV toolbar manager instance
*    CREATE OBJECT C_ALV_TOOLBARMANAGER
*      EXPORTING
*        IO_ALV_GRID = IO_ALV_GRID.
*  ENDMETHOD.                    "constructor
*
*  METHOD ON_TOOLBAR.
**   Add customized toolbar buttons.
**   variable for Toolbar Button
*    TY_TOOLBAR-ICON      =  ICON_VIEW_CLOSE.
*    TY_TOOLBAR-FUNCTION  =  C_CLOS_MSG.
*    TY_TOOLBAR-BUTN_TYPE = 0.
**    ty_toolbar-text = 'Button1'.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*
**    ty_toolbar-icon      =  icon_voice_output.
**    ty_toolbar-butn_type = 0.
**    ty_toolbar-text = 'Button2'.
**
**    APPEND ty_toolbar TO e_object->mt_toolbar.
**
**    ty_toolbar-icon      =  icon_phone.
**    ty_toolbar-butn_type = 0.
**    ty_toolbar-text = 'Button3'.
**    APPEND ty_toolbar TO e_object->mt_toolbar.
**
**    ty_toolbar-icon      =  icon_mail.
**    ty_toolbar-butn_type = 0.
**    ty_toolbar-text = 'Button4'.
**    APPEND ty_toolbar TO e_object->mt_toolbar.
**
**    ty_toolbar-icon      =  icon_voice_input.
**    ty_toolbar-butn_type = 0.
**    ty_toolbar-text = 'Button5'.
**    APPEND ty_toolbar TO e_object->mt_toolbar.
***   Call reorganize method of toolbar manager to
***   display the toolbar
*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.
*  ENDMETHOD.                    "on_toolbar
*  METHOD HANDLE_USER_COMMAND.
**   User Command Botões Incluidos
**    break abap.
*    CASE E_UCOMM.
*      WHEN C_CLOS_MSG.
*        CALL METHOD OBG_DOCKING->SET_VISIBLE
*          EXPORTING
*            VISIBLE = SPACE.
*
*        LEAVE TO SCREEN 100.
*    ENDCASE.
*  ENDMETHOD.                    "zm_handle_user_command
*
*ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*"lcl_alv_toolbar IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
form get_next_number  using    p_object   "TYPE nrobj
      p_nr_range "TYPE nrnr
changing p_number.

  clear p_number.

  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = p_nr_range
      object                  = p_object
    importing
      number                  = p_number
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc ne 0.
    clear: p_number.
    message e836(sd) with 'O intervalo de numeração,'
    'não foi encontrado!'.
  else.
    wg_flag = c_x.
  endif.

endform.                    " get_next_number
*&--------------------------------------------------------------------&*
*& MODULE TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT                          &*
*&--------------------------------------------------------------------&*
*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
module tab_strip_nf_active_tab_set output.
  perform verifica_erros.
  tab_strip_nf-activetab = g_tab_strip_nf-pressed_tab.
  case g_tab_strip_nf-pressed_tab.
    when c_tab_strip_nf-tab1.
      g_tab_strip_nf-subscreen = '0201'.
    when c_tab_strip_nf-tab2.
      g_tab_strip_nf-subscreen = '0202'.
    when c_tab_strip_nf-tab3.
      g_tab_strip_nf-subscreen = '0203'.
    when c_tab_strip_nf-tab4.
      g_tab_strip_nf-subscreen = '0204'.
    when c_tab_strip_nf-tab5.
      g_tab_strip_nf-subscreen = '0205'.
    when c_tab_strip_nf-tab6.
      g_tab_strip_nf-subscreen = '0208'.
*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
    when c_tab_strip_nf-tab7.
      g_tab_strip_nf-subscreen = '0209'.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
    when c_tab_strip_nf-tab8.
      g_tab_strip_nf-subscreen = '0210'.
    when others.
*&SPWIZARD:      DO NOTHING
  endcase.
endmodule.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT
*&--------------------------------------------------------------------&*
*& MODULE TAB_STRIP_NF_ACTIVE_TAB_GET INPUT                           &*
*&--------------------------------------------------------------------&*
*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
module tab_strip_nf_active_tab_get input.
  ok-code = sy-ucomm.
  case ok-code.
    when c_tab_strip_nf-tab1.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab1.
    when c_tab_strip_nf-tab2.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab2.
    when c_tab_strip_nf-tab3.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab3.
    when c_tab_strip_nf-tab4.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab4.
    when c_tab_strip_nf-tab5.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab5.
    when c_tab_strip_nf-tab6.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab6.
*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
    when c_tab_strip_nf-tab7.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab7.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
    when c_tab_strip_nf-tab8.
      g_tab_strip_nf-pressed_tab = c_tab_strip_nf-tab8.
    when others.
*&SPWIZARD:      DO NOTHING
  endcase.
endmodule.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: fcode type table of sy-ucomm.

  refresh: fcode.
*  APPEND C_SAVE TO FCODE.

  if wg_acao eq c_modif.
    append c_add to fcode.
    append c_atuali to fcode.
    set pf-status '0001' excluding fcode.

  elseif wg_acao eq c_add.
    append c_modif to fcode.
    append c_atuali to fcode.
    set pf-status '0001' excluding fcode.

  else.
    append c_save to fcode.
    set pf-status '002' excluding fcode.
    set pf-status '0001' excluding fcode.
  endif.
  set titlebar '001'.

*  IF zfiwrt0001-operacao IS INITIAL.
*    PERFORM get_next_number USING  'ZOPERACAO'
*                                     '1'
*                            CHANGING zfiwrt0001-operacao.
*  ENDIF.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  clear: c_sel_table, c_exib_log.


  case ok-code.
    when c_save.
      perform verifica_erros.
      if tg_msg_ret[] is initial.
        perform cria_documento.
      else.
        message s000(zwrm001) display like 'E' with 'Há erro no documento.'.
*                                                     ' "Mensagens" para visualizar.'.
*        CALL METHOD obg_docking->set_visible
*          EXPORTING
*            visible = c_x.

        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          tables
            it_msgs       = tg_msg_ret.
      endif.
    when c_add.
      wg_acao = c_add.
      if wg_flag is initial.
        perform get_next_number using  'ZOPERACAO'
              '1'
        changing zfiwrt0001-operacao.
        perform limpa_campos.
      endif.
    when c_modif.
      call function 'ENQUEUE_EZFIWRT0001'
        exporting
          operacao       = zfiwrt0001-operacao
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      if zfiwrt0001-descricao is not initial.
        wg_acao = c_modif.

      else.
        wg_acao = c_atuali.
        perform limpa_campos.
        perform busca_operacao.
        perform busca_descricoes.
        wg_acao = c_modif.
      endif.
    when c_atuali.
      wg_acao = c_atuali.
      perform limpa_campos.
      perform busca_operacao.
      perform busca_descricoes.
    when c_desat.
      if zfiwrt0001-descricao is initial.
        perform limpa_campos.
        perform busca_operacao.
        perform busca_descricoes.
      endif.
      perform verifica_erros.
      if tg_msg_ret[] is initial.
        perform modifica_status.
      else.
        message s000(zwrm001) display like 'E' with 'Há erro no documento.'.
*                                                     ' "Mensagens" para visualizar.'.
*******        CALL METHOD OBG_DOCKING->SET_VISIBLE
*******          EXPORTING
*******            VISIBLE = C_X.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          tables
            it_msgs       = tg_msg_ret.
      endif.
    when c_back.
      call function 'DEQUEUE_EZFIWRT0001'
        exporting
          operacao = zfiwrt0001-operacao.
      set screen 0.

    when c_show_msgret.
      perform verifica_erros.
      if tg_msg_ret[] is not initial.
        call function 'Z_DOC_CHECK_NEW'
          exporting
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          importing
            e_messagem    = wg_mensagens
          tables
            it_msgs       = tg_msg_ret.
      endif.
    when c_exit.
      call function 'DEQUEUE_EZFIWRT0001'
        exporting
          operacao = zfiwrt0001-operacao.
      leave program.
    when c_cancel.
      call function 'DEQUEUE_EZFIWRT0001'
        exporting
          operacao = zfiwrt0001-operacao.
      clear: wg_acao, zfiwrt0001-operacao, wg_flag.
      perform limpa_campos.

*&------Inicio CS2024000487 Histórico ZNFW0001 / AOENNING--&
    when c_sel_tp_a.
      c_sel_table = 'ZFIWRT0001'. "ABA Dados Gerais
      c_exib_log = abap_true.
    when c_sel_tp_b.
      c_sel_table = 'ZFIWRT0002'. "ABA Impostos
      c_exib_log = abap_true.
    when c_sel_tp_c.
      c_sel_table = 'ZFIWRT0003'. "ABA Contabilização
      c_exib_log = abap_true.
    when c_sel_tp_d.
      c_sel_table = 'ZFIWRT0004'. "ABA Movimento Estoque
      c_exib_log = abap_true.
    when c_sel_tp_e.
      c_sel_table = 'ZFIWRT0028'. "ABA CFOP
      c_exib_log = abap_true.
    when c_sel_tp_f.
      c_sel_table = 'ZFIWRT0006'. "ABA Dados Gerais / Dentro/Fora estado
      c_exib_log = abap_true.

*&------Fim CS2024000487 Histórico ZNFW0001 / AOENNING--&
  endcase.

*&------Inicio CS2024000487 Histórico ZNFW0001 / AOENNING&
  if c_exib_log eq  abap_true.
    call function 'Z_ANALISE_LOGS_TABLE'
      exporting
        cusobj   = c_sel_table
        tabfirst = 'X'.
  endif.
*&------Fim CS2024000487 Histórico ZNFW0001 / AOENNING--&


endmodule.                 " USER_COMMAND_0100  INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_MSG_NF' ITSELF
controls: tc_msg_nf type tableview using screen 0203.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MSG_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module tc_msg_nf_change_tc_attr output.
  describe table tg_mensagems lines tc_msg_nf-lines.
  add 5 to tc_msg_nf-lines.
endmodule.                    "TC_MSG_NF_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_MSG_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module tc_msg_nf_modify input.
  modify tg_mensagems
  index tc_msg_nf-current_line.
endmodule.                    "TC_MSG_NF_MODIFY INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.

  if sy-dynnr eq 203.
    if g_custom_container is initial.
      create object g_custom_container
        exporting
          container_name = g_textbox.

      if g_custom_container is not initial.
        create object obg_textbox
          exporting
            parent            = g_custom_container
            wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
            wordwrap_position = 72
            max_number_chars  = 255.

        call method obg_textbox->set_visible
          exporting
            visible = '0'.

      endif.
    endif.
    if obg_textbox is not initial.
      if wg_acao eq c_add
      or wg_acao eq c_modif.
        call method obg_textbox->set_readonly_mode
          exporting
            readonly_mode = 0.
      else.
        call method obg_textbox->set_readonly_mode
          exporting
            readonly_mode = 1.
      endif.
    endif.

  elseif sy-dynnr eq 208 or   sy-dynnr eq 201.

    if g_custom_cont_desc is initial.
      create object g_custom_cont_desc
        exporting
          container_name = g_descbox.

      if g_custom_cont_desc is not initial.
        create object obg_descbox
          exporting
            parent            = g_custom_cont_desc
            wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
            wordwrap_position = 72
            max_number_chars  = 255.

        call method obg_descbox->set_toolbar_mode
          exporting
            toolbar_mode = '0'.
      endif.

    endif.

    if obg_descbox is not initial.
      if wg_acao eq c_add.
        call method obg_descbox->set_readonly_mode( readonly_mode = obg_descbox->false ).
      endif.
      if wg_acao eq c_atuali.
        call method obg_descbox->set_readonly_mode( readonly_mode = obg_descbox->true ).
      endif.
    endif.
  endif.
endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0203  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0203 input.
  data: wl_line       type sy-tabix,
        wl_texto(255),
        wl_linetb     type sy-tabix.

  case ok-code.

      case gv_text_sd.

        when abap_true.

*          CALL FUNCTION 'READ_TEXT'
*            EXPORTING
*              id                            =
*              language                      =
*              NAME                          =
*              OBJECT                        =
*
*            TABLES
*              lines                         =
*           EXCEPTIONS
*             ID                            = 1
*             LANGUAGE                      = 2
*             NAME                          = 3
*             NOT_FOUND                     = 4
*             OBJECT                        = 5
*             REFERENCE_CHECK               = 6
*             WRONG_ACCESS_TO_ARCHIVE       = 7
*             OTHERS                        = 8
*                    .
*          IF sy-subrc <> 0.
** Implement suitable error handling here
*          ENDIF.
*
        when abap_false.

      endcase.

    when c_dclick.
      perform get_current_lin using 'TC_MSG_NF'
      changing wl_line.
      if wl_line gt 0.
        call method obg_textbox->set_visible
          exporting
            visible = '1'.

        if wg_acao eq c_add
        or wg_acao eq c_modif.
          perform trata_campos using 'ADDMSG'
                c_1       "INPUT 1     NO INPUT 0
                c_0.      "INVISIBLE 1 VISIBLE 0
        endif.

        refresh: tg_editor.
        clear: wg_editor.
        read table tg_mensagems index wl_line.
        if sy-subrc is initial.
          loop at tg_mensagems_aux
          where seqnum eq wl_line.

            move tg_mensagems_aux-message to wg_editor-line.
            append wg_editor to tg_editor.
            clear: wg_editor, tg_mensagems_aux.

          endloop.
        endif.

        call method obg_textbox->set_text_as_r3table
          exporting
            table = tg_editor.
      endif.
    when c_add_msg.
      call method obg_textbox->get_text_as_r3table
        importing
          table = tg_editor.

      clear: tg_mensagems_aux, tg_mensagems.
      loop at tg_editor into wg_editor.
        if sy-tabix eq 1.
          tg_mensagems-seqnum     = wl_line.
          tg_mensagems-message    = wg_editor-line.
          modify tg_mensagems index wl_line.

          if sy-subrc is not initial.
            append tg_mensagems.

          endif.
          delete tg_mensagems_aux where seqnum eq wl_line.
        endif.


        tg_mensagems_aux-seqnum = wl_line.
        add 1 to tg_mensagems_aux-linnum.
        tg_mensagems_aux-message = wg_editor-line.

*        READ TABLE tg_mensagems_aux TRANSPORTING NO FIELDS
*          WITH KEY seqnum = wl_line
*                   linnum = tg_mensagems_aux-linnum.
*        IF sy-subrc IS INITIAL.
*          MODIFY tg_mensagems_aux INDEX sy-tabix.
*
*        ELSE.
        append tg_mensagems_aux.

*        ENDIF.
      endloop.
*      LOOP AT tg_editor INTO wg_editor.
*        IF sy-tabix EQ 1.
*          wl_texto = wg_editor-line.
*
*        ELSEIF sy-tabix GE 2.
*          CONCATENATE wl_texto  wg_editor-line INTO wl_texto SEPARATED BY space.
*
*        ENDIF.
*        AT LAST.
*          MOVE wl_texto TO tg_mensagems-message.
*          MODIFY tg_mensagems INDEX wl_line.
*          IF sy-subrc IS NOT INITIAL.
*            APPEND tg_mensagems.
*
*          ENDIF.
*          CLEAR: wl_texto, tg_mensagems, wg_editor.
*          REFRESH: tg_editor.
*        ENDAT.
*      ENDLOOP.

      call method obg_textbox->set_visible
        exporting
          visible = '0'.

      perform trata_campos using 'ADDMSG'
            c_0       "INPUT 1     NO INPUT 0
            c_1.      "INVISIBLE 1 VISIBLE 0
    when others.
      call method obg_textbox->set_visible
        exporting
          visible = '0'.

      perform trata_campos using 'ADDMSG'
            c_0       "INPUT 1     NO INPUT 0
            c_1.      "INVISIBLE 1 VISIBLE 0
  endcase.
endmodule.                 " USER_COMMAND_0203  INPUT
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields output.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  if wg_acao eq c_add.
    perform f_trata_complemento_icms.
  endif.

  perform trata_acao_editar. "PSA

*-CS2023000043-09.02.2023-#102019-JT-fim
*    LOOP AT SCREEN.
*      IF screen-group1 EQ 'A1'
*      OR screen-group1 EQ 'MOD'
*      OR screen-name EQ 'ZFIWRT0001-DESCRICAO'.
*        screen-input     = 1. "tg_fields-value.
*        MODIFY SCREEN.
**        EXIT.
*      ENDIF.
*      IF screen-name EQ 'ZFIWRT0001-OPERACAO'.
*        screen-input     = 0.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF screen-group1 EQ 'I1'.
*        IF zfiwrt0001-imobilizado = 'S'.
*          screen-input     = 1.
*          screen-invisible = 0.
*          MODIFY SCREEN.
*        ELSE.
*          screen-input     = 0.
*          screen-invisible = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      ENDIF.

  if wg_acao eq c_add.
    loop at screen.
      if screen-group1 eq 'A1'
      or screen-group1 eq 'MOD'.
        screen-input     = 1. "tg_fields-value.
*        screen-invisible = tg_fields-invisible.
        modify screen.
*        EXIT.
      endif.
      if screen-name eq 'ZFIWRT0001-OPERACAO'.
        screen-input     = 0.
        modify screen.
      endif.
      if screen-name eq 'ZFIWRT0001-DESCRICAO'.
        screen-input     = 1.
*        screen-request   = 1.
        modify screen.
      endif.
    endloop.
  elseif wg_acao eq c_search
    or   wg_acao eq c_atuali
    or   wg_acao is initial.
    perform desabilitar_fields.
*    LOOP AT SCREEN.
*      IF screen-group1 EQ 'A1'
*      OR screen-group1 EQ 'MOD'.
*        screen-input     = 0. "tg_fields-value.
**        screen-invisible = tg_fields-invisible.
*        MODIFY SCREEN.
**        EXIT.
*      ENDIF.
*    ENDLOOP.
  endif.

  loop at tg_fields.
    loop at screen.
      if screen-name eq tg_fields-campo.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        modify screen.
        exit.
      endif.

    endloop.
  endloop.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  if ( wg_acao = c_modif or wg_acao = c_add ) and zfiwrt0001-complement_icms = 'S'.
    loop at screen.
      if screen-group2 = 'G2'.
        screen-input = 0.
        modify screen.
      endif.
    endloop.
  endif.
*-CS2023000043-09.02.2023-#102019-JT-fim

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  if zfiwrt0001-valida_cfop ne 'S'.
    clear:tg_cfop[], tg_cfop.
  endif.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

  if x_field is not initial.
    set cursor field x_field.
  endif.
** Depois de tratados os valores da tabela tem que ser eliminados para um proximo tratamento.
  refresh tg_fields.
  clear: tg_fields.
endmodule.                 " TRATA_FIELDS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  TRATA_COMPLEMENTO_ICMSCAMPOS
*&---------------------------------------------------------------------*
form f_trata_complemento_icms.

  if zfiwrt0001-complement_icms = 'S'.
*   MOVE oprd_trib                TO wl_dent_old-oprd_trib.
*   MOVE oprd_isent               TO wl_dent_old-oprd_isent.
*   MOVE oprd_ntrib               TO wl_dent_old-oprd_ntrib.
*   MOVE-CORRESPONDING wg_dentro  TO wl_dent_old.
**
*   MOVE oprf_trib                TO wl_fora_old-oprf_trib.
*   MOVE oprf_isent               TO wl_fora_old-oprf_isent.
*   MOVE oprf_ntrib               TO wl_fora_old-oprf_ntrib.
*   MOVE-CORRESPONDING wg_fora    TO wl_fora_old.
**
*   MOVE wl_desc_den_cfop         TO wl_dent_old-wl_desc_den_cfop.
*   MOVE wl_desc_den_taxlw1       TO wl_dent_old-wl_desc_den_taxlw1.
*   MOVE wl_desc_den_taxlw2       TO wl_dent_old-wl_desc_den_taxlw2.
*   MOVE wl_desc_den_taxlw3       TO wl_dent_old-wl_desc_den_taxlw3.
*   MOVE wl_desc_den_taxlw4       TO wl_dent_old-wl_desc_den_taxlw4.
*   MOVE wl_desc_den_taxlw5       TO wl_dent_old-wl_desc_den_taxlw5.
*   MOVE wl_desc_den_taxcode      TO wl_dent_old-wl_desc_den_taxcode.
**
*   MOVE wl_desc_for_cfop         TO wl_fora_old-wl_desc_for_cfop.
*   MOVE wl_desc_for_taxlw1       TO wl_fora_old-wl_desc_for_taxlw1.
*   MOVE wl_desc_for_taxlw2       TO wl_fora_old-wl_desc_for_taxlw2.
*   MOVE wl_desc_for_taxlw3       TO wl_fora_old-wl_desc_for_taxlw3.
*   MOVE wl_desc_for_taxlw4       TO wl_fora_old-wl_desc_for_taxlw4.
*   MOVE wl_desc_for_taxlw5       TO wl_fora_old-wl_desc_for_taxlw5.
*   MOVE wl_desc_for_taxcode      TO wl_fora_old-wl_desc_for_taxcode.
*
    clear: oprd_trib,          oprd_isent,         oprd_ntrib,         wg_dentro,
    oprf_trib,          oprf_isent,         oprf_ntrib,         wg_fora,
    wl_desc_den_cfop,   wl_desc_den_taxlw1, wl_desc_den_taxlw2, wl_desc_den_taxlw3,
    wl_desc_den_taxlw4, wl_desc_den_taxlw5, wl_desc_den_taxcode,
    wl_desc_for_cfop,   wl_desc_for_taxlw1, wl_desc_for_taxlw2, wl_desc_for_taxlw3,
    wl_desc_for_taxlw4, wl_desc_for_taxlw5, wl_desc_for_taxcode.

    oprd_trib         = abap_true.
    oprf_trib         = abap_true.
    wg_dentro-taxcode = 'V2'.
    wg_fora-taxcode   = 'V2'.

    select single txt
    from j_1btxsdct
    into wl_desc_den_taxcode
    where langu   eq sy-langu
    and taxcode eq wg_dentro-taxcode.

    select single txt
    from j_1btxsdct
    into wl_desc_for_taxcode
    where langu   eq sy-langu
    and taxcode eq wg_fora-taxcode.

  elseif zfiwrt0001-complement_icms = abap_off.

    move wl_dent_old-oprd_trib           to oprd_trib.
    move wl_dent_old-oprd_isent          to oprd_isent.
    move wl_dent_old-oprd_ntrib          to oprd_ntrib.
    move-corresponding wl_dent_old       to wg_dentro.
*
    move wl_fora_old-oprf_trib           to oprf_trib.
    move wl_fora_old-oprf_isent          to oprf_isent.
    move wl_fora_old-oprf_ntrib          to oprf_ntrib.
    move-corresponding wl_fora_old       to wg_fora.
*
    move wl_dent_old-wl_desc_den_cfop    to wl_desc_den_cfop.
    move wl_dent_old-wl_desc_den_taxlw1  to wl_desc_den_taxlw1.
    move wl_dent_old-wl_desc_den_taxlw2  to wl_desc_den_taxlw2.
    move wl_dent_old-wl_desc_den_taxlw3  to wl_desc_den_taxlw3.
    move wl_dent_old-wl_desc_den_taxlw4  to wl_desc_den_taxlw4.
    move wl_dent_old-wl_desc_den_taxlw5  to wl_desc_den_taxlw5.
    move wl_dent_old-wl_desc_den_taxcode to wl_desc_den_taxcode.
*
    move wl_fora_old-wl_desc_for_cfop    to wl_desc_for_cfop.
    move wl_fora_old-wl_desc_for_taxlw1  to wl_desc_for_taxlw1.
    move wl_fora_old-wl_desc_for_taxlw2  to wl_desc_for_taxlw2.
    move wl_fora_old-wl_desc_for_taxlw3  to wl_desc_for_taxlw3.
    move wl_fora_old-wl_desc_for_taxlw4  to wl_desc_for_taxlw4.
    move wl_fora_old-wl_desc_for_taxlw5  to wl_desc_for_taxlw5.
    move wl_fora_old-wl_desc_for_taxcode to wl_desc_for_taxcode.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0112   text
*      -->P_C_0  text
*----------------------------------------------------------------------*
form trata_campos  using    p_field
      p_value
      p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  append tg_fields.

endform.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_LIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1021   text
*      <--P_WL_LINE  text
*----------------------------------------------------------------------*
form get_current_lin  using    value(tc)
changing wl_line.
  data: wl_topline(30) type c.
  field-symbols: <fs_tc> type any.
  concatenate tc '-top_line' into wl_topline.
  assign (wl_topline) to <fs_tc>.
*   CONCATENATE TC '-top_line' INTO WL_TOPLINE.
  get cursor line wl_line.
  wl_line = <fs_tc> + wl_line - 1.
endform.                    " GET_CURRENT_LIN

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_IMPOSTOS' ITSELF
controls: tc_impostos type tableview using screen 0202.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_IMPOSTOS'
data:     g_tc_impostos_lines  like sy-loopc.

data:     ok_code like sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module tc_impostos_change_tc_attr output.
  describe table tg_impo lines tc_impostos-lines.
endmodule.                    "TC_IMPOSTOS_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
module tc_impostos_get_lines output.
  g_tc_impostos_lines = sy-loopc.
endmodule.                    "TC_IMPOSTOS_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module tc_impostos_modify input.
  read table tg_impo transporting no fields
  with key taxtyp = tg_impo-taxtyp.

  if sy-subrc is initial.
    message e000(zwrm001) with 'Já existe um registro com esse tipo de imposto'
    'na tabela'.
  else.
    if tg_impo-taxtyp is initial.
      message e000(zwrm001) with 'Nao é possivel entra com o campo em branco'.
    else.
      select single taxgrp
      from j_1baj
      into tg_impo-taxgrp
      where taxtyp eq tg_impo-taxtyp.

      if sy-subrc is not initial.
        clear: tg_impo-taxgrp.

      endif.

      select single ttypetxt
      from j_1bajt
      into  tg_impo-ttypetxt
      where  spras  eq sy-langu
      and  taxtyp eq tg_impo-taxtyp.

      if sy-subrc is not initial.
        clear: tg_impo-ttypetxt.
      endif.
*      ELSE.
*        MESSAGE E000(ZWRM001) WITH 'O registro não foi encontrado na tabela'
*                                   '"Tipos de imposto"'.
*        CLEAR: TG_IMPO-TAXGRP.
*      ENDIF.
    endif.
    modify tg_impo
    index tc_impostos-current_line.
    if sy-subrc is not initial.
      append tg_impo.
    endif.
    clear: tg_impo.
  endif.
endmodule.                    "TC_IMPOSTOS_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
module tc_impostos_mark input.
  data: g_tc_impostos_wa2 like line of tg_impo.
  if tc_impostos-line_sel_mode = 1
  and tg_impo-mark = 'X'.
    loop at tg_impo into g_tc_impostos_wa2
    where mark = 'X'.
      g_tc_impostos_wa2-mark = ''.
      modify tg_impo
      from g_tc_impostos_wa2
      transporting mark.
    endloop.
  endif.
  modify tg_impo
  index tc_impostos-current_line
  transporting mark.
endmodule.                    "TC_IMPOSTOS_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_IMPOSTOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
module tc_impostos_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'TC_IMPOSTOS'
        'TG_IMPO'
        'MARK'
  changing ok_code.
  sy-ucomm = ok_code.
endmodule.                    "TC_IMPOSTOS_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
form user_ok_tc using    p_tc_name type dynfnam
      p_table_name
      p_mark_name
changing p_ok      like sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data: l_ok     type sy-ucomm,
        l_offset type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  search p_ok for p_tc_name.
  if sy-subrc <> 0.
    exit.
  endif.
  l_offset = strlen( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  case l_ok.
    when 'INSR'.                      "insert row
      perform fcode_insert_row using    p_tc_name
            p_table_name.
      clear p_ok.

    when 'DELE'.                      "delete row
      perform fcode_delete_row using    p_tc_name
            p_table_name
            p_mark_name.
      clear p_ok.

    when 'P--' or                     "top of list
      'P-'  or                     "previous page
      'P+'  or                     "next page
      'P++'.                       "bottom of list
      perform compute_scrolling_in_tc using p_tc_name
            l_ok.
      clear p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    when 'MARK'.                      "mark all filled lines
      perform fcode_tc_mark_lines using p_tc_name
            p_table_name
            p_mark_name   .
      clear p_ok.

    when 'DMRK'.                      "demark all filled lines
      perform fcode_tc_demark_lines using p_tc_name
            p_table_name
            p_mark_name .
      clear p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.
    when 'LOG_0003'.
      perform view_log_contab.
  endcase.

endform.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_insert_row
using    p_tc_name           type dynfnam
      p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_lines_name       like feld-name.
  data l_selline          like sy-stepl.
  data l_lastline         type i.
  data l_line             type i.
  data l_table_name       like feld-name.
  field-symbols <tc>                 type cxtab_control.
  field-symbols <table>              type standard table.
  field-symbols <lines>              type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_lines_name.
  assign (l_lines_name) to <lines>.

*&SPWIZARD: get current line                                           *
  get cursor line l_selline.
  if sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    if l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    else.
      <tc>-top_line = 1.
    endif.
  else.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  endif.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  insert initial line into <table> index l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  set cursor line l_line.

endform.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
form fcode_delete_row
using    p_tc_name           type dynfnam
      p_table_name
      p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  describe table <table> lines <tc>-lines.

  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    if <mark_field> = 'X'.
      delete <table> index syst-tabix.
      if sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      endif.
    endif.
  endloop.

endform.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
form compute_scrolling_in_tc using    p_tc_name
      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_tc_new_top_line     type i.
  data l_tc_name             like feld-name.
  data l_tc_lines_name       like feld-name.
  data l_tc_field_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <lines>      type i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  concatenate 'G_' p_tc_name '_LINES' into l_tc_lines_name.
  assign (l_tc_lines_name) to <lines>.


*&SPWIZARD: is no line filled?                                         *
  if <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  else.
*&SPWIZARD: no, ...                                                    *
    call function 'SCROLLING_IN_TABLE'
      exporting
        entry_act      = <tc>-top_line
        entry_from     = 1
        entry_to       = <tc>-lines
        last_page_full = 'X'
        loops          = <lines>
        ok_code        = p_ok
        overlapping    = 'X'
      importing
        entry_new      = l_tc_new_top_line
      exceptions
*       NO_ENTRY_OR_PAGE_ACT  = 01
*       NO_ENTRY_TO    = 02
*       NO_OK_CODE_OR_PAGE_GO = 03
        others         = 0.
  endif.

*&SPWIZARD: get actual tc and column                                   *
  get cursor field l_tc_field_name
  area  l_tc_name.

  if syst-subrc = 0.
    if l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      set cursor field l_tc_field_name line 1.
    endif.
  endif.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


endform.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_mark_lines using p_tc_name
      p_table_name
      p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = 'X'.
  endloop.
endform.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_demark_lines using p_tc_name
      p_table_name
      p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  assign (p_tc_name) to <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  concatenate p_table_name '[]' into l_table_name. "table body
  assign (l_table_name) to <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  loop at <table> assigning <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    assign component p_mark_name of structure <wa> to <mark_field>.

    <mark_field> = space.
  endloop.
endform.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  MD_CHANGE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module md_change_screen output.
  data: tl_tbsl type table of tbsl with header line.
  refresh: tl_tbsl.
  if tg_contab[] is not initial.


    select *
    from tbsl
    into table tl_tbsl
    for all entries in tg_contab
    where bschl eq tg_contab-bschl.

    sort: tl_tbsl by bschl.
  endif.

  perform trata_acao_editar. "PSA

  if wg_acao eq c_add.
    if tg_impo-taxtyp is initial.
      loop at screen.
        if screen-name = 'TG_IMPO-TAXTYP'.
          screen-input = 1.
        endif.
        modify screen.

      endloop.
    endif.
** Contabilizacao
    if tg_contab-bschl is  initial
    and tg_contab-hkont is  initial
    and tg_contab-taxtyp is initial
    and tg_contab-estorno is initial.
      loop at screen.
        if screen-name = 'TG_CONTAB-BSCHL'
*        OR screen-name = 'TG_CONTAB-HKONT'
        or screen-name = 'TG_CONTAB-TAXTYP'
        or screen-name = 'TG_CONTAB-ESTORNO'
        or screen-name = 'TG_CONTAB-UMSKZ'
        or screen-name = 'TG_CONTAB-NEWBW'.
          screen-input = 1.

        endif.
        if screen-name = 'TG_CONTAB-HKONT'.
          read table tl_tbsl
          with key bschl = tg_contab-bschl
          binary search.
          if tl_tbsl-koart eq 'D'
          or tl_tbsl-koart eq 'K'.
            screen-input = 0.
          else.
            screen-input = 1.
          endif.
        endif.
        modify screen.
      endloop.
    endif.
** Movimentacao de estoque
    if tg_movest-bwart is initial
    and tg_movest-tcode is initial
    and tg_movest-mwskz1 is initial.
      loop at screen.
        if screen-name = 'TG_MOVEST-BWART'
        or screen-name = 'TG_MOVEST-TCODE'
        or screen-name = 'TG_MOVEST-MWSKZ1'
        or screen-name = 'TG_MOVEST-ESTORNO'.
          screen-input = 1.
        endif.
        modify screen.

      endloop.
    endif.

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
**CFOP
    if tg_cfop-uf is initial
    and tg_cfop-cfop is initial.
      loop at screen.
        if ( screen-name = 'TG_CFOP-UF'
        or screen-name = 'TG_CFOP-CFOP' )
        and zfiwrt0001-valida_cfop eq 'S'.
          screen-input = 1.
        endif.
        modify screen.

      endloop.
    endif.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

  elseif wg_acao eq c_modif.
*    LOOP AT SCREEN.
*      IF screen-name = 'TG_CONTAB-BSCHL'
**      OR screen-name = 'TG_CONTAB-HKONT'
*      OR screen-name = 'TG_CONTAB-TAXTYP'
*      OR screen-name = 'TG_CONTAB-ESTORNO'
*      OR screen-name = 'TG_IMPO-TAXTYP'
*      OR screen-name = 'TG_MOVEST-BWART'
*      OR screen-name = 'TG_MOVEST-TCODE'
*      OR screen-name = 'TG_MOVEST-MWSKZ1'
*      OR screen-name = 'TG_MOVEST-ESTORNO'
*      OR screen-name = 'TG_CONTAB-UMSKZ'
*      OR screen-name = 'TG_CONTAB-NEWBW'.
*        screen-input = 1.
*      ENDIF.
**Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
*      IF ( screen-name = 'TG_CFOP-UF'
*      OR screen-name = 'TG_CFOP-CFOP' )
*      AND zfiwrt0001-valida_cfop EQ 'S'.
*        screen-input = 1.
*      ENDIF.
**Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
*
*
*      IF screen-name = 'TG_CONTAB-HKONT'.
*        READ TABLE tl_tbsl
*        WITH KEY bschl = tg_contab-bschl
*        BINARY SEARCH.
*        IF tl_tbsl-koart EQ 'D'
*        OR tl_tbsl-koart EQ 'K'.
*          screen-input = 0.
*        ELSE.
*          screen-input = 1.
*        ENDIF.
*      ENDIF.
*      MODIFY SCREEN.
*
*    ENDLOOP.
  endif.
endmodule.                 " MD_CHANGE_SCREEN  OUTPUT

module mdc_change_screen output.
  perform trata_acao_editar. "PSA
endmodule.                 " MD_CHANGE_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MATCHCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module matchcode_taxtyp input.
  data: tl_dynpselect   type table of dselc with header line,
        tl_dynpvaluetab type table of dval with header line,
        wl_help_info    type help_info,
        wl_selected     type help_info-fldvalue.

  wl_help_info-call	     = 'V'.
  wl_help_info-object	   = 'F'.
  wl_help_info-program   = sy-repid.
  wl_help_info-dynpro	   = sy-dynnr.
  wl_help_info-tabname   = 'ZFIWRT0002'.
  wl_help_info-fieldname = 'TAXTYP'.
*wl_HELP_INFO-FIELDTYPE	                                   	CHAR
  wl_help_info-spras     = sy-langu.
  wl_help_info-menufunct = 'HC'.

  call function 'HELP_START'
    exporting
      help_infos   = wl_help_info
    importing
      select_value = wl_selected
    tables
      dynpselect   = tl_dynpselect
      dynpvaluetab = tl_dynpvaluetab.

  if wl_selected is not initial.
    tg_impo-taxtyp = wl_selected.

  endif.

endmodule.                 " MATCHCODE  INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_CONTAB' ITSELF
controls: tc_contab type tableview using screen 0204.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_CONTAB'
data:     g_tc_contab_lines  like sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module tc_contab_change_tc_attr output.
  describe table tg_contab lines tc_contab-lines.
endmodule.                    "TC_CONTAB_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
module tc_contab_get_lines output.
  g_tc_contab_lines = sy-loopc.
endmodule.                    "TC_CONTAB_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module tc_contab_modify input.
  modify tg_contab
  index tc_contab-current_line.
  if sy-subrc is not initial.
*    SELECT SINGLE BSCHL
*      FROM TBSL
*      INTO TG_CONTAB-BSCHL
*       WHERE BSCHL EQ TG_CONTAB-BSCHL.
*
*    IF SY-SUBRC IS INITIAL.
*      SELECT SINGLE SAKNR
*        FROM SKA1
*        INTO TG_CONTAB-HKONT
*         WHERE SAKNR EQ TG_CONTAB-HKONT.
*
*      IF SY-SUBRC IS INITIAL.
*
*      ELSE.
*        MESSAGE E000(ZWRM001) WITH 'O "Chv.Lançamento" não foi encontrado na tabela'
*                                   '"Mestre de contas do Razão"'.
*      ENDIF.
*
*    ELSE.
*      MESSAGE E000(ZWRM001) WITH 'O "Cont. Razão" não foi encontrado na tabela'
*                                 '"Chave de lançamento"'.
*
*    ENDIF.
    append tg_contab.
  endif.
  clear: tg_contab.
endmodule.                    "TC_CONTAB_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
module tc_contab_mark input.
  data: g_tc_contab_wa2 like line of tg_contab.
  if tc_contab-line_sel_mode = 1
  and tg_contab-mark = 'X'.
    loop at tg_contab into g_tc_contab_wa2
    where mark = 'X'.
      g_tc_contab_wa2-mark = ''.
      modify tg_contab
      from g_tc_contab_wa2
      transporting mark.
    endloop.
  endif.
  modify tg_contab
  index tc_contab-current_line
  transporting mark.
endmodule.                    "TC_CONTAB_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_CONTAB'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
module tc_contab_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'TC_CONTAB'
        'TG_CONTAB'
        'MARK'
  changing ok_code.
  sy-ucomm = ok_code.
endmodule.                    "TC_CONTAB_USER_COMMAND INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_MOVEST' ITSELF
controls: tc_movest type tableview using screen 0205.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_MOVEST'
data:     g_tc_movest_lines  like sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
module tc_movest_change_tc_attr output.
  describe table tg_movest lines tc_movest-lines.
endmodule.                    "TC_MOVEST_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
module tc_movest_get_lines output.
  g_tc_movest_lines = sy-loopc.
endmodule.                    "TC_MOVEST_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
module tc_movest_modify input.
  modify tg_movest
  index tc_movest-current_line.
  if sy-subrc is not initial.
    append tg_movest.
  endif.
  clear: tg_movest.
endmodule.                    "TC_MOVEST_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
module tc_movest_mark input.
  data: g_tc_movest_wa2 like line of tg_movest.
  if tc_movest-line_sel_mode = 1
  and tg_movest-mark = 'X'.
    loop at tg_movest into g_tc_movest_wa2
    where mark = 'X'.
      g_tc_movest_wa2-mark = ''.
      modify tg_movest
      from g_tc_movest_wa2
      transporting mark.
    endloop.
  endif.
  modify tg_movest
  index tc_movest-current_line
  transporting mark.
endmodule.                    "TC_MOVEST_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC_MOVEST'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
module tc_movest_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'TC_MOVEST'
        'TG_MOVEST'
        'MARK'
  changing ok_code.
  sy-ucomm = ok_code.
endmodule.                    "TC_MOVEST_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form verifica_erros .
  data: wl_cont     type sy-tabix,
        wl_linha(6),
        wl_1bad     type j_1bad,
        tl_j1baj    type table of j_1baj with header line,
        tl_tbsl     type table of tbsl   with header line,
        tl_ska1     type table of ska1   with header line,
        tl_t156     type table of t156   with header line,
        tl_tstc     type table of tstc   with header line,
        tl_tmrm007a type table of tmrm007a with header line,
        wl_cont_aux type sy-tabix.

  clear: wl_cont_aux.
  refresh: tg_msg_ret, tl_j1baj, tl_tbsl, tl_ska1, tl_t156,
  tl_tstc, tl_tmrm007a.

  if tg_impo[] is not initial.
    select *
    from j_1baj
    into table tl_j1baj
    for all entries in tg_impo
    where taxtyp eq tg_impo-taxtyp.

  endif.


  if tg_contab[] is not initial.
    select *
    from tbsl
    into table tl_tbsl
    for all entries in tg_contab
    where bschl eq tg_contab-bschl.

    select *
    from ska1
    into table tl_ska1
    for all entries in tg_contab
    where saknr eq tg_contab-hkont.

  endif.

  if tg_movest[] is not initial.
    select *
    from t156
    into table tl_t156
    for all entries in tg_movest
    where bwart eq tg_movest-bwart.

    select *
    from tstc
    into table tl_tstc
    for all entries in tg_movest
    where tcode eq tg_movest-tcode.

    select *
    from tmrm007a
    into table tl_tmrm007a
    for all entries in tg_movest
    where mwskz eq tg_movest-mwskz1.

  endif.
** Tela geral
  if zfiwrt0001-descricao is initial.
    wg_msg_ret-msg = text-e01. "O campo "Descrição" precisa ser preenchido
*    WG_MSG_RET-ABA =
    wg_msg_ret-field = 'ZFIWRT0001-DESCRICAO'.
    append wg_msg_ret to tg_msg_ret.
    clear: wg_msg_ret.
  endif.

  if zfiwrt0001-ge_remessa eq 'S'.


  endif.

  if zfiwrt0001-lm_estoque ne 'S'.
**Dados Gerais
    if zfiwrt0001-nftype is initial.
      wg_msg_ret-msg = text-e02. "O campo "Ctg.NF" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-NFTYPE'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-itmtyp is initial.
      wg_msg_ret-msg = text-e03. "O campo "Tipo de item NF" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-ITMTYP'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-parvw is initial.
      wg_msg_ret-msg = text-e22. "O campo "NF função parc." na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-PARVW'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    else.
      select single *
      from j_1bad
      into wl_1bad
      where parvw eq zfiwrt0001-parvw.

      if sy-subrc is not initial.
        wg_msg_ret-msg = text-e23. "A Função Parçeiro selecionada não existe!
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'ZFIWRT0001-PARVW'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.
    endif.

    if zfiwrt0001-retorno is initial.
      wg_msg_ret-msg = text-e19. "O campo "Nota Fiscal de Retorno" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-RETORNO'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-zpesagem is initial.
      wg_msg_ret-msg = text-e20. "O campo "Tipo de Pesagem" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-ZPESAGEM'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-imobilizado is initial.
      wg_msg_ret-msg = text-e21. "O campo "imobilizado" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-IMOBILIZADO'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-imobilizado = 'S' and zfiwrt0001-tp_mv_imob is initial.
      wg_msg_ret-msg = text-e34. "Informe o tipo de movimento do imobilizado na aba Dados Gerais.
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-TP_MV_IMOB'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    else.
*    CLEAR ZFIWRT0001-TP_MV_IMOB .
    endif.

    if zfiwrt0001-ctrl_zrfl is initial.
      wg_msg_ret-msg = text-e28. "O campo "Controle Formação Lote" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-CTRL_ZRFL'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-energia is initial.
      wg_msg_ret-msg = text-e29. "O campo "Energia" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-ENERGIA'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-complemento eq 'S' and zfiwrt0001-transf_icms is initial.
      wg_msg_ret-msg = text-e33. "O campo "Energia" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-TRANSF_ICMS'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-servico is initial.
      wg_msg_ret-msg = text-e32. "O campo "Serviço" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-SERVICO'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

*-CS2023000043-09.02.2023-#102019-JT-inicio
    if zfiwrt0001-complement_icms is initial.
      wg_msg_ret-msg = text-e40. "O campo "Complementar ICMS" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-COMPLEMENT_ICMS'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.
*-CS2023000043-09.02.2023-#102019-JT-fim

    if zfiwrt0001-disp_nf_cct is initial.
      wg_msg_ret-msg = text-e35. "O campo "Disp. NF CCT" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-DISP_NF_CCT'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

**********************************************************************"150184 CS2024000781 Aprovações ZNFW - PSA
    if sy-ucomm = 'DESAT' and zfiwrt0001-status_aprov <> 'A'.
      wg_msg_ret-msg = text-e45. "Operação não aprovada!
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-STATUS_APROV'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.


    if zfiwrt0001-dt_ini_val is initial or zfiwrt0001-dt_ini_val = '00000000'.
      wg_msg_ret-msg = text-e42. "O campo "Disp. NF CCT" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-DT_INI_VAL'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-dt_fim_val is initial or zfiwrt0001-dt_fim_val = '00000000'.
      wg_msg_ret-msg = text-e43. "O campo "Disp. NF CCT" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-DT_FIM_VAL'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

    if zfiwrt0001-dep_resp is initial.
      wg_msg_ret-msg = text-e44. "O campo "Disp. NF CCT" na aba Dados Gerais, precisa ser preenchido
      wg_msg_ret-aba = c_tab_strip_nf-tab1.
      wg_msg_ret-field = 'ZFIWRT0001-DEP_RESP'.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

**********************************************************************


***>Dados fiscais dentro do estado
    if zfiwrt0001-complement_icms = 'N'.  "*-CS2023000043-09.02.2023-#102019-JT
      if  oprd_trib  is initial
      and oprd_isent is initial
      and oprd_ntrib is initial.
        wg_msg_ret-msg = text-e24. "É preciso preencher um "Tipo de Operação" na aba Dados Gerais, dentro do estado.
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'OPRD_TRIB'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.

      else.
        clear: wl_cont_aux.
        if oprd_trib is not initial.
          add 1 to wl_cont_aux.
        endif.

        if oprd_isent is not initial.
          add 1 to wl_cont_aux.
        endif.

        if oprd_ntrib is not initial.
          add 1 to wl_cont_aux.
        endif.

        if wl_cont_aux gt 1.
          wg_msg_ret-msg = text-e25. "Só é possivel preencher  um "Tipo de Operação" na aba Dados Gerais, dentro do estado.
          wg_msg_ret-aba = c_tab_strip_nf-tab1.
          wg_msg_ret-field = 'OPRD_TRIB'.
          append wg_msg_ret to tg_msg_ret.
          clear: wg_msg_ret.
        endif.
      endif.
    endif.

    if zfiwrt0001-complement_icms = 'N'.  "*-CS2023000043-09.02.2023-#102019-JT
      if wg_dentro-taxlw1 is initial.
        wg_msg_ret-msg = text-e04. "O campo "Dir.fisc.: ICMS" na aba Dados Gerais, dentro do estado,  precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_DENTRO-TAXLW1'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_dentro-taxlw2 is initial.
        wg_msg_ret-msg = text-e05. "O campo "Dir.fiscal: IPI" na aba Dados Gerais, dentro do estado precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_DENTRO-TAXLW2'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_dentro-taxlw4 is initial.
        wg_msg_ret-msg = text-e06. "O campo "Lei COFINS" na aba Dados Gerais, dentro do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_DENTRO-TAXLW4'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_dentro-taxlw5 is initial.
        wg_msg_ret-msg = text-e07. "O campo "Lei PIS" na aba Dados Gerais, dentro do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_DENTRO-TAXLW5'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_dentro-taxcode is initial.
        wg_msg_ret-msg = text-e30. "campo "Código do Imposto" na aba Dados Gerais, dentro do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_DENTRO-TAXCODE'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.
    endif.



***> Dados fiscais Fora do estado.
    if zfiwrt0001-complement_icms = 'N'.  "*-CS2023000043-09.02.2023-#102019-JT
      if  oprf_trib  is initial
      and oprf_isent is initial
      and oprf_ntrib is initial.
        wg_msg_ret-msg = text-e26. "É preciso preencher um "Tipo de Operação" na aba Dados Gerais, fora do estado.
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'OPRF_TRIB'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.

      else.
        clear: wl_cont_aux.
        if oprf_trib is not initial.
          add 1 to wl_cont_aux.
        endif.

        if oprf_isent is not initial.
          add 1 to wl_cont_aux.
        endif.

        if oprf_ntrib is not initial.
          add 1 to wl_cont_aux.
        endif.

        if wl_cont_aux gt 1.
          wg_msg_ret-msg = text-e27. "Só é possivel preencher  um "Tipo de Operação" na aba Dados Gerais, fora do estado.
          wg_msg_ret-aba = c_tab_strip_nf-tab1.
          wg_msg_ret-field = 'OPRF_TRIB'.
          append wg_msg_ret to tg_msg_ret.
          clear: wg_msg_ret.
        endif.
      endif.
    endif.

    if zfiwrt0001-complement_icms = 'N'.  "*-CS2023000043-09.02.2023-#102019-JT
      if wg_fora-taxlw1 is initial.
        wg_msg_ret-msg = text-e15. "O campo "Dir.fisc.: ICMS" na aba Dados Gerais, fora do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_FORA-TAXLW1'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_fora-taxlw2 is initial.
        wg_msg_ret-msg = text-e16. "O campo "Dir.fiscal: IPI" na aba Dados Gerais, fora do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_FORA-TAXLW2'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_fora-taxlw4 is initial.
        wg_msg_ret-msg = text-e17. "O campo "Lei COFINS" na aba Dados Gerais, fora do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_FORA-TAXLW4'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_fora-taxlw5 is initial.
        wg_msg_ret-msg = text-e18. "O campo "Lei PIS" na aba Dados Gerais, fora do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_FORA-TAXLW5'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.

      if wg_fora-taxcode is initial.
        wg_msg_ret-msg = text-e31. "O campo "Código do Imposto" na aba Dados Gerais, fora do estado, precisa ser preenchido
        wg_msg_ret-aba = c_tab_strip_nf-tab1.
        wg_msg_ret-field = 'WG_FORA-TAXCODE'.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.
      endif.
    endif.

** impostos
    describe table tg_impo lines wl_cont.
    if wl_cont is initial.
      wg_msg_ret-msg = text-e08.   "É preciso preencher pelo menos um tipo de imposto na aba Impostos
      wg_msg_ret-aba = c_tab_strip_nf-tab2.

      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    else.
*    READ TABLE tg_impo
*      WITH KEY taxtyp = space.
*    IF sy-subrc IS INITIAL.
*      wg_msg_ret-msg = text-e09.   "O campo "Tip. de Imposto" na aba Impostos precisa ser preenchido
*      APPEND wg_msg_ret TO tg_msg_ret.
*      CLEAR: wg_msg_ret.
*    ENDIF.
      sort: tl_j1baj by taxtyp.
      loop at tg_impo.
        wl_linha = sy-tabix.
        read table tl_j1baj
        with key taxtyp = tg_impo-taxtyp
        binary search.

        if sy-subrc is not initial.
          concatenate text-e12 wl_linha into wg_msg_ret-msg.
          wg_msg_ret-aba = c_tab_strip_nf-tab2.
          append wg_msg_ret to tg_msg_ret.
          clear: wg_msg_ret.
        endif.
      endloop.
    endif.
  elseif tg_movest[] is initial.
    wg_msg_ret-msg = text-e36.
    wg_msg_ret-aba = c_tab_strip_nf-tab2.

    append wg_msg_ret to tg_msg_ret.
    clear: wg_msg_ret.
  endif.


** Contabilização
  sort: tl_tbsl by bschl,
  tl_ska1 by saknr.

  loop at tg_contab.
*     WHERE BSCHL IS NOT INITIAL.
    wl_linha = sy-tabix.
    read table tl_tbsl
    with key bschl = tg_contab-bschl
    binary search.
    if sy-subrc is not initial.
      concatenate text-e13 wl_linha into wg_msg_ret-msg.
      wg_msg_ret-aba = c_tab_strip_nf-tab4.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.

    endif.

    if tg_contab-hkont is not initial.
      read table tl_ska1
      with key saknr = tg_contab-hkont
      binary search.
      if sy-subrc is not initial.
        concatenate text-e14 wl_linha into wg_msg_ret-msg.
        wg_msg_ret-aba = c_tab_strip_nf-tab4.
        append wg_msg_ret to tg_msg_ret.
        clear: wg_msg_ret.

      endif.
    endif.

    if  tg_contab-bschl is not initial
    and tg_contab-hkont is initial
    and tl_tbsl-koart ne 'D'
    and tl_tbsl-koart ne 'K'.

      concatenate text-e10 wl_linha into wg_msg_ret-msg.
      wg_msg_ret-aba = c_tab_strip_nf-tab4.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.

  endloop.

** Movimentação do estoque
  loop at tg_movest
  where bwart is not initial.
    wl_linha = sy-tabix.

    if tg_movest-tcode is initial.
      concatenate text-e11 wl_linha into wg_msg_ret-msg.
      wg_msg_ret-aba = c_tab_strip_nf-tab5.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.
  endloop.

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  if zfiwrt0001-valida_cfop eq 'S' and tg_cfop[] is initial.
    if ok-code eq c_save.
      wg_msg_ret-msg = text-e41. "É necessário adicionar ao menos um CFOP Permitido!
      wg_msg_ret-aba = c_tab_strip_nf-tab7.
      append wg_msg_ret to tg_msg_ret.
      clear: wg_msg_ret.
    endif.
  endif.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

  call function 'Z_DOC_CHECK_NEW'
    exporting
      i_screen      = '100'
*     I_SHOW        = C_X
      i_repid       = sy-repid
      i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
    importing
      e_messagem    = wg_mensagens
    tables
      it_msgs       = tg_msg_ret.


endform.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_201  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_201 input.
  case ok-code.
    when c_search.
      perform busca_descricoes.
  endcase.
endmodule.                 " USER_COMMAND_201  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_descricoes .

  if zfiwrt0001-nftype is not initial.
    select single nfttxt
    from j_1baat
    into wl_desc_nftype
    where spras  eq sy-langu
    and nftype eq zfiwrt0001-nftype.

  endif.

  if zfiwrt0001-itmtyp is not initial.
    select single text
    from j_1bitemtypest
    into wl_desc_itmtyp
    where spras  eq sy-langu
    and itmtyp eq zfiwrt0001-itmtyp.

  endif.

  if zfiwrt0001-parvw is not initial.
    select single partxt
    from j_1badt
    into wl_desc_parvw
    where spras eq sy-langu
    and parvw eq zfiwrt0001-parvw.

  endif.

** Dentro do estado
  if wg_dentro-cfop is not initial.
    select single cfotxt
    from j_1bagnt
    into wl_desc_den_cfop
    where spras  eq sy-langu
    and cfop eq wg_dentro-cfop.
  endif.

  if wg_dentro-taxlw1 is not initial.
    select single descrip
    from j_1batl1t
    into wl_desc_den_taxlw1
    where langu  eq sy-langu
    and taxlaw eq wg_dentro-taxlw1.
  endif.

  if wg_dentro-taxlw2 is not initial.
    select single descrip
    from j_1batl2t
    into wl_desc_den_taxlw2
    where langu  eq sy-langu
    and taxlaw eq wg_dentro-taxlw2.
  endif.

  if wg_dentro-taxlw4 is not initial.
    select single descrip
    from j_1batl4t
    into wl_desc_den_taxlw4
    where langu  eq sy-langu
    and taxlaw eq wg_dentro-taxlw4.
  endif.

  if wg_dentro-taxlw5 is not initial.
    select single descrip
    from j_1batl5t
    into wl_desc_den_taxlw5
    where langu  eq sy-langu
    and taxlaw eq wg_dentro-taxlw5.
  endif.


  if wg_dentro-taxlw3 is not initial.
    select single descrip
    from j_1batl3t
    into wl_desc_den_taxlw3
    where langu  eq sy-langu
    and taxlaw eq wg_dentro-taxlw3.
  endif.



  if wg_dentro-taxcode is not initial.
    select single txt
    from j_1btxsdct
    into wl_desc_den_taxcode
    where langu   eq sy-langu
    and taxcode eq wg_dentro-taxcode.
  endif.

** Fora do estado.
  if wg_fora-cfop is not initial.
    select single cfotxt
    from j_1bagnt
    into wl_desc_for_cfop
    where spras  eq sy-langu
    and cfop eq wg_fora-cfop.
  endif.

  if wg_fora-taxlw1 is not initial.
    select single descrip
    from j_1batl1t
    into wl_desc_for_taxlw1
    where langu  eq sy-langu
    and taxlaw eq wg_fora-taxlw1.
  endif.

  if wg_fora-taxlw2 is not initial.
    select single descrip
    from j_1batl2t
    into wl_desc_for_taxlw2
    where langu  eq sy-langu
    and taxlaw eq wg_fora-taxlw2.
  endif.

  if wg_fora-taxlw4 is not initial.
    select single descrip
    from j_1batl4t
    into wl_desc_for_taxlw4
    where langu  eq sy-langu
    and taxlaw eq wg_fora-taxlw4.
  endif.

  if wg_fora-taxlw5 is not initial.
    select single descrip
    from j_1batl5t
    into wl_desc_for_taxlw5
    where langu  eq sy-langu
    and taxlaw eq wg_fora-taxlw5.
  endif.

  if wg_fora-taxlw3 is not initial.
    select single descrip
    from j_1batl3t
    into wl_desc_for_taxlw3
    where langu  eq sy-langu
    and taxlaw eq wg_fora-taxlw3.
  endif.

  if wg_fora-taxcode is not initial.
    select single txt
    from j_1btxsdct
    into wl_desc_for_taxcode
    where langu   eq sy-langu
    and taxcode eq wg_fora-taxcode.
  endif.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  move wl_desc_den_cfop     to wl_dent_old-wl_desc_den_cfop.
  move wl_desc_den_taxlw1   to wl_dent_old-wl_desc_den_taxlw1.
  move wl_desc_den_taxlw2   to wl_dent_old-wl_desc_den_taxlw2.
  move wl_desc_den_taxlw3   to wl_dent_old-wl_desc_den_taxlw3.
  move wl_desc_den_taxlw4   to wl_dent_old-wl_desc_den_taxlw4.
  move wl_desc_den_taxlw5   to wl_dent_old-wl_desc_den_taxlw5.
  move wl_desc_den_taxcode  to wl_dent_old-wl_desc_den_taxcode.
*
  move wl_desc_for_cfop     to wl_fora_old-wl_desc_for_cfop.
  move wl_desc_for_taxlw1   to wl_fora_old-wl_desc_for_taxlw1.
  move wl_desc_for_taxlw2   to wl_fora_old-wl_desc_for_taxlw2.
  move wl_desc_for_taxlw3   to wl_fora_old-wl_desc_for_taxlw3.
  move wl_desc_for_taxlw4   to wl_fora_old-wl_desc_for_taxlw4.
  move wl_desc_for_taxlw5   to wl_fora_old-wl_desc_for_taxlw5.
  move wl_desc_for_taxcode  to wl_fora_old-wl_desc_for_taxcode.
*-CS2023000043-09.02.2023-#102019-JT-fim

endform.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Form  CRIA_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cria_documento .
  data: wl_input_0001  type zfiwrt0001,
        tl_input_0002  type table of zfiwrt0002  with header line,
        tl_input_0003  type table of zfiwrt0003  with header line,
        tl_input_0003l type table of zfiwrt0003l with header line,
        tl_input_0004  type table of zfiwrt0004  with header line,
        tl_input_0005  type table of zfiwrt0005  with header line,
        tl_input_0006  type table of zfiwrt0006  with header line,
*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
        tl_input_0007  type table of zfiwrt0028  with header line,
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
        tl_status_abas type table of zfiwrt0017  with header line,
        wl_0001_aux    type zfiwrt0001,
        wl_status      type zfiwrt0017-aba_blq.

  clear: wl_input_0001, tl_input_0002, tl_input_0003, tl_input_0003l, tl_input_0004, tl_input_0005,
  tl_input_0006, wl_0001_aux.
  refresh: tl_input_0002, tl_input_0003, tl_input_0003l, tl_input_0004, tl_input_0005,
  tl_input_0006, tg_editor.

  select single *
  from zfiwrt0001
  into wl_0001_aux
  where operacao eq zfiwrt0001-operacao.
  if sy-subrc is not initial.
    move:  sy-uname to wl_input_0001-usnam,
    sy-datum to wl_input_0001-dt_criacao,
    sy-uzeit to wl_input_0001-hr_criacao.
  else.
    move: wl_0001_aux-usnam      to wl_input_0001-usnam,
    wl_0001_aux-dt_criacao to wl_input_0001-dt_criacao,
    wl_0001_aux-hr_criacao to wl_input_0001-hr_criacao.

  endif.

  if zfiwrt0001-imobilizado ne 'S'.
    clear zfiwrt0001-tp_mv_imob.
  endif.

  if zfiwrt0001-status_aprov is initial . "150184 CS2024000781 Aprovações ZNFW - PSA (Grava L)
    zfiwrt0001-status_aprov = 'L'.
  endif.

** zfiwrt0001
  move: sy-mandt                 to wl_input_0001-mandt,
  zfiwrt0001-operacao      to wl_input_0001-operacao,
  zfiwrt0001-descricao     to wl_input_0001-descricao,
  zfiwrt0001-nftype        to wl_input_0001-nftype,
  zfiwrt0001-itmtyp        to wl_input_0001-itmtyp,
  c_b                      to wl_input_0001-opr_blq,
  zfiwrt0001-dias          to wl_input_0001-dias,
  zfiwrt0001-retorno       to wl_input_0001-retorno,
  zfiwrt0001-zpesagem      to wl_input_0001-zpesagem,
  zfiwrt0001-imobilizado   to wl_input_0001-imobilizado,
  zfiwrt0001-tp_mv_imob    to wl_input_0001-tp_mv_imob,
  zfiwrt0001-ctrl_zrfl     to wl_input_0001-ctrl_zrfl,
  zfiwrt0001-energia       to wl_input_0001-energia,
  zfiwrt0001-servico       to wl_input_0001-servico,
  zfiwrt0001-disp_nf_cct   to wl_input_0001-disp_nf_cct,
  zfiwrt0001-transf_icms   to wl_input_0001-transf_icms,
  zfiwrt0001-complemento   to wl_input_0001-complemento,
  zfiwrt0001-complement_icms to wl_input_0001-complement_icms, "*-CS2023000043-09.02.2023-#102019-JT
  zfiwrt0001-parvw         to wl_input_0001-parvw,
  zfiwrt0001-aviso_rec     to wl_input_0001-aviso_rec,
  zfiwrt0001-lm_estoque    to wl_input_0001-lm_estoque,
  zfiwrt0001-lm_aprova     to wl_input_0001-lm_aprova,
  zfiwrt0001-lm_indea      to wl_input_0001-lm_indea,
  zfiwrt0001-lm_contri_uf to wl_input_0001-lm_contri_uf,
  zfiwrt0001-ge_remessa    to wl_input_0001-ge_remessa,
  sy-uname                 to wl_input_0001-usuario_ult_mod,
  sy-datum                 to wl_input_0001-dt_ult_mod,
  sy-uzeit                 to wl_input_0001-hr_ult_mod,
  zfiwrt0001-referencia    to wl_input_0001-referencia,
  zfiwrt0001-valida_cfop   to wl_input_0001-valida_cfop, "CS2023000264 - 09.06.2023 - #108893 - LV

  zfiwrt0001-dt_ini_val   to wl_input_0001-dt_ini_val, "150184 CS2024000781 Aprovações ZNFW - PSA
  zfiwrt0001-dt_fim_val   to wl_input_0001-dt_fim_val,
  zfiwrt0001-dep_resp     to wl_input_0001-dep_resp,
  zfiwrt0001-status_aprov to wl_input_0001-status_aprov.


  if flag_tab8 = 'X'.
    clear:flag_tab8.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "133290 CS2024000037 Liberar tabela de vinculação de NCM vs produto SAP - znfw0009 Lib. tab. vinc. de NCM vs prod. SAP  PBALVES
    if wl_input_0001-opr_blq = 'B'.
      clear: wl_input_0001-opr_blq.
      wl_input_0001-opr_blq = 'L'.
    endif.

  else.
** Passa icone de cadeado para tela de exibicao
    call function 'ZNFW_CHANG_STATU_ABAS_ZWRR0001'
      exporting
        i_operacao     = zfiwrt0001-operacao
        i_usuario      = sy-uname
        i_status       = c_b
      importing
        e_status_opr   = wl_status
      tables
        et_status_abas = tl_status_abas
      exceptions
        user_not_found = 1
        others         = 2.
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
    perform atualiza_status_abas tables tl_status_abas
using  wl_status.
  endif.

***> texto descritivo.
  if obg_descbox is not initial.
    call method obg_descbox->get_text_as_r3table
      importing
        table = tg_editor.

    loop at tg_editor into wg_editor.
      if sy-tabix eq 1.
        wl_input_0001-txt_compl = wg_editor-line.

      elseif sy-tabix ge 2.
        concatenate wl_input_0001-txt_compl  wg_editor-line into wl_input_0001-txt_compl separated by space.

      endif.
    endloop.
  endif.
***<
** zfiwrt0002
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0002 where operacao eq wl_input_0001-operacao.
  loop at tg_impo.
    move: sy-mandt            to tl_input_0002-mandt,
    zfiwrt0001-operacao to tl_input_0002-operacao,
    tg_impo-taxtyp      to tl_input_0002-taxtyp.

    append tl_input_0002.
    clear: tl_input_0002.
  endloop.

** zfiwrt0003
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0003 where operacao eq wl_input_0001-operacao.
  loop at tg_contab.
    move: sy-mandt             to tl_input_0003-mandt,
    zfiwrt0001-operacao  to tl_input_0003-operacao,
    tg_contab-bschl      to tl_input_0003-bschl,
    tg_contab-hkont      to tl_input_0003-hkont,
    tg_contab-taxtyp     to tl_input_0003-taxtyp,
    tg_contab-estorno    to tl_input_0003-estorno,
    tg_contab-newbw      to tl_input_0003-newbw,
    tg_contab-umskz      to tl_input_0003-umskz,
    sy-datum             to tl_input_0003-data_reg,
    sy-uzeit             to tl_input_0003-hora_reg,
    sy-uname             to tl_input_0003-usname_reg.

    tl_input_0003-buzei  = sy-tabix.

    append tl_input_0003.
    clear: tl_input_0003.
  endloop.

  loop at tg_contab_log.
    read table tg_contab with key bschl    =  tg_contab_log-bschl
    hkont    =  tg_contab_log-hkont
    taxtyp   =  tg_contab_log-taxtyp
    estorno  =  tg_contab_log-estorno
    newbw    =  tg_contab_log-newbw
    umskz    =  tg_contab_log-umskz.

    if sy-subrc ne 0.
      clear: tl_input_0003l.

      select max( id_log )
      into tl_input_0003l-id_log
      from zfiwrt0003l.

      if sy-subrc ne 0.
        tl_input_0003l-id_log = 1.
      else.
        add 1 to tl_input_0003l-id_log.
      endif.

      move-corresponding tg_contab_log to tl_input_0003l.
      tl_input_0003l-data_reg   = sy-datum.
      tl_input_0003l-hora_reg   = sy-uzeit.
      tl_input_0003l-usname_reg = sy-uname.
      append tl_input_0003l.
    endif.
  endloop.

  tg_contab_log[] = tg_contab[].

** zfiwrt0004
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0004 where operacao eq wl_input_0001-operacao.
  loop at tg_movest.
    move: sy-mandt             to  tl_input_0004-mandt,
    zfiwrt0001-operacao  to  tl_input_0004-operacao,
    tg_movest-bwart      to  tl_input_0004-bwart,
    tg_movest-tcode      to  tl_input_0004-tcode,
    tg_movest-mwskz1     to  tl_input_0004-mwskz1,
    tg_movest-estorno    to  tl_input_0004-estorno.

    append tl_input_0004.
    clear: tl_input_0004.
  endloop.

** zfiwrt0005
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0005 where operacao eq wl_input_0001-operacao.
  loop at tg_mensagems_aux.
    move: sy-mandt                 to tl_input_0005-mandt,
    zfiwrt0001-operacao      to tl_input_0005-operacao,
    tg_mensagems_aux-seqnum  to tl_input_0005-seqnum,
    tg_mensagems_aux-linnum  to tl_input_0005-linnum,
    tg_mensagems_aux-message to tl_input_0005-message.

    append tl_input_0005.
    clear: tl_input_0005.
  endloop.

** zfiwrt0006
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0006 where operacao eq wl_input_0001-operacao.
***> Dentro do estado
  move: sy-mandt                 to tl_input_0006-mandt,
  zfiwrt0001-operacao      to tl_input_0006-operacao,
  c_d                      to tl_input_0006-indcoper,
  wg_dentro-cfop           to tl_input_0006-cfop,
  wg_dentro-taxlw1         to tl_input_0006-taxlw1,
  wg_dentro-taxlw2         to tl_input_0006-taxlw2,
  wg_dentro-taxlw4         to tl_input_0006-taxlw4,
  wg_dentro-taxlw5         to tl_input_0006-taxlw5,
  wg_dentro-taxlw3         to tl_input_0006-taxlw3,
  wg_dentro-taxcode        to tl_input_0006-taxcode.

  if oprd_trib eq c_x.
    move: c_t to tl_input_0006-opertyp.
  elseif oprd_isent eq c_x.
    move: c_i to tl_input_0006-opertyp.
  elseif oprd_ntrib eq c_x.
    move: c_n to tl_input_0006-opertyp.
  endif.
  append tl_input_0006.
  clear: tl_input_0006.

***> Fora do estado
  move: sy-mandt                 to tl_input_0006-mandt,
  zfiwrt0001-operacao      to tl_input_0006-operacao,
  c_f                      to tl_input_0006-indcoper,
  wg_fora-cfop             to tl_input_0006-cfop,
  wg_fora-taxlw1           to tl_input_0006-taxlw1,
  wg_fora-taxlw2           to tl_input_0006-taxlw2,
  wg_fora-taxlw4           to tl_input_0006-taxlw4,
  wg_fora-taxlw5           to tl_input_0006-taxlw5,
  wg_fora-taxlw3           to tl_input_0006-taxlw3,
  wg_fora-taxcode          to tl_input_0006-taxcode.

  if oprf_trib eq c_x.
    move: c_t to tl_input_0006-opertyp.
  elseif oprf_isent eq c_x.
    move: c_i to tl_input_0006-opertyp.
  elseif oprf_ntrib eq c_x.
    move: c_n to tl_input_0006-opertyp.
  endif.
  append tl_input_0006.
  clear: tl_input_0006.

** zfiwrt0028
**  elimina dados da tabela antes de atualizar as informacoes.
  delete from zfiwrt0028 where operacao eq wl_input_0001-operacao.
  loop at tg_cfop.
    move: sy-mandt                 to tl_input_0007-mandt,
    zfiwrt0001-operacao      to tl_input_0007-operacao,
    tg_cfop-uf    to tl_input_0007-uf,
    tg_cfop-cfop  to tl_input_0007-cfop.

    append tl_input_0007.
    clear: tl_input_0007.
  endloop.

  modify zfiwrt0001  from wl_input_0001.
  modify zfiwrt0002  from table tl_input_0002.
  modify zfiwrt0003  from table tl_input_0003.
  modify zfiwrt0003l from table tl_input_0003l. "Log.
  modify zfiwrt0004  from table tl_input_0004.
  modify zfiwrt0005  from table tl_input_0005.
  modify zfiwrt0006  from table tl_input_0006.
*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  modify zfiwrt0028  from table tl_input_0007.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

  message s836(sd) with 'Código de operação'
  wl_input_0001-operacao
  ', criado/modificado com sucesso!'.

*  CALL FUNCTION 'ZNFW_CHECK_STATU_ABAS_OPR'
*    EXPORTING
*      i_operacao       = zfiwrt0001-operacao
*    IMPORTING
*      e_status_opr     = wl_status
*    TABLES
*      et_status_abas   = tl_status_abas
*    EXCEPTIONS
*      status_not_found = 1.


  clear: wg_flag.
  wg_acao = c_atuali.
  leave to screen 100.

endform.                    " CRIA_DOCUMENTO
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos_100 output.
**  IF OBG_GRID1 IS INITIAL.
**    WG_LAYOUT-CWIDTH_OPT = C_X.
**    WG_LAYOUT-ZEBRA      = C_X.
***    wg_layout-no_toolbar = c_x.
**    WG_LAYOUT-COL_OPT    = C_X.
**    WG_STABLE-ROW        = C_X.
**
*** create the docking container
**    CREATE OBJECT OBG_DOCKING
**      EXPORTING
**        SIDE      = OBG_DOCKING->DOCK_AT_BOTTOM
**        EXTENSION = 100.
**
**    CALL METHOD OBG_DOCKING->SET_VISIBLE
**      EXPORTING
**        VISIBLE = ' '.
**
**    CREATE OBJECT OBG_GRID1
**      EXPORTING
**        I_PARENT = OBG_DOCKING.
**
**    PERFORM MONTAR_LAYOUT.
**
**    IF OBG_GRID1 IS NOT INITIAL.
***      CREATE alv event handler
**      CREATE OBJECT OBG_TOOLBAR
**        EXPORTING
**          IO_ALV_GRID = OBG_GRID1.
**
***      * Register event handler
**      SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR OBG_GRID1.
**      SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR OBG_GRID1.
**
**      CALL METHOD OBG_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
**        EXPORTING
**          IS_LAYOUT       = WG_LAYOUT
**        CHANGING
**          IT_FIELDCATALOG = TG_FIELDCATALOG[]
**          IT_OUTTAB       = TG_MSG_RET.
**
**
**
***      SET HANDLER lcl_event_receiver=>handle_user_command
***                  lcl_event_receiver=>handle_menu_button
***                  lcl_event_receiver=>handle_toolbar.
***    SET HANDLER:
***                 LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR OBG_GRID1.
**
*** Método de atualização de dados na Tela
**      CALL METHOD OBG_GRID1->REFRESH_TABLE_DISPLAY
**        EXPORTING
**          IS_STABLE = WG_STABLE.
**
**    ENDIF.
**
*** Método de atualização de dados na Tela
**    CALL METHOD OBG_GRID1->REFRESH_TABLE_DISPLAY
**      EXPORTING
**        IS_STABLE = WG_STABLE.
**
**
**  ENDIF.
endmodule.                 " CRIA_OBJETOS_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout .
  refresh: tg_fieldcatalog.

  perform montar_estrutura using:
        1  ' ' ' '    'TG_MSG_RET'   'MSG'      'Mensagem de Erro'  ' '.

endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------
form montar_estrutura using value(p_col_pos)       type i
      value(p_ref_tabname)   like dd02d-tabname
      value(p_ref_fieldname) like dd03d-fieldname
      value(p_tabname)       like dd02d-tabname
      value(p_field)         like dd03d-fieldname
      value(p_scrtext_l)     like dd03p-scrtext_l
      value(p_outputlen)     type i.



  clear: wg_fieldcatalog.

  wg_fieldcatalog-fieldname     = p_field.
  wg_fieldcatalog-tabname       = p_tabname.
  wg_fieldcatalog-ref_table     = p_ref_tabname.
  wg_fieldcatalog-ref_field     = p_ref_fieldname.
  wg_fieldcatalog-key           = ' '.
  wg_fieldcatalog-key_sel       = 'X'.
  wg_fieldcatalog-col_pos       = p_col_pos.
  wg_fieldcatalog-no_out        = ' '.
  wg_fieldcatalog-scrtext_l     = p_scrtext_l.
  wg_fieldcatalog-scrtext_m     = p_scrtext_l.
  wg_fieldcatalog-scrtext_s     = p_scrtext_l.
  wg_fieldcatalog-outputlen     = p_outputlen.



  if wg_fieldcatalog-scrtext_l ne space.

    wg_fieldcatalog-selddictxt       = 'L'.

  endif.

  append wg_fieldcatalog to tg_fieldcatalog.

endform.                    " montar_estrutura

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TC_DIREITOS'
constants: begin of c_tc_direitos,
             tab1 like sy-ucomm value 'TC_DIREITOS_FC1',
             tab2 like sy-ucomm value 'TC_DIREITOS_FC2',
           end of c_tc_direitos.
*&SPWIZARD: DATA FOR TABSTRIP 'TC_DIREITOS'
controls:  tc_direitos type tabstrip.
data: begin of g_tc_direitos,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'ZWRR0001',
        pressed_tab like sy-ucomm value c_tc_direitos-tab1,
      end of g_tc_direitos.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TC_DIREITOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
module tc_direitos_active_tab_set output.
  tc_direitos-activetab = g_tc_direitos-pressed_tab.
  case g_tc_direitos-pressed_tab.
    when c_tc_direitos-tab1.
      g_tc_direitos-subscreen = '0206'.
    when c_tc_direitos-tab2.
      g_tc_direitos-subscreen = '0207'.
    when others.
*&SPWIZARD:      DO NOTHING
  endcase.
endmodule.                    "TC_DIREITOS_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TC_DIREITOS'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
module tc_direitos_active_tab_get input.
  ok_code = sy-ucomm.
  case ok_code.
    when c_tc_direitos-tab1.
      g_tc_direitos-pressed_tab = c_tc_direitos-tab1.
    when c_tc_direitos-tab2.
      g_tc_direitos-pressed_tab = c_tc_direitos-tab2.
    when others.
*&SPWIZARD:      DO NOTHING
  endcase.
endmodule.                    "TC_DIREITOS_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_ESTONO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module mark_estono input.
  modify tg_contab
  index tc_contab-current_line.
endmodule.                 " MARK_ESTONO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_oper input.
  data: tl_return_tab type table of ddshretval with header line,
        tl_dselc      type table of dselc      with header line.
  data: begin of t_fieldtab occurs 3.
          include structure dynpread.
  data: end of t_fieldtab.

  data: begin of tl_operacao occurs 0,
          operacao  type zfiwrt0001-operacao,
          descricao type zfiwrt0001-descricao,
          cfop      type zfiwrt0006-cfop,
          indcoper  type zfiwrt0006-indcoper,
          bwart     type z_bwart_text, "US #164015 - MMSILVA - 17.02.2025
        end of tl_operacao.

  refresh: tl_operacao, t_fieldtab.
  clear:   tl_operacao, t_fieldtab.

  select a~operacao, a~descricao, b~cfop,  b~indcoper, c~bwart"OPERACAO DESCRICAO
  from zfiwrt0001 as a
  left join zfiwrt0006 as b on b~operacao eq a~operacao
  left join zfiwrt0004 as c on c~operacao eq a~operacao "US #164015 - MMSILVA - 17.02.2025
  into corresponding fields of table @tl_operacao where a~operacao ne @space.
  sort tl_operacao by operacao.

  tl_dselc-fldname    = 'DESCRICAO'.
  tl_dselc-dyfldname = 'TL_OPERACAO-DESCRICAO'.
  append tl_dselc.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'OPERACAO'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZFIWRT0001-OPERACAO'
      value_org       = 'S'
    tables
      value_tab       = tl_operacao
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

*  READ TABLE tl_return_tab INDEX 1.
*  READ TABLE tl_operacao
*    WITH KEY  operacao = tl_return_tab-fieldval.
*  IF sy-subrc IS INITIAL.
*    MOVE tl_operacao-descricao TO zfiwrt0001-descricao.
**    LEAVE TO SCREEN 100.
*  ENDIF.
endmodule.                 " SEARCH_OPER  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_OPERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_operacao .
  data: wl_0001        type zfiwrt0001,
        tl_0002        type table of zfiwrt0002  with header line,
        tl_0003        type table of zfiwrt0003  with header line,
        tl_0003l       type table of zfiwrt0003  with header line,
        tl_0004        type table of zfiwrt0004  with header line,
        tl_0005        type table of zfiwrt0005  with header line,
        tl_0006        type table of zfiwrt0006  with header line,
        tl_0007        type table of zfiwrt0028  with header line,
        tl_0008        type table of zfiwrt0032  with header line,
        tl_1baj        type table of j_1baj      with header line,
        tl_1bajt       type table of j_1bajt    with header line,
        tl_status_abas type table of zfiwrt0017 with header line,
        wl_cont        type sy-tabix,
        wl_cont_aux    type sy-tabix,
        wl_cont_aux2   type sy-tabix,
        wl_status      type zfiwrt0017-aba_blq..

  refresh: tl_0002, tl_0003, tl_0004, tl_0005, tl_0006, tl_0007, tl_0008,tl_1baj, tl_1bajt.
  clear: wl_0001, tl_0002, tl_0003, tl_0004, tl_0005, tl_0006, tl_0007,tl_0008, tl_1baj, tl_1bajt,
  wl_status.

  if zfiwrt0001-operacao is not initial.
    select single *
    from zfiwrt0001
    into wl_0001
    where operacao eq zfiwrt0001-operacao.

    if sy-subrc is initial.
      select *
      from zfiwrt0002
      into table tl_0002
      where operacao eq wl_0001-operacao.

      if sy-subrc is initial.
        select *
        from j_1baj
        into table tl_1baj
        for all entries in tl_0002
        where taxtyp eq tl_0002-taxtyp.

        select *
        from j_1bajt
        into table tl_1bajt
        for all entries in tl_0002
        where  spras  eq sy-langu
        and  taxtyp eq tl_0002-taxtyp.

      endif.
      select *
      from zfiwrt0003
      into table tl_0003
      where operacao eq wl_0001-operacao.

      select *
      from zfiwrt0004
      into table tl_0004
      where operacao eq wl_0001-operacao.

      select *
      from zfiwrt0005
      into table tl_0005
      where operacao eq wl_0001-operacao.

      select *
      from zfiwrt0006
      into table tl_0006
      where operacao eq wl_0001-operacao.

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
      select *
      from zfiwrt0028
      into  table tl_0007
      where operacao eq wl_0001-operacao.

      select distinct *
            from zfiwrt0032
        into  table tl_0008
            where operacao eq wl_0001-operacao.



    else.
      message s836(sd) display like 'E' with 'Cód. de operação não existe!'.
      leave to screen 100.
    endif.
  else.
    message s836(sd) display like 'E' with 'Cód. de operação não existe!'.
    leave to screen 100.
  endif.


** Preenche valores da tela.
  move: wl_0001-nftype         to zfiwrt0001-nftype,
  wl_0001-itmtyp         to zfiwrt0001-itmtyp,
  wl_0001-descricao      to zfiwrt0001-descricao,
  wl_0001-dias           to zfiwrt0001-dias,
  wl_0001-parvw          to zfiwrt0001-parvw,
  wl_0001-retorno        to zfiwrt0001-retorno,
  wl_0001-zpesagem       to zfiwrt0001-zpesagem,
  wl_0001-imobilizado    to zfiwrt0001-imobilizado,
  wl_0001-tp_mv_imob     to zfiwrt0001-tp_mv_imob,
  wl_0001-ctrl_zrfl      to zfiwrt0001-ctrl_zrfl,
  wl_0001-energia        to zfiwrt0001-energia,
  wl_0001-servico        to zfiwrt0001-servico,
  wl_0001-disp_nf_cct    to zfiwrt0001-disp_nf_cct,
  wl_0001-transf_icms    to zfiwrt0001-transf_icms,
  wl_0001-complemento    to zfiwrt0001-complemento,
  wl_0001-complement_icms to zfiwrt0001-complement_icms,  "*-CS2023000043-09.02.2023-#102019-JT
  wl_0001-aviso_rec      to zfiwrt0001-aviso_rec,
  wl_0001-lm_estoque     to zfiwrt0001-lm_estoque,
  wl_0001-lm_aprova      to zfiwrt0001-lm_aprova,
  wl_0001-lm_indea       to zfiwrt0001-lm_indea,
  wl_0001-ge_remessa     to zfiwrt0001-ge_remessa,
  wl_0001-referencia     to zfiwrt0001-referencia,
  wl_0001-lm_contri_uf  to zfiwrt0001-lm_contri_uf,

  wl_0001-dt_ini_val  to zfiwrt0001-dt_ini_val, "150184 CS2024000781 Aprovações ZNFW - PSA
  wl_0001-dt_fim_val  to zfiwrt0001-dt_fim_val,
  wl_0001-dep_resp  to zfiwrt0001-dep_resp,
  wl_0001-status_aprov  to zfiwrt0001-status_aprov,

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  wl_0001-valida_cfop  to zfiwrt0001-valida_cfop.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

***      descricao da operacao.
  refresh: tg_editor.
  clear: wl_cont_aux2, wl_cont_aux, wl_cont.
  wl_cont = strlen( wl_0001-txt_compl ).
  wl_cont_aux = wl_cont / 72.

  do.
    move: wl_0001-txt_compl+wl_cont_aux2 to wg_editor-line.
    add 72 to wl_cont_aux2.
    append wg_editor to tg_editor.

    if wl_cont_aux2 gt wl_cont.
      exit.

    endif.
  enddo.
  call method obg_descbox->set_text_as_r3table
    exporting
      table = tg_editor.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*  IF wl_0001-opr_blq EQ c_b.
*    wg_blq =  '@06@'.
*  ELSEIF wl_0001-opr_blq EQ c_l.
*    wg_blq =  '@07@'.
*  ELSE.
*    wg_blq = '@09@'.
*  ENDIF.

  call function 'ZNFW_CHECK_STATU_ABAS_OPR'
    exporting
      i_operacao       = zfiwrt0001-operacao
    importing
      e_status_opr     = wl_status
    tables
      et_status_abas   = tl_status_abas
    exceptions
      status_not_found = 1.

  perform atualiza_status_abas tables tl_status_abas
  using  wl_status.


** impostos
  loop at tl_0002.
    read table tl_1baj
    with key taxtyp = tl_0002-taxtyp.

    read table tl_1bajt
    with key taxtyp = tl_0002-taxtyp.

    move: tl_0002-taxtyp   to tg_impo-taxtyp,
    tl_1bajt-ttypetxt to tg_impo-ttypetxt,
    tl_1baj-taxgrp  to tg_impo-taxgrp.

    append tg_impo.
    clear: tg_impo.
  endloop.


  loop at tl_0003.
    move: tl_0003-operacao    to tg_contab-operacao,
    tl_0003-buzei       to tg_contab-buzei,
    tl_0003-bschl       to tg_contab-bschl,
    tl_0003-hkont       to tg_contab-hkont,
    tl_0003-taxtyp      to tg_contab-taxtyp,
    tl_0003-estorno     to tg_contab-estorno,
    tl_0003-umskz       to tg_contab-umskz,
    tl_0003-newbw       to tg_contab-newbw,
    tl_0003-data_reg    to tg_contab-data_reg,
    tl_0003-hora_reg    to tg_contab-hora_reg,
    tl_0003-usname_reg  to tg_contab-usname_reg.

    append tg_contab.
    clear: tg_contab.
  endloop.

  tg_contab_log[] = tg_contab[].

  sort tg_contab by data_reg hora_reg.

  loop at tl_0004.
    move: tl_0004-bwart  to tg_movest-bwart,
    tl_0004-tcode  to tg_movest-tcode,
    tl_0004-mwskz1 to tg_movest-mwskz1,
    tl_0004-estorno to tg_movest-estorno.

    append tg_movest.
    clear: tg_movest.
  endloop.

  clear: tg_mensagems_aux, tg_mensagems.
  loop at tl_0005.
    if tl_0005-linnum eq 1.
      tg_mensagems-seqnum     = tl_0005-seqnum.
      tg_mensagems-message    = tl_0005-message.

      append tg_mensagems.

    endif.

    tg_mensagems_aux-seqnum = tl_0005-seqnum.
    tg_mensagems_aux-linnum = tl_0005-linnum.
    tg_mensagems_aux-message = tl_0005-message.

    append tg_mensagems_aux.

  endloop.

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  clear: tl_0007.
  loop at tl_0007.
    move-corresponding tl_0007 to tg_cfop.
    append tg_cfop.
    clear tg_cfop.
  endloop.
*Fim Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893

  clear: tl_0008.
  loop at tl_0008.
    move-corresponding tl_0008 to tg_zfiwrt0032.
    append tg_zfiwrt0032.
    clear tg_zfiwrt0032.
  endloop.

  loop at tl_0006.
    if tl_0006-indcoper eq c_d.
      move: tl_0006-cfop   to wg_dentro-cfop,
      tl_0006-taxlw1 to wg_dentro-taxlw1,
      tl_0006-taxlw2 to wg_dentro-taxlw2,
      tl_0006-taxlw4 to wg_dentro-taxlw4,
      tl_0006-taxlw5 to wg_dentro-taxlw5,
      tl_0006-taxlw3 to wg_dentro-taxlw3,
      tl_0006-taxcode to wg_dentro-taxcode.

      case tl_0006-opertyp.
        when c_t.
          oprd_trib  = c_x.

        when c_i.
          oprd_isent = c_x.

        when c_n.
          oprd_ntrib = c_x.

      endcase.


    elseif tl_0006-indcoper eq c_f.
      move: tl_0006-cfop  to wg_fora-cfop,
      tl_0006-taxlw1 to wg_fora-taxlw1,
      tl_0006-taxlw2 to wg_fora-taxlw2,
      tl_0006-taxlw4 to wg_fora-taxlw4,
      tl_0006-taxlw5 to wg_fora-taxlw5,
      tl_0006-taxlw3 to wg_fora-taxlw3,
      tl_0006-taxcode to wg_fora-taxcode.

      case tl_0006-opertyp.
        when c_t.
          oprf_trib  = c_x.

        when c_i.
          oprf_isent = c_x.

        when c_n.
          oprf_ntrib = c_x.

      endcase.
    endif.
  endloop.

*-CS2023000043-09.02.2023-#102019-JT-inicio
  move oprd_trib                to wl_dent_old-oprd_trib.
  move oprd_isent               to wl_dent_old-oprd_isent.
  move oprd_ntrib               to wl_dent_old-oprd_ntrib.
  move-corresponding wg_dentro  to wl_dent_old.
*
  move oprf_trib                to wl_fora_old-oprf_trib.
  move oprf_isent               to wl_fora_old-oprf_isent.
  move oprf_ntrib               to wl_fora_old-oprf_ntrib.
  move-corresponding wg_fora    to wl_fora_old.
*-CS2023000043-09.02.2023-#102019-JT-fim

endform.                    " BUSCA_OPERACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form preenche_campos .

*  MOVE: wl_0001-nftype    TO zfiwrt0001-nftype,
*        wl_0001-itmtyp    TO zfiwrt0001-itmtyp.
****      descricao da operacao.
*
****
*  LOOP AT tl_0002.
*    READ TABLE tl_1baj
*      WITH KEY taxtyp = tl_0002-taxtyp.
*
*    READ TABLE tl_1bajt
*      WITH KEY taxtyp = tl_0002-taxtyp.
*
*    MOVE: tl_0002-taxtyp   TO tg_impo-taxtyp,
*          tl_1baj-ttypetxt TO tg_impo-ttypetxt,
*          tl_1bajt-taxgrp  TO tg_impo-taxgrp.
*
*    APPEND tg_impo.
*  ENDLOOP.

endform.                    " PREENCHE_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_campos .

  clear: zfiwrt0001-descricao,
  wg_dentro,
  wg_fora,
  oprd_trib,
  oprf_trib,
  oprd_isent,
  oprf_isent,
  oprd_ntrib,
  oprf_ntrib,
  zfiwrt0001-nftype,
  zfiwrt0001-itmtyp,
  zfiwrt0001-parvw,
  zfiwrt0001-dias,
  zfiwrt0001-retorno,
  zfiwrt0001-zpesagem,
  zfiwrt0001-imobilizado,
  wl_desc_nftype,
  wl_desc_itmtyp,
  wl_desc_parvw,
  wl_desc_den_cfop,
  wl_desc_den_taxlw1,
  wl_desc_den_taxlw2,
  wl_desc_den_taxlw4,
  wl_desc_den_taxlw5,
  wl_desc_den_taxcode,
  wl_desc_for_cfop,
  wl_desc_for_taxlw1,
  wl_desc_for_taxlw2,
  wl_desc_for_taxlw4,
  wl_desc_for_taxlw5,
  wl_desc_for_taxlw3,
  wl_desc_for_taxcode,
  wg_blq.

  tab_1 = 'Dados Gerais'.
  tab_2 = 'Texto Compl. Operação'.
  tab_3 = 'Impostos'.
  tab_4 = 'Mensagem Nota'.
  tab_5 = 'Contabilização'.
  tab_6 = 'Movimento Estoque'.
  tab_7 = 'CFOP'.
  tab_8 = 'NCM - ZNFW0009'.
**       campo descricao.

**
  refresh: tg_impo,
  tg_movest,
  tg_contab,
  tg_mensagems_aux,
  tg_mensagems,
  tg_editor,
  tg_cfop,
  tg_zfiwrt0032.

  call method obg_descbox->set_text_as_r3table
    exporting
      table = tg_editor.


endform.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modifica_status .
  data: wl_0001        type zfiwrt0001,
        wl_status      type zfiwrt0017-aba_blq,
        tl_status_abas type table of zfiwrt0017 with header line.

  if zfiwrt0001-operacao is not initial.
    select single *
    from zfiwrt0001
    into wl_0001
    where operacao eq zfiwrt0001-operacao.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    IF WL_0001-OPR_BLQ EQ C_B
*    OR WL_0001-OPR_BLQ IS INITIAL.
*      WL_0001-OPR_BLQ = C_L.
*      WG_BLQ = '@07@'.
*    ELSE.
*      WL_0001-OPR_BLQ = C_B.
*      WG_BLQ = '@06@'.
*    ENDIF.

    modify zfiwrt0001 from wl_0001.
    call function 'ZNFW_CHANG_STATU_ABAS_ZWRR0001'
      exporting
        i_operacao     = zfiwrt0001-operacao
        i_usuario      = sy-uname
      importing
        e_status_opr   = wl_status
      tables
        et_status_abas = tl_status_abas
      exceptions
        user_not_found = 1
        others         = 2.

    if sy-subrc is initial.
      wl_0001-opr_blq = wl_status.
      modify zfiwrt0001 from wl_0001.
      message s836(sd) with 'O status da operação foi modificado!'.

      perform atualiza_status_abas tables tl_status_abas
      using wl_status.
    elseif sy-subrc eq 1.
      message s836(sd) display like 'E' with 'O usuário não tem perfil para modificar'
      'o status da operação!'.
    elseif sy-subrc eq 2.
      message s836(sd) display like 'E' with 'O status da operação não foi modificado!'.
    endif.


    wg_acao = c_atuali.
    leave to screen 100.
  endif.
endform.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_STATUS_ABAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_STATUS_ABAS  text
*----------------------------------------------------------------------*
form atualiza_status_abas  tables   tl_status_abas structure zfiwrt0017
using    wl_status.
*  LOOP AT tl_status_abas.
*    CASE tl_status_abas-aba.
**        Dados Gerais
*      WHEN 'TAB_1'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_1'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_1 = '@06@ Dados Gerais'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_1 = '@07@ Dados Gerais'.
  endif.
**        Texto_compl._Operação
*      WHEN 'TAB_2'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_2'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_2 = '@06@ Texto Compl. Operação'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_2 = '@07@ Texto Compl. Operação'.
  endif.
**        Impostos
*      WHEN 'TAB_3'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_3'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_3 = '@06@ Impostos'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_3 = '@07@ Impostos'.
  endif.
**        Mensagem_Nota
*      WHEN 'TAB_4'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_4'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_4 = '@06@ Mensagem Nota'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_4 = '@07@ Mensagem Nota'.
  endif.
**        Contabilização
*      WHEN 'TAB_5'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_5'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_5 = '@06@ Contabilização'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_5 = '@07@ Contabilização'.
  endif.
**        Movimento_Estoque
*      WHEN 'TAB_6'.
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_6'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_6 = '@06@ Movimento Estoque'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_6 = '@07@ Movimento Estoque'.
  endif.
*    ENDCASE.
*  ENDLOOP.

*Inicio Alteração - Leandro Valentim Ferreira - 13.06.23 - #108893
  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_7'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_7 = '@06@ CFOP'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_7 = '@07@ CFOP'.
  endif.

  clear: tl_status_abas.
  read table tl_status_abas
  with key aba = 'TAB_8'.

  if tl_status_abas-aba_blq eq c_b
  or tl_status_abas-aba_blq is initial.
    tab_8 = '@06@ NCM - ZNFW0009'.
  elseif tl_status_abas-aba_blq eq c_l.
    tab_8 = '@07@ NCM - ZNFW0009'.
  endif.

  if wl_status = c_l.
    wg_blq = '@07@'.
  elseif wl_status = c_b.
    wg_blq = '@06@'.

  endif.



endform.                    " ATUALIZA_STATUS_ABAS
*&---------------------------------------------------------------------*
*&      Module  SEL_TMV_IMOB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module sel_tmv_imob input.
  if zfiwrt0001-imobilizado = 'S'.
    wg_tmv = 'S'.
  else.
    wg_tmv = 'N'.
  endif.
endmodule.                 " SEL_TMV_IMOB  INPUT
*&---------------------------------------------------------------------*
*&      Form  VIEW_LOG_CONTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form view_log_contab .

  data: it_log_0003l type table of zfiwrt0003l.

  select *
  from zfiwrt0003l
  into table it_log_0003l
  where operacao = zfiwrt0001-operacao.

  if it_log_0003l[] is not initial.

    sort it_log_0003l by data_reg hora_reg.

    perform montar_layout_log.
    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        it_fieldcat           = it_estrutura[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 115
        i_screen_end_line     = 13
      tables
        t_outtab              = it_log_0003l.
  endif.


endform.


form montar_layout_log.
  refresh: it_estrutura.

  perform estrutura_alv_log using:
        1  ''   ''            'IT_LOG_003' 'ID_LOG'     'Id.'         '10',
        1  ''   ''            'IT_LOG_003' 'BUZEI'      'Item'        '04',
        1  ''   ''            'IT_LOG_003' 'BSCHL'      'Chv.Lcto'    '08',
        1  ''   ''            'IT_LOG_003' 'HKONT'      'Cont.Razão'  '13',
        1  ''   ''            'IT_LOG_003' 'UMSKZ'      'Rz.Esp.'     '07',
        1  ''   ''            'IT_LOG_003' 'TAXTYP'     'Tp.Imp.'     '07',
        1  ''   ''            'IT_LOG_003' 'ESTORNO'    'Est.'        '04',
        1  ''   ''            'IT_LOG_003' 'NEWBW'      'Tp.Mov.'     '07',
        1  ''   ''            'IT_LOG_003' 'DATA_REG'   'Data'        '12',
        1  ''   ''            'IT_LOG_003' 'HORA_REG'   'Hora'        '12',
        1  ''   ''            'IT_LOG_003' 'USNAME_REG' 'Usuário'     '12'.


endform.                    " MONTAR_LAYOUT

form estrutura_alv_log using value(p_col_pos)       type i
      value(p_ref_tabname)   like dd02d-tabname
      value(p_ref_fieldname) like dd03d-fieldname
      value(p_tabname)       like dd02d-tabname
      value(p_field)         like dd03d-fieldname
      value(p_scrtext_l)     like dd03p-scrtext_l
      value(p_outputlen).

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  if p_outputlen is initial.
    wa_estrutura-outputlen     = x_contador.
  else.
    wa_estrutura-outputlen     =  p_outputlen.
  endif.

  append wa_estrutura to it_estrutura.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  TC_TEXT_SD  INPUT
*&---------------------------------------------------------------------*
module tc_text_sd input.

*  SELECT SINGLE *
*    FROM zfiwrt0001
*    INTO @DATA(lwa_zfiwrt0001)
*    WHERE operacao = @zfiwrt0001-operacao.
*  IF sy-subrc IS INITIAL.
*
*  ENDIF.

  zfiwrt0001-texto_sd = gv_text_sd.

endmodule.

controls: tc_cfop type tableview using screen 0209.
data:     g_tc_cfop_lines  like sy-loopc.

module tc_cfop_change_tc_attr output.
  describe table tg_cfop lines tc_cfop-lines.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_CFOP_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_cfop_get_lines output.
  g_tc_cfop_lines = sy-loopc.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_CFOP_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_cfop_modify input.

  if tg_cfop-uf is not initial and tg_cfop-cfop is not initial.
    read table tg_cfop transporting no fields with key uf = tg_cfop-uf
    cfop = tg_cfop-cfop.
    if sy-subrc eq 0.
      message e000(zwrm001) with 'Já existe um registro com essa UF e CFOP '
      'na tabela'.
    endif.
  endif.

  if tg_cfop-uf is initial.
    message e000(zwrm001) with 'Favor, informar UF'.
  endif.

  if tg_cfop-cfop is initial.
    message e000(zwrm001) with 'Favor, informar CFOP'.
  endif.

  modify tg_cfop
  index tc_cfop-current_line.
  if sy-subrc is not initial.
    append tg_cfop.
  endif.
  clear: tg_cfop.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_CFOP_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_cfop_mark input.
  data: g_tc_cfop_wa2 like line of tg_cfop.
  if tc_cfop-line_sel_mode = 1
  and tg_cfop-mark = 'X'.
    loop at tg_cfop into g_tc_cfop_wa2
    where mark = 'X'.
      g_tc_cfop_wa2-mark = ''.
      modify tg_cfop
      from g_tc_cfop_wa2
      transporting mark.
    endloop.
  endif.
  modify tg_cfop
  index tc_cfop-current_line
  transporting mark.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_CFOP_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_cfop_user_command input.
  ok_code = sy-ucomm.
  perform user_ok_tc using    'TC_CFOP'
        'TG_CFOP'
        'MARK'
  changing ok_code.
  sy-ucomm = ok_code.
endmodule.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC_zfiwrt0032' ITSELF
controls: tc_zfiwrt0032 type tableview using screen 0210.

*&SPWIZARD: LINES OF TABLECONTROL 'TC_CONTAB'
data:     g_tc_zfiwrt0032_lines  like sy-loopc.

module tc_zfiwrt0032_change_tc_attr output.
  describe table tg_zfiwrt0032 lines tc_zfiwrt0032-lines.
endmodule.
*&---------------------------------------------------------------------*
*& Module TC_ZFIWRT0032_GET_LINES OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module tc_zfiwrt0032_get_lines output.
  g_tc_zfiwrt0032_lines = sy-loopc.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  TC_ZFIWRT0032_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_zfiwrt0032_modify input.

  read table tg_zfiwrt0032 with key ncm = tg_zfiwrt0032-ncm.
  if sy-subrc ne 0.
    "append tg_zfiwrt0032.
    modify tg_zfiwrt0032 index tc_zfiwrt0032-current_line.
  else.
    if sy-tabix ne tc_zfiwrt0032-current_line.
      message 'NCM duplicado' type 'I'.
      delete tg_zfiwrt0032 index tc_zfiwrt0032-current_line.
    endif.
  endif.

  clear: tg_zfiwrt0032.


endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_ZFIWRT0032_MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_zfiwrt0032_mark input.

  data: g_tc_zfiwrt0032_wa2 like line of tg_zfiwrt0032.
  if tc_zfiwrt0032-line_sel_mode = 1
  and tg_zfiwrt0032-mark = 'X'.
    loop at tg_zfiwrt0032 into g_tc_zfiwrt0032_wa2
    where mark = 'X'.
      g_tc_zfiwrt0032_wa2-mark = ''.
      modify tg_zfiwrt0032
      from g_tc_zfiwrt0032_wa2
      transporting mark.
    endloop.
  endif.
  modify tg_zfiwrt0032
  index tc_zfiwrt0032-current_line
  transporting mark.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  TC_ZFIWRT0032_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_zfiwrt0032_user_command input.
  ok_code = sy-ucomm.

  if ok_code = 'TC_ZFIWRT0032_INSR'.
    perform user_ok_tc using    'TC_ZFIWRT0032'
          'TG_ZFIWRT0032'
          'MARK'
    changing ok_code.
  elseif ok_code = 'TC_ZFIWRT0032_DELE'.

    loop at tg_zfiwrt0032 assigning field-symbol(<del>).
      if <del>-mark = 'X'.
        delete from zfiwrt0032 where operacao = zfiwrt0001-operacao and ncm = <del>-ncm and material = <del>-material.
      endif.
      delete from zfiwrt0032 where ncm = '' and material = ''.
    endloop.
    perform user_ok_tc using    'TC_ZFIWRT0032'
          'TG_ZFIWRT0032'
          'MARK'
    changing ok_code.
    "PERFORM atualiza_zfiwrt0032.

  elseif ok_code = 'SAVE'.
    clear: flag_tab8.
    flag_tab8 = abap_true.

    loop at tg_zfiwrt0032 assigning field-symbol(<delnull>) where ncm = '' and material = ''.
      delete tg_zfiwrt0032 where ncm = '' and material = ''.
    endloop.

    loop at tg_zfiwrt0032 assigning field-symbol(<save>).
      modify zfiwrt0032 from @(
                                value #(
                                mandt = sy-mandt
                                operacao = zfiwrt0001-operacao
                                ncm = <save>-ncm
                                material = <save>-material
                                deposito = <save>-deposito
                                lote = <save>-lote
                                lote_aut = <save>-lote_aut
*                                data = sy-datum
*                                    hora = sy-uzeit
*                                    usuario = sy-uname
    )
    ).
    endloop.
    "PERFORM atualiza_zfiwrt0032.
  elseif ok_code = 'SEARCH'.

  elseif ok_code =  'F_ATUAL'.
    wg_acao = c_atuali.
    perform limpa_campos.
    perform busca_operacao.
    perform busca_descricoes.
  elseif ok_code = 'F_NCM'.

    sort tg_zfiwrt0032 by ncm ascending.

    delete tg_zfiwrt0032 where ncm <> filter_ncm.

  elseif ok_code = 'CANCEL'.
    clear: tg_zfiwrt0032,ok_code.
  elseif ok_code = 'ATUALI'.
    if zfiwrt0001-operacao is not initial and zfiwrt0001-descricao is not initial.
      "PERFORM atualiza_zfiwrt0032.
    endif.
  elseif ok_code = 'TAB_STRIP_NF_FC8'.
    perform user_ok_tc using    'TC_ZFIWRT0032'
    'TG_ZFIWRT0032'
    'MARK'
changing ok_code.
  endif.

  sy-ucomm = ok_code.

endmodule.

module dados_zfiwrt_0032 output.

  if ok_code = 'TAB_STRIP_NF_FC8' and zfiwrt0001-operacao is not initial and zfiwrt0001-descricao is not initial.

  endif.

endmodule.

form verifica_permissoes_aba.

  perform get_relation_fields.

  select distinct aba from zfiwrt0016
    into table @data(it_aba)
where usnam = @sy-uname.

  if it_aba is not initial.

    sort it_aba by aba ascending.
    clear:it_aba_lista.
    move-corresponding it_aba to it_aba_lista.
    clear: it_aba.

    loop at it_aba_lista assigning field-symbol(<add_descricao>).
      loop at it_list_fields[] assigning field-symbol(<add>) where aba = <add_descricao>-aba.
        if <add> is not initial.
          <add>-permissao = abap_true.
        endif.
      endloop.
    endloop.

  else.

  endif.

endform.


form get_relation_fields.

  clear: it_list_fields, it_list_fields[].

  it_list_fields-tela = '0201'.
  it_list_fields-desc = 'Dados Gerais'.
  it_list_fields-aba = 'TAB_1'.
  append it_list_fields.

  it_list_fields-tela = '0206'.
  it_list_fields-desc = 'Dados Gerais'.
  it_list_fields-aba = 'TAB_1'.
  append it_list_fields.

  it_list_fields-tela = '0207'.
  it_list_fields-desc = 'Dados Gerais'.
  it_list_fields-aba = 'TAB_1'.
  append it_list_fields.

  it_list_fields-tela = '0208'.
  it_list_fields-desc = 'Texto Compl. Operação'.
  it_list_fields-aba = 'TAB_2'.
  append it_list_fields.

  it_list_fields-tela = '0202'.
  it_list_fields-desc = 'Impostos'.
  it_list_fields-aba = 'TAB_3'.
  append it_list_fields.

  it_list_fields-tela = '0203'.
  it_list_fields-desc = 'Mensagem Nota'.
  it_list_fields-aba = 'TAB_4'.
  append it_list_fields.

  it_list_fields-tela = '0204'.
  it_list_fields-desc = 'Contabilização'.
  it_list_fields-aba = 'TAB_5'.
  append it_list_fields.

  it_list_fields-tela = '0205'.
  it_list_fields-desc = 'Movimento Estoque'.
  it_list_fields-aba = 'TAB_6'.
  append it_list_fields.

  it_list_fields-tela = '0209'.
  it_list_fields-desc = 'CFOP'.
  it_list_fields-aba = 'TAB_7'.
  append it_list_fields.

  it_list_fields-tela = '0210'.
  it_list_fields-desc = 'NCM - ZNFW0009'.
  it_list_fields-aba = 'TAB_8'.
  append it_list_fields.
endform.

form desabilitar_fields.

  loop at screen.
    if screen-group1 eq 'A1'
    or screen-group1 eq 'MOD'.
      screen-input     = 0. "tg_fields-value.
*        screen-invisible = tg_fields-invisible.
      modify screen.
*        EXIT.
    endif.
  endloop.

endform.

form trata_acao_editar.

  perform verifica_permissoes_aba. "PSA

  clear:wa_tela.
  read table it_list_fields[] into wa_tela with key tela = sy-dynnr permissao = abap_true.

  if wg_acao eq c_modif.
    case sy-dynnr.
      when '0100'.
        loop at screen.
          if screen-name = 'ZFIWRT0001-DESCRICAO'.
            screen-input = 1.
            modify screen.
            exit.
          endif.
        endloop.
      when '0201'.
        if wa_tela-tela = '0201'.
          loop at screen.
            if screen-name = 'ZFIWRT0001-LM_ESTOQUE'
            or screen-name = 'ZFIWRT0001-REFERENCIA'
            or screen-name = 'ZFIWRT0001-GE_REMESSA'
            or screen-name = 'ZFIWRT0001-LM_INDEA'
            or screen-name = 'ZFIWRT0001-LM_APROVA'
            or screen-name = 'ZFIWRT0001-LM_CONTRI_UF'
            or screen-name = 'ZFIWRT0001-COMPLEMENT_ICMS'
            or screen-name = 'ZFIWRT0001-DISP_NF_CCT'
            or screen-name = 'ZFIWRT0001-ZPESAGEM'
            or screen-name = 'ZFIWRT0001-SERVICO'
            or screen-name = 'ZFIWRT0001-IMOBILIZADO'
            or screen-name = 'ZFIWRT0001-TP_MV_IMOB'
            or screen-name = 'ZFIWRT0001-COMPLEMENTO'
            or screen-name = 'ZFIWRT0001-CTRL_ZRFL'
            or screen-name = 'ZFIWRT0001-TRANSF_ICMS'
            or screen-name = 'ZFIWRT0001-ENERGIA'
            or screen-name = 'ZFIWRT0001-AVISO_REC'
            or screen-name = 'ZFIWRT0001-NFTYPE'
            or screen-name = 'ZFIWRT0001-ITMTYP'
            or screen-name = 'ZFIWRT0001-PARVW'
            or screen-name = 'ZFIWRT0001-DIAS'
            or screen-name = 'ZFIWRT0001-VALIDA_CFOP'
            or screen-name = 'ZFIWRT0001-RETORNO'

              or screen-name = 'ZFIWRT0001-DT_INI_VAL' "150184 CS2024000781 Aprovações ZNFW - PSA
              or screen-name = 'ZFIWRT0001-DT_FIM_VAL' "150184 CS2024000781 Aprovações ZNFW - PSA
              or screen-name = 'ZFIWRT0001-DEP_RESP' "150184 CS2024000781 Aprovações ZNFW - PSA

              or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name+0(10) = 'ZFIWRT0001'
              or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0202'.
        if wa_tela-tela = '0202'.
          loop at screen.
            if screen-name = 'TG_IMPO-TAXTYP'
            or screen-name = 'TG_IMPO-TTYPETXT'
            or screen-name = 'TG_IMPO-TAXGRP'
            or screen-name = 'TC_IMPOSTOS_INSERT'
            or screen-name = 'TC_IMPOSTOS_DELETE'
            or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'TG_IMPO-TAXTYP'
            or screen-name = 'TG_IMPO-TTYPETXT'
            or screen-name = 'TG_IMPO-TAXGRP'
            or screen-name = 'TC_IMPOSTOS_INSERT'
            or screen-name = 'TC_IMPOSTOS_DELETE'
            or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0203'.
        if wa_tela-tela = '0203'.
          loop at screen.
            if screen-name = 'TG_MENSAGEMS-MESSAGE'
            or screen-name = 'TXT_SD_MATERIAL'
              or screen-name = 'ADDMSG'
              or screen-name = 'GV_TEXT_SD'
            or screen-name = 'CC_TEXTBOX'
              or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name+0(12) = 'TG_MENSAGEMS'
            or screen-name = 'TXT_SD_MATERIAL'
                            or screen-name = 'ADDMSG'
              or screen-name = 'GV_TEXT_SD'
            or screen-name = 'CC_TEXTBOX'
              or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0204'.
        if wa_tela-tela = '0204'.
          loop at screen.
            if screen-name = 'TG_CONTAB-BSCHL'
              or screen-name = 'TG_CONTAB-HKONT'
              or screen-name = 'TG_CONTAB-UMSKZ'
              or screen-name = 'TG_CONTAB-TAXTYP'
              or screen-name = 'TG_CONTAB-NEWBW'
              or screen-name = 'TG_CONTAB-ESTORNO'
              or screen-name = 'TC_CONTAB_INSERT'
              or screen-name = 'TC_CONTAB_DELETE'
            or screen-name = 'BTN_LOG'
              or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'TG_CONTAB-BSCHL'
              or screen-name = 'TG_CONTAB-HKONT'
              or screen-name = 'TG_CONTAB-UMSKZ'
              or screen-name = 'TG_CONTAB-TAXTYP'
              or screen-name = 'TG_CONTAB-NEWBW'
              or screen-name = 'TG_CONTAB-ESTORNO'
              or screen-name = 'TC_CONTAB_INSERT'
              or screen-name = 'TC_CONTAB_DELETE'
            or screen-name = 'BTN_LOG'
              or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0205'.
        if wa_tela-tela = '0205'.
          loop at screen.
            if screen-name = 'TG_MOVEST-BWART'
            or screen-name = 'TG_MOVEST-TCODE'
            or screen-name = 'TG_MOVEST-MWSKZ1'
            or screen-name = 'TG_MOVEST-ESTORNO'
            or screen-name = 'TC_MOVEST_INSERT'
            or screen-name = 'TC_MOVEST_DELETE'
              or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'TG_MOVEST-BWART'
            or screen-name = 'TG_MOVEST-TCODE'
            or screen-name = 'TG_MOVEST-MWSKZ1'
            or screen-name = 'TG_MOVEST-ESTORNO'
            or screen-name = 'TC_MOVEST_INSERT'
            or screen-name = 'TC_MOVEST_DELETE'
            or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0206'.
        if wa_tela-tela = '0206'.
          loop at screen.
            if screen-name = 'OPRD_TRIB'
            or screen-name = 'OPRD_ISENT'
            or screen-name = 'OPRD_NTRIB'
            or screen-name = 'WG_DENTRO-CFOP'
            or screen-name = 'WG_DENTRO-TAXLW1'
            or screen-name = 'WG_DENTRO-TAXLW2'
            or screen-name = 'WG_DENTRO-TAXLW4'
            or screen-name = 'WG_DENTRO-TAXLW3'
            or screen-name = 'WG_DENTRO-TAXLW5'
            or screen-name = 'WG_DENTRO-TAXCODE'
            or screen-name = 'WL_DESC_DEN_CFOP'
            or screen-name = 'WL_DESC_DEN_TAXLW1'
            or screen-name = 'WL_DESC_DEN_TAXLW2'
            or screen-name = 'WL_DESC_DEN_TAXLW3'
            or screen-name = 'WL_DESC_DEN_TAXLW4'
            or screen-name = 'WL_DESC_DEN_TAXLW5'
            or screen-name = 'WL_DESC_DEN_TAXCODE'
                or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'OPRD_TRIB'
            or screen-name = 'OPRD_ISENT'
            or screen-name = 'OPRD_NTRIB'
            or screen-name = 'WG_DENTRO-CFOP'
            or screen-name = 'WG_DENTRO-TAXLW1'
            or screen-name = 'WG_DENTRO-TAXLW2'
            or screen-name = 'WG_DENTRO-TAXLW4'
            or screen-name = 'WG_DENTRO-TAXLW3'
            or screen-name = 'WG_DENTRO-TAXLW5'
            or screen-name = 'WG_DENTRO-TAXCODE'
            or screen-name = 'WL_DESC_DEN_CFOP'
            or screen-name = 'WL_DESC_DEN_TAXLW1'
            or screen-name = 'WL_DESC_DEN_TAXLW2'
            or screen-name = 'WL_DESC_DEN_TAXLW3'
            or screen-name = 'WL_DESC_DEN_TAXLW4'
            or screen-name = 'WL_DESC_DEN_TAXLW5'
            or screen-name = 'WL_DESC_DEN_TAXCODE'
                or screen-group1 = 'A1'.
              screen-input = 1.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0207' .
        if wa_tela-tela = '0207'.
          loop at screen.
            if screen-name = 'OPRD_TRIB'
        or screen-name = 'OPRD_ISENT'
        or screen-name = 'OPRD_NTRIB'
        or screen-name = 'WG_DENTRO-CFOP'
        or screen-name = 'WG_DENTRO-TAXLW1'
        or screen-name = 'WG_DENTRO-TAXLW2'
        or screen-name = 'WG_DENTRO-TAXLW4'
        or screen-name = 'WG_DENTRO-TAXLW3'
        or screen-name = 'WG_DENTRO-TAXLW5'
        or screen-name = 'WG_DENTRO-TAXCODE'
        or screen-name = 'WL_DESC_DEN_CFOP'
        or screen-name = 'WL_DESC_DEN_TAXLW1'
        or screen-name = 'WL_DESC_DEN_TAXLW2'
        or screen-name = 'WL_DESC_DEN_TAXLW3'
        or screen-name = 'WL_DESC_DEN_TAXLW4'
        or screen-name = 'WL_DESC_DEN_TAXLW5'
        or screen-name = 'WL_DESC_DEN_TAXCODE'
            or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'OPRD_TRIB'
        or screen-name = 'OPRD_ISENT'
        or screen-name = 'OPRD_NTRIB'
        or screen-name = 'WG_DENTRO-CFOP'
        or screen-name = 'WG_DENTRO-TAXLW1'
        or screen-name = 'WG_DENTRO-TAXLW2'
        or screen-name = 'WG_DENTRO-TAXLW4'
        or screen-name = 'WG_DENTRO-TAXLW3'
        or screen-name = 'WG_DENTRO-TAXLW5'
        or screen-name = 'WG_DENTRO-TAXCODE'
        or screen-name = 'WL_DESC_DEN_CFOP'
        or screen-name = 'WL_DESC_DEN_TAXLW1'
        or screen-name = 'WL_DESC_DEN_TAXLW2'
        or screen-name = 'WL_DESC_DEN_TAXLW3'
        or screen-name = 'WL_DESC_DEN_TAXLW4'
        or screen-name = 'WL_DESC_DEN_TAXLW5'
        or screen-name = 'WL_DESC_DEN_TAXCODE'
            or screen-group1 = 'A1'.
              screen-input = 1.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0208'.
        if wa_tela-tela = '0208'.
          call method obg_descbox->set_readonly_mode( readonly_mode = obg_descbox->false ).
        else.
          call method obg_descbox->set_readonly_mode( readonly_mode = obg_descbox->true ).
        endif.
      when '0209'.
        if wa_tela-tela = '0209'.
          loop at screen.
            if screen-name = 'TG_CFOP-UF'
            or screen-name = 'TG_CFOP-CFOP'
            or screen-name = 'TC_CFOP'
            or screen-name = 'TC_CFOP_INSERT'
            or screen-name = 'TC_CFOP_DELETE'
              or screen-group1 = 'A1'.
              screen-input = 1.
            endif.
            modify screen.
          endloop.
        else.
          loop at screen.
            if screen-name = 'TG_CFOP-UF'
            or screen-name = 'TG_CFOP-CFOP'
            or screen-name = 'TC_CFOP'
            or screen-name = 'TC_CFOP_INSERT'
            or screen-name = 'TC_CFOP_DELETE'
              or screen-group1 = 'A1'.
              screen-input = 0.
            endif.
            modify screen.
          endloop.
        endif.
      when '0210'.
        if wa_tela-tela = '0210'.
          loop at screen.
            if screen-name = 'ZFIWRT0032-NCM'
              or screen-name = 'ZFIWRT0032-MATERIAL'
              or screen-name = 'ZFIWRT0032-DEPOSITO'
              or screen-name = 'ZFIWRT0032-LOTE'
              or screen-name = 'ZFIWRT0032-LOTE_AUT'
              or screen-name = 'TG_ZFIWRT0032-NCM'
              or screen-name = 'TG_ZFIWRT0032-MATERIAL'
              or screen-name = 'TG_ZFIWRT0032-DEPOSITO'
              or screen-name = 'TG_ZFIWRT0032-LOTE'
              or screen-name = 'TG_ZFIWRT0032-LOTE_AUT'
              or screen-name = 'TC_ZFIWRT0032_INSERT'
              or screen-name = 'TC_ZFIWRT0032_DELETE'
              or screen-group1 = 'A1'.
              screen-input = 1.
              modify screen.
            endif.
          endloop.
        else.
          loop at screen.
            if screen-name = 'ZFIWRT0032-NCM'
              or screen-name = 'ZFIWRT0032-MATERIAL'
              or screen-name = 'ZFIWRT0032-DEPOSITO'
              or screen-name = 'ZFIWRT0032-LOTE'
              or screen-name = 'ZFIWRT0032-LOTE_AUT'
              or screen-name = 'TG_ZFIWRT0032-NCM'
              or screen-name = 'TG_ZFIWRT0032-MATERIAL'
              or screen-name = 'TG_ZFIWRT0032-DEPOSITO'
              or screen-name = 'TG_ZFIWRT0032-LOTE'
              or screen-name = 'TG_ZFIWRT0032-LOTE_AUT'
              or screen-name = 'TC_ZFIWRT0032_INSERT'
              or screen-name = 'TC_ZFIWRT0032_DELETE'
              or screen-group1 = 'A1'.
              screen-input = 0.
              modify screen.
            endif.
          endloop.
        endif.
    endcase.
  endif.

endform.
