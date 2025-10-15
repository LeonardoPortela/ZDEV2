*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 22/09/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0223 (Prest. Serv. Frete Terceiros)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |22/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*

TABLES: zlest0255, *zlest0255, *kna1.

*----------------------------------------------------------------------*
* T Y P E S                                                            *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_editor,
         line TYPE c LENGTH 163,
       END   OF ty_editor,

       BEGIN OF ty_editor2,
         line TYPE c LENGTH 72,
       END   OF ty_editor2.

*----------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                         *
*----------------------------------------------------------------------*
DATA: tg_fcat    TYPE          lvc_t_fcat,
      tg_frete   TYPE TABLE OF zlese_alv_frete_terc,
      tg_editor  TYPE TABLE OF ty_editor,
      tg_editor2 TYPE TABLE OF ty_editor.

*----------------------------------------------------------------------*
* W O R K  A R E A                                                     *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* V A R I A B L E S                                                    *
*----------------------------------------------------------------------*
DATA: gv_db_tab  TYPE tabname,
      gv_stcnam  TYPE tabname,
      gv_scmant  TYPE c LENGTH 4,
      gv_title   TYPE cua_tit_tx,
      gv_act_01  TYPE cua_tit_tx,
      gv_butxt   TYPE butxt,
      gv_wgbez60 TYPE wgbez60,
      gv_maktx   TYPE maktx,
      gv_text1   TYPE text1_052,
      gv_ktext   TYPE vtxtk,
      gv_zone_pc TYPE lzone,
      gv_zone_lr TYPE lzone,
* O SY-UCOMM é uma variável de sitema que vai assumindo os botões que são acionados pelos
*usuários durante o processamento. Por conta disso, foram criadas as duas variáveis abaixo
*que permitem saber qual foi o botão acionado na tela anterior.
      gv_acao    TYPE syucomm,    "Usado como auxiliar com possibilidade de nível 2 de tela
      gv_ucomm   TYPE syucomm,    "Usado como auxiliar com possibilidade de nível 3 de tela
      gv_erro    TYPE c.

*----------------------------------------------------------------------*
* R A N G E S                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* C O N S T A N T S                                                    *
*----------------------------------------------------------------------*
CONSTANTS: gc_db_tab    TYPE tabname    VALUE 'ZLEST0256',
           gc_stcnam    TYPE tabname    VALUE 'ZLEST0256_OUT',
           gc_scmant    TYPE c LENGTH 4 VALUE '0340',
           gc_container TYPE scrfname   VALUE 'CCTR_ALV_FRETE',
           gc_descbox   TYPE scrfname   VALUE 'CC_DESC',
           gc_can_fin   TYPE scrfname   VALUE 'CC_CAN_FIN'.

*----------------------------------------------------------------------*
* C L A S S  D E C L A R E T I O N                                     *
*----------------------------------------------------------------------*
DATA: gcl_custom_container TYPE REF TO cl_gui_custom_container,
      gcl_grid1            TYPE REF TO cl_gui_alv_grid,
      gcl_cc_desc          TYPE REF TO cl_gui_custom_container,
      gcl_observacao       TYPE REF TO cl_gui_textedit,
      gcl_cc_can_fin       TYPE REF TO cl_gui_custom_container,
      gcl_canc_final       TYPE REF TO cl_gui_textedit.
