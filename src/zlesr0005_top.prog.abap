*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 10/10/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0224 (Prest. Serv. Frete - Faturar)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |10/10/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*

TABLES: zlest0257, *zlest0257, *zlest0255, *zsdt0001od, *zlest0002,
        lfa1, *lfa1, *zib_nfe_dist_ter, *zib_nfe_dist_itm.

*----------------------------------------------------------------------*
* T Y P E S                                                            *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_conj_veic,
         nome_veic    TYPE char10,
         pc_veiculo   TYPE zpc_veiculo,
         tp_veiculo   TYPE ztp_veiculo,
         proprietario TYPE lifnr,
         name1        TYPE name1_gp,
         stcd1        TYPE stcd1,
         stcd2        TYPE stcd2,
         stcd3        TYPE stcd3,
         grupo        TYPE char3,
         frota        TYPE char4,
         bahns        TYPE bahns,
       END   OF ty_conj_veic.

*----------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                         *
*----------------------------------------------------------------------*
DATA: tg_fcat      TYPE          lvc_t_fcat,
      tg_frete_fat TYPE TABLE OF zlese_alv_frete_faturar,
      tg_conj_veic TYPE TABLE OF ty_conj_veic.

*----------------------------------------------------------------------*
* V A R I A B L E S                                                    *
*----------------------------------------------------------------------*
* O SY-UCOMM é uma variável de sitema que vai assumindo os botões que são acionados pelos
*usuários durante o processamento. Por conta disso, foram criadas as duas variáveis abaixo
*que permitem saber qual foi o botão acionado na tela anterior.
DATA: gv_acao      TYPE syucomm,    "Usado como auxiliar com possibilidade de nível 2 de tela
      gv_ucomm     TYPE syucomm,    "Usado como auxiliar com possibilidade de nível 3 de tela
      gv_erro      TYPE c,
      vg_viagem_id TYPE zde_viagem_id.

*----------------------------------------------------------------------*
* C O N S T A N T S                                                    *
*----------------------------------------------------------------------*
CONSTANTS: gc_container  TYPE scrfname VALUE 'CCTR_ALV_FRE_FAT',
           gc_container2 TYPE scrfname VALUE 'CCTR_ALV_CJT_VIC'.

*----------------------------------------------------------------------*
* C L A S S  D E C L A R E T I O N                                     *
*----------------------------------------------------------------------*
DATA: gcl_custom_container  TYPE REF TO cl_gui_custom_container,
      gcl_grid1             TYPE REF TO cl_gui_alv_grid,
      gcl_custom_container2 TYPE REF TO cl_gui_custom_container,
      gcl_grid2             TYPE REF TO cl_gui_alv_grid.
