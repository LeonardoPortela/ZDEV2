*&-------------------------------------------------------------------------------------------------------*
*& Método         : MZSD_APROV_SEM_SALDO_TOP (Include)                                                   *
*& Chamado        : USER STORY 169312                                                                    *
*& Data           : 21/03/2025                                                                           *
*& Especificado   : Leonardo Portela                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 21/03/2025|DEVK9A1XAW |NSEGATIN       | Aprovar NFL sem Saldo a Vincular. Desenvolvimento inicial.    *
*--------------------------------------------------------------------------------------------------------*

CONTROLS gts_9000 TYPE TABSTRIP.

*--------------------------------------------------------------------*
* C O N S T A N T S                                                  *
*----------------------------------------------------------------
CONSTANTS: BEGIN OF cg_sb_9000,
             t1_pend TYPE syucomm VALUE 'T1_PEND',
             t2_apre TYPE syucomm VALUE 'T2_APRE',
             scr_pen TYPE sydynnr VALUE '9100',
             scr_a_r TYPE sydynnr VALUE '9200',
             scr_apr TYPE sydynnr VALUE '9300',
             scr_mtv TYPE sydynnr VALUE '9400',
           END   OF cg_sb_9000.

*--------------------------------------------------------------------*
* I N T E R N A L  T A B L E S                                       *
*--------------------------------------------------------------------*
DATA: tg_vinc_f_apv_pen TYPE TABLE OF zsdtvinc_f_aprov,
      tg_vinc_f_apv_a_r TYPE TABLE OF zsdtvinc_f_aprov,
      tg_vinc_aprov     TYPE TABLE OF zsdtvinc_aprov,
      tg_aprovador_buk  TYPE TABLE OF zsdtvinc_aprov.

*--------------------------------------------------------------------*
* W O R K   A R E A S                                                *
*--------------------------------------------------------------------*
DATA: BEGIN OF eg_sbsc_9000,
        subscreen   TYPE sydynnr,
        subscreen2  TYPE sydynnr,
        program     TYPE syrepid VALUE 'SAPMZSD_APROV_SEM_SALDO',
        pressed_tab TYPE syucomm VALUE cg_sb_9000-t1_pend,
      END OF eg_sbsc_9000.

*--------------------------------------------------------------------*
* V A R I A B L E S                                                  *
*--------------------------------------------------------------------*
DATA: vg_status TYPE zsdestatus.
*--------------------------------------------------------------------*
* C L A S S   D E C L A R A T I O N S                                *
*--------------------------------------------------------------------*
DATA: clg_table_pen  TYPE REF TO cl_salv_table,
      clg_table_a_r  TYPE REF TO cl_salv_table,
      clg_table_apr  TYPE REF TO cl_salv_table.
* Variáveis do editor do Motivo Aprovação/Reprovação.
DATA: clg_txtedit_cstm_cntner TYPE REF TO cl_gui_custom_container,
      clg_editor              TYPE REF TO cl_gui_textedit.

*--------------------------------------------------------------------*
* C L A S S   D E F I N I T I O N                                    *
*--------------------------------------------------------------------*
CLASS clg_event_handler DEFINITION.
  PUBLIC SECTION.
* To implement double click in ALV Grid.
    METHODS: zm_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row
                column.

  PRIVATE SECTION.

ENDCLASS.
*--------------------------------------------------------------------*
* C L A S S   I M P L E M E N T A T I O N S                          *
*--------------------------------------------------------------------*
CLASS clg_event_handler IMPLEMENTATION.
  METHOD zm_double_click.
    PERFORM zf_double_click USING row
                                  column.

  ENDMETHOD.                    "ZF_TOP_OF_PAGE
ENDCLASS.                    "ZCL_EVENT_HANDLER IMPLEMENTATION
