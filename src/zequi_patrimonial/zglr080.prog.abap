*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Paulo Ferraz                                            &*
*& Data.....: 13.05.2024                                              &*
*& Descrição: MEP - Mov equivalencia patrimonial                      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT zglr080.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_monat TYPE monat OBLIGATORY,
              p_exer  TYPE gjahr OBLIGATORY,
              p_ivtd  TYPE bukrs OBLIGATORY,
              p_ivda  TYPE bukrs OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  DATA: go_mep TYPE REF TO zclfi_mep.
  DATA: gs_mep TYPE zsfi_mep_process.

  CLEAR: gs_mep.

  gs_mep-mes = p_monat.
  gs_mep-ano = p_exer.
  gs_mep-empresa_investida = p_ivda.
  gs_mep-empresa_investidora = p_ivtd.

  go_mep = NEW zclfi_mep( gs_mep ).
  go_mep->processa( ).
