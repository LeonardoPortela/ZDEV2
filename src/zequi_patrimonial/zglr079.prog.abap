*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Paulo Ferraz                                            &*
*& Data.....: 15.04.2024                                              &*
*& Descrição: CTA - equivalencia patrimonial                          &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT zglr079.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_pais  TYPE land1 OBLIGATORY,
              p_mfunc TYPE z_moeda_func OBLIGATORY,
              p_ini   TYPE dats OBLIGATORY,
              p_fim   TYPE dats OBLIGATORY,
              p_ivtd  TYPE bukrs OBLIGATORY,
              p_ivda  TYPE bukrs OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.

  DATA: go_cta TYPE REF TO zclfi_cta.
  DATA: gs_cta TYPE zsfi_cta.

  CLEAR: gs_cta.
  gs_cta-pais = p_pais.
  gs_cta-moeda_funcional = p_mfunc.
  gs_cta-inicio_validade = p_ini.
  gs_cta-fim_validade = p_fim.
  gs_cta-empresa_investida = p_ivda.
  gs_cta-empresa_investidora = p_ivtd.

  go_cta = NEW zclfi_cta( gs_cta ).
  go_cta->processa( ).
