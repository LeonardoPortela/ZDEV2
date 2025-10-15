* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZMMR173                                                *
* Title.......: Gerar arquivo CSV com informacoes dos pedidos          *
* Date........: 09/03/2022                                             *
* -------------------------------------------------------------------- *
REPORT zmmr173.

INCLUDE: zmmr173_top,
         zmmr173_src,
         zmmr173_f01.

INITIALIZATION.
  PERFORM f_preenche_campos_tela.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modifica_tela.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_ajuda_de_pesquisa CHANGING p_file.

START-OF-SELECTION.
  PERFORM: f_valida_campos_tela,
           f_seleciona_dados,
           f_monta_saida,
           f_grava_arquivo.
