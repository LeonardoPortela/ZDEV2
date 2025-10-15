*&---------------------------------------------------------------------*
*& Report  ZSDR0015                                                    *
* Descrição : Programa para Atualização Categoria de Investimentos 2012*
* Módulo    :                                Transação: -              *
*                                                                      *
*----------------------------------------------------------------------*
* Autor     : Camila Brand                            Data: 25/01/2012 *
* Observações: Desenvolvimento inicial do Programa - DEVK920417        *
* Conforme chamado - 59788                                             *
*----------------------------------------------------------------------*

report  zsdr0015.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables:
   zim01_sol_ap_inv. " Solicitação de aprovação de investimento

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
data:
     it_zim01_sol_ap_inv type table of zim01_sol_ap_inv.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
data:
      wk_zim01_sol_ap_inv type zim01_sol_ap_inv.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.

select-options:  " Informações para atualização
                 p_kostl for zim01_sol_ap_inv-kostl  no intervals, " obligatory , " Centro de custo
                 p_posnr for zim01_sol_ap_inv-posnr  no intervals, " obligatory , " Nº interno - solicitação de investimento
                 p_izwek for zim01_sol_ap_inv-izwek no-extension no intervals, "obligatory , " Motivo do investimento
                 p_txt50 for zim01_sol_ap_inv-txt50 no-extension no intervals. "obligatory . " Texto

selection-screen: end of block b1.


*Atualizar os campos ZIM01_SOL_AP_INV-IZWEK e
* ZIM01_SOL_AP_INV-TXT50 para os registros
* onde ZIM01_SOL_AP_INV-KOSTL=Centro de custo Parametro
* e ZIM01_SOL_AP_INV-POSNR=Sol.Inv. Parametro

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  perform: form_atualiza_zim01_sol_ap_inv.

*&---------------------------------------------------------------------*
*&      Form  form_atualiza_zim01_sol_ap_inv
*&---------------------------------------------------------------------*
form form_atualiza_zim01_sol_ap_inv.



   IF p_kostl-low is initial .
    MESSAGE i000(z01) WITH 'Informar Centro de Custo' .
    STOP.
   ENDIF.

   IF p_posnr-low is initial .
    MESSAGE i000(z01) WITH 'Informar o Número Interno' .
    STOP.
   ENDIF.

   IF  p_izwek-low is initial .
    MESSAGE i000(z01) WITH 'Informar o Motivo do Investimento' .
    STOP.
   ENDIF.

   IF  p_txt50-low is initial .
    MESSAGE i000(z01) WITH 'Informar o Texto Obrigatório' .
    STOP.
   ENDIF.

  select *
  from zim01_sol_ap_inv
  into table it_zim01_sol_ap_inv
where kostl in p_kostl
  and posnr in p_posnr.

  if it_zim01_sol_ap_inv is not initial.
    loop at it_zim01_sol_ap_inv into wk_zim01_sol_ap_inv.
      move: p_izwek-low  to wk_zim01_sol_ap_inv-izwek,
            p_txt50-low  to wk_zim01_sol_ap_inv-txt50.
      modify zim01_sol_ap_inv from wk_zim01_sol_ap_inv.
      clear: wk_zim01_sol_ap_inv.
    endloop.
  endif.

  message i000(z01) with 'Processamento concluido com sucesso!'.

  clear: wk_zim01_sol_ap_inv,
         it_zim01_sol_ap_inv,
         p_kostl,
         p_posnr,
         p_izwek,
         p_txt50.


endform.      "form_atualiza_zim01_sol_ap_inv.
