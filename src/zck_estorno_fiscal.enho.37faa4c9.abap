"Name: \FU:J_1B_NFE_CHECK_CANCEL_STATUS\SE:END\EI
ENHANCEMENT 0 ZCK_ESTORNO_FISCAL.
*
  "Manual de Orientação do Contribuinte
  "4 Web Services ................................................................................................................ 028
  "4.11 Web Service – NFeConsultaDest ............................................................................................ 104
  "4.11.9 Recomendações para evitar o uso indevido ............................................................................... 110
  "A análise do comportamento atual das aplicações das empresas ("aplicação cliente") permite identificar algumas situações de "uso indevido"
  "dos ambientes de autorização de Nota Fiscal Eletrônica mantidos pelas SEFAZ.
  "Como exemplo maior do mau uso do ambiente de autorização, ressalta-se a falta de controle de algumas aplicações que entram em "loop",
  "consumindo recursos de forma indevida, sobrecarregando principalmente o canal de comunicação com a Internet.
  "Para este Web Service de Consulta às operações destinadas serão mantidos controles para identificar as situações de uso indevido de
  "sucessivas tentativas de busca de registros já disponibilizados anteriormente.
  "As novas tentativas serão rejeitadas com o erro "656 – Rejeição: Consumo Indevido".

  IF NOT ls_act-form IS INITIAL and ls_act-CODE eq '656'.
    clear gv_docnum_cancel.
    MESSAGE E017(Z01) RAISING cancel_not_allowed.
  endif.

ENDENHANCEMENT.
