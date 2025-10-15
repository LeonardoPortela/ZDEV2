FUNCTION-POOL zgfs_pagamentos.              "MESSAGE-ID ..

************************************************************************
* Variaveis locais
************************************************************************
DATA: l_message      TYPE bapiret2-message,
      l_subrc        TYPE sy-subrc,
      l_mode         TYPE c,
      l_gjahr        TYPE bkpf-gjahr,
      l_erro         TYPE c,
      l_msgv1        TYPE sy-msgv1,
      l_motivo       TYPE bkpf-stgrd,
      l_fb08_belnr   TYPE bkpf-belnr,
      l_fb08_message TYPE bapiret2-message,
      l_fbra_message TYPE bapiret2-message,
      w_bkpf         TYPE bkpf,
      w_zfit0167     TYPE zfit0167.

************************************************************************
************************************************************************
