
wg_prazo_retirada_01 = wg_cond_pgto-prazo_ret01.
wg_prazo_retirada_02 = wg_cond_pgto-prazo_ret02.


"REPLACE ALL OCCURRENCES OF '.'  IN wg_prazo_retirada_01 WITH '/'.
"REPLACE ALL OCCURRENCES OF '.'  IN wg_prazo_retirada_02 WITH '/'.

CONCATENATE wg_prazo_retirada_01+6(2) '/'
            wg_prazo_retirada_01+4(2)'/'
            wg_prazo_retirada_01+0(4) into WG_PRAZO_RETIRADA_01.



CONCATENATE wg_prazo_retirada_02+6(2) '/'
            wg_prazo_retirada_02+4(2) '/'
            wg_prazo_retirada_02+0(4) into WG_PRAZO_RETIRADA_02.
















