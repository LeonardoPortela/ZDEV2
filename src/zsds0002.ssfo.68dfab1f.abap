data: wl_cultura(30).
clear: wg_desc_vencimento, wl_cultura.
wl_cultura = wg_header-cultura.
condense wl_cultura no-gaps.
concatenate 'Dt.Pagamento' wl_cultura
 into wg_desc_vencimento separated by '-'.
 condense wg_desc_vencimento no-gaps.
 translate wg_desc_vencimento using '- '.





















