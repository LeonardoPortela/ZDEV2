
CONCATENATE 'ATÉ O VENCIMENTO EM QUALQUER BANCO OU CORRESPONDENTE NÃO BANCÁRIO.'
'APÓS O VENCIMENTO ACESSE ITAU.COM.BR/BOLETOS E PAGUE EM QUALQUER BANCO'
'OU CORRESPONDENTE NÃO BANCÁRIO' INTO lva_texto SEPARATED BY space.

CONCATENATE wa_saida-zbd1t+6(2) '/'  wa_saida-zbd1t+4(2) '/'
wa_saida-zbd1t+0(4) INTO lva_data_venc.

CONCATENATE wa_saida-data_sist+6(2) '/' wa_saida-data_sist+4(2) '/'
wa_saida-data_sist+0(4) INTO lva_data_sist.

concatenate wa_saida-dtaid+0(3) '/' wa_saida-dtaid+3(8) '-' WA_SAIDA-DTAID+11(1)
into lva_nosso_numero.




