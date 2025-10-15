TYPE-POOL zctet .

TYPES: BEGIN OF zctet_info_nota.
        INCLUDE STRUCTURE zcte_info_nota.
TYPES:  mark            TYPE c LENGTH 1.
TYPES: END OF zctet_info_nota.

TYPES: BEGIN OF zctet_endereco,
         name1      TYPE ad_name1,                  "Nome
         state_insc TYPE j_1bstains,                "Insc. Estadual
         stcd1      TYPE stcd1,                     "CNPJ
         stcd2      TYPE stcd2,                     "CPF
         city2      TYPE ad_city2,                  "Bairro
         post_code1 TYPE adrc-post_code1,           "CEP
         street     TYPE adrc-street,               "Rua
         house_num1 TYPE adrc-house_num1,           "Número
         country    TYPE land1,                     "País
         langu      TYPE spras,                     "Idioma
         region     TYPE adrc-region,               "Estado
         taxjurcode	TYPE ad_txjcd,                  "Cidade
         tel_number	TYPE ad_tlnmbr1,                "Telefone
       END OF zctet_endereco.

TYPES: BEGIN OF zctet_info_parceiro.
TYPES: codigo	 TYPE lifnr,
       cnpj	   TYPE stcd1,
       cpf     TYPE stcd2,
       ie	     TYPE stcd3,
       xnome   TYPE char_60,
       xfant 	 TYPE char_60,
       fone	   TYPE ad_tlnmbr1,
       xlgr    TYPE ad_street,
       nro 	   TYPE ad_hsnm1,
       xcpl	   TYPE char_60,
       xbairro TYPE ad_city2,
       cmun	   TYPE char07,
       xmun	   TYPE char_60,
       cep     TYPE pstlz_bas,
       uf	     TYPE regio.
TYPES: END OF zctet_info_parceiro.
