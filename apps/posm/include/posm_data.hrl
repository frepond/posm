-record(posm_kv,  {key, 
				   value
				  }).

%% tables bank and account
-record(posm_bank, {cubco,	 % Identificador del Banco
			        nombco,	 % Nombre del Banco
			        accounts % Cuentas del banco [{cubcta, nro_cta, nro_cbu}]
			  	   }).