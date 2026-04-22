library(tsibble)
library(prophet)
library(fabletools)
library(forecast)

aplicar_imputacion <- function(ts_data, metodo_imputacion) {
  if (metodo_imputacion == "interpolacion") {
    ts_data <- ts_data %>%
      mutate(valor = approx(x = as.numeric(fecha), y = valor,
                            xout = as.numeric(fecha), rule = 2)$y)
  } else if (metodo_imputacion == "media") {
    media_valor <- mean(ts_data$valor, na.rm = TRUE)
    ts_data <- ts_data %>%
      mutate(valor = ifelse(is.na(valor), media_valor, valor))
  } else if (metodo_imputacion == "forward_fill") {
    ts_data <- ts_data %>%
      tidyr::fill(valor, .direction = "down")
  } else if (metodo_imputacion == "backward_fill") {
    ts_data <- ts_data %>%
      tidyr::fill(valor, .direction = "up")
  } else if (metodo_imputacion == "spline") {
    if (sum(!is.na(ts_data$valor)) >= 4) {
      ts_data <- ts_data %>%
        mutate(valor = spline(x = as.numeric(fecha)[!is.na(valor)],
                              y = valor[!is.na(valor)],
                              xout = as.numeric(fecha))$y)
    } else {
      ts_data <- ts_data %>%
        mutate(valor = approx(x = as.numeric(fecha), y = valor,
                              xout = as.numeric(fecha), rule = 2)$y)
    }
  }

  if (any(is.na(ts_data$valor))) {
    ts_data <- ts_data %>%
      tidyr::fill(valor, .direction = "downup")
  }

  cat("Imputación completada. Valores NA restantes:", sum(is.na(ts_data$valor)), "\n")
  return(ts_data)
}
extraer_intervalos <- function(forecast_obj, nivel_conf) {
  nivel_str <- paste0(nivel_conf * 100, "%")
  if (nivel_str %in% names(forecast_obj)) {
    intervalos <- forecast_obj[[nivel_str]]
    if (is.matrix(intervalos) && ncol(intervalos) == 2) {
      return(list(
        inferior = as.numeric(intervalos[, 1]),
        superior = as.numeric(intervalos[, 2])
      ))
    } else if (is.numeric(intervalos)) {
      return(list(
        inferior = as.numeric(intervalos),
        superior = as.numeric(intervalos)
      ))
    }
  }
  return(list(
    inferior = as.numeric(forecast_obj$.mean) - 1.96 * sd(as.numeric(forecast_obj$.mean), na.rm = TRUE),
    superior = as.numeric(forecast_obj$.mean) + 1.96 * sd(as.numeric(forecast_obj$.mean), na.rm = TRUE)
  ))
}
ejecutar_pronosticos <- function(train_data, test_data, h_periods, fechas_futuras, metodos, nivel_confianza, nombre_columna) {

  resultados <- list()
  metricas <- data.frame()

  # ARIMA ----
  if ("arima" %in% metodos) {
    tryCatch({
      modelo_arima <- train_data %>% model(ARIMA(valor))
      forecast_arima <- modelo_arima %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_arima, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_arima <- modelo_arima %>% forecast(h = nrow(test_data))
        rmse_arima <- sqrt(mean((test_data$valor - forecast_test_arima$.mean)^2, na.rm = TRUE))
        mape_arima <- mean(abs((test_data$valor - forecast_test_arima$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_arima <- NA; mape_arima <- NA
      }

      resultados$arima <- data.frame(
        fecha = fechas_futuras,
        metodo = "ARIMA",
        valor = as.numeric(forecast_arima$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "ARIMA", RMSE = rmse_arima, MAPE = mape_arima))
      cat("✓ ARIMA completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en ARIMA para", nombre_columna, ":", e$message)))
  }

  # ETS ----
  if ("ets" %in% metodos) {
    tryCatch({
      modelo_ets <- train_data %>% model(ETS(valor))
      forecast_ets <- modelo_ets %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_ets, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_ets <- modelo_ets %>% forecast(h = nrow(test_data))
        rmse_ets <- sqrt(mean((test_data$valor - forecast_test_ets$.mean)^2, na.rm = TRUE))
        mape_ets <- mean(abs((test_data$valor - forecast_test_ets$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_ets <- NA; mape_ets <- NA
      }

      resultados$ets <- data.frame(
        fecha = fechas_futuras,
        metodo = "ETS",
        valor = as.numeric(forecast_ets$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "ETS", RMSE = rmse_ets, MAPE = mape_ets))
      cat("✓ ETS completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en ETS para", nombre_columna, ":", e$message)))
  }

  # NNAR ----
  if ("nnetar" %in% metodos) {
    tryCatch({
      modelo_nnar <- train_data %>% model(NNETAR(valor))
      forecast_nnar <- modelo_nnar %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_nnar, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_nnar <- modelo_nnar %>% forecast(h = nrow(test_data))
        rmse_nnar <- sqrt(mean((test_data$valor - forecast_test_nnar$.mean)^2, na.rm = TRUE))
        mape_nnar <- mean(abs((test_data$valor - forecast_test_nnar$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_nnar <- NA; mape_nnar <- NA
      }

      resultados$nnetar <- data.frame(
        fecha = fechas_futuras,
        metodo = "NNAR",
        valor = as.numeric(forecast_nnar$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "NNAR", RMSE = rmse_nnar, MAPE = mape_nnar))
      cat("✓ NNAR completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en NNAR para", nombre_columna, ":", e$message)))
  }

  # MEAN ----
  if ("mean" %in% metodos) {
    tryCatch({
      modelo_mean <- train_data %>% model(MEAN(valor))
      forecast_mean <- modelo_mean %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_mean, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_mean <- modelo_mean %>% forecast(h = nrow(test_data))
        rmse_mean <- sqrt(mean((test_data$valor - forecast_test_mean$.mean)^2, na.rm = TRUE))
        mape_mean <- mean(abs((test_data$valor - forecast_test_mean$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_mean <- NA; mape_mean <- NA
      }

      resultados$mean <- data.frame(
        fecha = fechas_futuras,
        metodo = "MEAN",
        valor = as.numeric(forecast_mean$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "MEAN", RMSE = rmse_mean, MAPE = mape_mean))
      cat("✓ MEAN completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en MEAN para", nombre_columna, ":", e$message)))
  }

  # NAIVE ----
  if ("naive" %in% metodos) {
    tryCatch({
      modelo_naive <- train_data %>% model(NAIVE(valor))
      forecast_naive <- modelo_naive %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_naive, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_naive <- modelo_naive %>% forecast(h = nrow(test_data))
        rmse_naive <- sqrt(mean((test_data$valor - forecast_test_naive$.mean)^2, na.rm = TRUE))
        mape_naive <- mean(abs((test_data$valor - forecast_test_naive$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_naive <- NA; mape_naive <- NA
      }

      resultados$naive <- data.frame(
        fecha = fechas_futuras,
        metodo = "NAIVE",
        valor = as.numeric(forecast_naive$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "NAIVE", RMSE = rmse_naive, MAPE = mape_naive))
      cat("✓ NAIVE completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en NAIVE para", nombre_columna, ":", e$message)))
  }

  # SNAIVE ----
  if ("snaive" %in% metodos) {
    tryCatch({
      modelo_snaive <- train_data %>% model(SNAIVE(valor ~ lag("year")))
      forecast_snaive <- modelo_snaive %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_snaive, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_snaive <- modelo_snaive %>% forecast(h = nrow(test_data))
        rmse_snaive <- sqrt(mean((test_data$valor - forecast_test_snaive$.mean)^2, na.rm = TRUE))
        mape_snaive <- mean(abs((test_data$valor - forecast_test_snaive$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_snaive <- NA; mape_snaive <- NA
      }

      resultados$snaive <- data.frame(
        fecha = fechas_futuras,
        metodo = "SNAIVE",
        valor = as.numeric(forecast_snaive$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "SNAIVE", RMSE = rmse_snaive, MAPE = mape_snaive))
      cat("✓ SNAIVE completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en SNAIVE para", nombre_columna, ":", e$message)))
  }

  # TSLM ----
  if ("tslm" %in% metodos) {
    tryCatch({
      modelo_tslm <- train_data %>% model(TSLM(valor ~ trend() + season()))
      forecast_tslm <- modelo_tslm %>% forecast(h = h_periods, level = nivel_confianza * 100)

      intervalos <- extraer_intervalos(forecast_tslm, nivel_confianza)

      if (nrow(test_data) > 0) {
        forecast_test_tslm <- modelo_tslm %>% forecast(h = nrow(test_data))
        rmse_tslm <- sqrt(mean((test_data$valor - forecast_test_tslm$.mean)^2, na.rm = TRUE))
        mape_tslm <- mean(abs((test_data$valor - forecast_test_tslm$.mean) / test_data$valor) * 100, na.rm = TRUE)
      } else {
        rmse_tslm <- NA; mape_tslm <- NA
      }

      resultados$tslm <- data.frame(
        fecha = fechas_futuras,
        metodo = "TSLM",
        valor = as.numeric(forecast_tslm$.mean),
        limite_inferior = intervalos$inferior,
        limite_superior = intervalos$superior,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "TSLM", RMSE = rmse_tslm, MAPE = mape_tslm))
      cat("✓ TSLM completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en TSLM para", nombre_columna, ":", e$message)))
  }

  # Prophet ----
  if ("prophet" %in% metodos) {
    tryCatch({
      prophet_data <- train_data %>%
        mutate(ds = as.Date(fecha), y = valor) %>%
        select(ds, y) %>%
        filter(!is.na(y))

      modelo_prophet <- prophet(prophet_data,
                                interval.width = nivel_confianza,
                                yearly.seasonality = TRUE,
                                weekly.seasonality = FALSE,
                                daily.seasonality = FALSE)

      future_prophet <- make_future_dataframe(modelo_prophet, periods = h_periods, freq = "month")
      forecast_prophet <- predict(modelo_prophet, future_prophet)
      forecast_prophet_future <- tail(forecast_prophet, h_periods)

      if (nrow(test_data) > 0) {
        test_prophet <- test_data %>%
          mutate(ds = as.Date(fecha), y = valor) %>%
          select(ds, y) %>%
          filter(!is.na(y))

        if (nrow(test_prophet) > 0) {
          forecast_test_prophet <- predict(modelo_prophet, test_prophet)
          rmse_prophet <- sqrt(mean((test_prophet$y - forecast_test_prophet$yhat)^2, na.rm = TRUE))
          mape_prophet <- mean(abs((test_prophet$y - forecast_test_prophet$yhat) / test_prophet$y) * 100, na.rm = TRUE)
        } else {
          rmse_prophet <- NA; mape_prophet <- NA
        }
      } else {
        rmse_prophet <- NA; mape_prophet <- NA
      }

      resultados$prophet <- data.frame(
        fecha = fechas_futuras,
        metodo = "Prophet",
        valor = forecast_prophet_future$yhat,
        limite_inferior = forecast_prophet_future$yhat_lower,
        limite_superior = forecast_prophet_future$yhat_upper,
        tipo = "Pronóstico"
      )
      metricas <- rbind(metricas, data.frame(metodo = "Prophet", RMSE = rmse_prophet, MAPE = mape_prophet))
      cat("✓ Prophet completado para", nombre_columna, "\n")
    }, error = function(e) warning(paste("Error en Prophet para", nombre_columna, ":", e$message)))
  }

  return(list(pronosticos = resultados, metricas = metricas))
}
Pronosticar <- function(df, fecha_col = "fecha", valor_cols = NULL, nivel_confianza = 0.95,
                        periodos_pronostico = NULL,
                        metodos = c("arima", "ets", "prophet", "nnetar", "mean", "naive", "snaive", "tslm"),
                        metodo_imputacion = "interpolacion", incluir_historicos = TRUE) {

  # Validaciones iniciales
  if (!fecha_col %in% names(df)) {
    stop("La columna de fecha especificada no existe en el dataframe")
  }

  # Si no se especifican columnas, usar todas las numéricas excepto fecha
  if (is.null(valor_cols)) {
    valor_cols <- names(df)[sapply(df, is.numeric) & names(df) != fecha_col]
    if (length(valor_cols) == 0) {
      stop("No se encontraron columnas numéricas para pronosticar")
    }
    cat("Columnas detectadas automáticamente:", paste(valor_cols, collapse = ", "), "\n")
  }

  # Validar que las columnas existen
  if (!all(valor_cols %in% names(df))) {
    columnas_faltantes <- valor_cols[!valor_cols %in% names(df)]
    stop(paste("Las siguientes columnas no existen:", paste(columnas_faltantes, collapse = ", ")))
  }

  # Preparar datos base
  df_clean <- df %>%
    select(fecha = all_of(fecha_col), all_of(valor_cols)) %>%
    mutate(fecha = as.Date(fecha)) %>%
    arrange(fecha)

  # Verificar valores NA por columna
  na_por_columna <- df_clean %>%
    select(-fecha) %>%
    summarise_all(~sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "columna", values_to = "na_count")

  if (any(na_por_columna$na_count > 0)) {
    cat("Valores NA encontrados por columna:\n")
    print(na_por_columna %>% filter(na_count > 0))
  }

  freq <- 12
  cat("Serie detectada como MENSUAL (frecuencia = 12)\n")

  # Lista para almacenar resultados de todas las columnas
  resultados_todas_columnas <- list()
  metricas_todas_columnas <- data.frame()

  for (col_actual in valor_cols) {
    cat("\n=== PROCESANDO COLUMNA:", col_actual, "===\n")

    # Preparar datos para la columna actual
    df_col <- df_clean %>%
      select(fecha, valor = all_of(col_actual)) %>%
      filter(!is.na(valor))

    if (nrow(df_col) < 10) {
      warning(paste("Columna", col_actual, "tiene muy pocos datos válidos. Saltando..."))
      next
    }

    # CREAR TSIBBLE CON FRECUENCIA MENSUAL
    ts_data <- df_col %>%
      mutate(fecha = yearmonth(fecha)) %>%
      as_tsibble(index = fecha) %>%
      tsibble::fill_gaps()

    # Aplicar imputación si es necesario
    if (any(is.na(ts_data$valor))) {
      cat("Aplicando imputación para", col_actual, "...\n")
      ts_data <- aplicar_imputacion(ts_data, metodo_imputacion)
    }

    # División entrenamiento/prueba
    n_total <- nrow(ts_data)
    n_train <- floor(n_total * 0.8)
    train_data <- ts_data[1:n_train, ]
    test_data <- ts_data[(n_train + 1):n_total, ]

    # Calcular períodos de pronóstico
    fecha_max <- max(as.Date(df_col$fecha))
    if (is.null(periodos_pronostico)) {
      año_siguiente <- year(fecha_max) + 1
      fecha_objetivo <- as.Date(paste0(año_siguiente, "-12-01"))
      meses_hasta_objetivo <- interval(fecha_max, fecha_objetivo) %/% months(1)
      h_periods <- max(1, meses_hasta_objetivo)
      cat("Pronosticando", h_periods, "meses hasta diciembre", año_siguiente, "para", col_actual, "\n")
    } else {
      h_periods <- periodos_pronostico
      cat("Pronosticando", h_periods, "meses especificados para", col_actual, "\n")
    }

    # Crear fechas futuras
    ultima_fecha_ts <- max(ts_data$fecha)
    fechas_futuras_ym <- seq(ultima_fecha_ts + 1, length.out = h_periods, by = 1)
    fechas_futuras <- as.Date(fechas_futuras_ym)

    # Ejecutar pronósticos para la columna actual
    resultados_col <- ejecutar_pronosticos(train_data, test_data, h_periods, fechas_futuras,
                                           metodos, nivel_confianza, col_actual)

    # Agregar resultados de esta columna
    if (length(resultados_col$pronosticos) > 0) {
      resultados_todas_columnas[[col_actual]] <- resultados_col$pronosticos

      # Agregar métricas con identificador de columna
      metricas_col <- resultados_col$metricas %>%
        mutate(columna = col_actual)
      metricas_todas_columnas <- rbind(metricas_todas_columnas, metricas_col)
    }
  }
  if (length(resultados_todas_columnas) > 0) {
    df_pronosticos_final <- map_dfr(names(resultados_todas_columnas), function(col_name) {
      map_dfr(resultados_todas_columnas[[col_name]], function(resultado_metodo) {
        resultado_metodo %>% mutate(columna = col_name)
      })
    })

    # AGREGAR DATOS HISTÓRICOS SI SE SOLICITA
    if (incluir_historicos) {
      combinaciones_metodo_columna <- df_pronosticos_final %>%
        distinct(metodo, columna)

      datos_historicos_expandidos <- map_dfr(1:nrow(combinaciones_metodo_columna), function(i) {
        metodo_actual <- combinaciones_metodo_columna$metodo[i]
        columna_actual <- combinaciones_metodo_columna$columna[i]

        df_clean %>%
          select(fecha, valor = all_of(columna_actual)) %>%
          filter(!is.na(valor)) %>%
          mutate(
            metodo = metodo_actual,
            columna = columna_actual,
            limite_inferior = NA_real_,
            limite_superior = NA_real_,
            tipo = "Histórico"
          )
      })

      df_final <- bind_rows(datos_historicos_expandidos, df_pronosticos_final) %>%
        arrange(columna, metodo, fecha)
    } else {
      df_final <- df_pronosticos_final
    }

    # Agregar atributos
    attr(df_final, "metricas") <- metricas_todas_columnas
    attr(df_final, "nivel_confianza") <- nivel_confianza
    attr(df_final, "columnas_pronosticadas") <- valor_cols
    attr(df_final, "datos_originales") <- nrow(df_clean)
    attr(df_final, "periodos_pronostico") <- h_periods
    attr(df_final, "datos_historicos") <- df_clean
    attr(df_final, "metodo_imputacion") <- metodo_imputacion
    attr(df_final, "incluye_historicos") <- incluir_historicos
    attr(df_final, "frecuencia") <- "mensual"

    cat("\n=== RESUMEN FINAL ===\n")
    cat("Columnas procesadas:", length(valor_cols), "\n")
    cat("Métodos exitosos por columna:\n")
    resumen_metodos <- df_pronosticos_final %>%
      filter(tipo == "Pronóstico") %>%
      group_by(columna) %>%
      summarise(metodos_exitosos = n_distinct(metodo), .groups = "drop")
    print(resumen_metodos)

    return(df_final)
  } else {
    stop("No se pudo generar ningún pronóstico para ninguna columna")
  }
}

PronMetricas <- function(resultado_pronostico, columna = NULL) {
  metricas <- attr(resultado_pronostico, "metricas")

  if (!is.null(metricas)) {
    cat("=== MÉTRICAS DE EVALUACIÓN ===\n")

    if (!is.null(columna)) {
      metricas_filtradas <- metricas %>% filter(columna == !!columna)
      if (nrow(metricas_filtradas) == 0) {
        cat("No se encontraron métricas para la columna:", columna, "\n")
        return()
      }
      cat("Columna:", columna, "\n")
      print(metricas_filtradas %>% select(-columna))
    } else {
      print(metricas)

      # Resumen por columna
      cat("\n=== RESUMEN POR COLUMNA ===\n")
      resumen_columnas <- metricas %>%
        group_by(columna) %>%
        summarise(
          metodos_evaluados = n(),
          mejor_rmse = min(RMSE, na.rm = TRUE),
          mejor_mape = min(MAPE, na.rm = TRUE),
          metodo_mejor_rmse = metodo[which.min(RMSE)],
          metodo_mejor_mape = metodo[which.min(MAPE)],
          .groups = "drop"
        )
      print(resumen_columnas)
    }

    cat("\nNivel de confianza:", attr(resultado_pronostico, "nivel_confianza") * 100, "%\n")
    cat("Columnas pronosticadas:", paste(attr(resultado_pronostico, "columnas_pronosticadas"), collapse = ", "), "\n")
    cat("Datos originales:", attr(resultado_pronostico, "datos_originales"), "\n")
    cat("Períodos pronosticados:", attr(resultado_pronostico, "periodos_pronostico"), "\n")
  }
}
PronSeleccionar <- function(resultado_pronostico, columna = NULL, criterio_principal = "compuesto",
                            peso_rmse = 0.5, peso_mape = 0.3, peso_intervalo = 0.2,
                            excluir_metodos = c("SNAIVE"), mostrar_analisis = TRUE,
                            fallback_method = "MEAN") {
  library(dplyr)

  metricas <- attr(resultado_pronostico, "metricas")

  if (is.null(metricas)) {
    stop("No se encontraron métricas en el resultado del pronóstico")
  }

  # Si se especifica una columna, filtrar solo esa columna
  if (!is.null(columna)) {
    if (!columna %in% metricas$columna) {
      stop(paste("La columna", columna, "no existe en los resultados"))
    }
    metricas <- metricas %>% filter(columna == !!columna)
    resultado_pronostico <- resultado_pronostico %>% filter(columna == !!columna)
    columnas_a_procesar <- columna
  } else {
    columnas_a_procesar <- unique(metricas$columna)
  }

  selecciones_por_columna <- list()

  for (col_actual in columnas_a_procesar) {
    cat("\n=== SELECCIÓN PARA COLUMNA:", col_actual, "===\n")

    # Envolvemos el procesamiento por columna en tryCatch para capturar errores y fallback
    tryCatch({
      metricas_col <- metricas %>%
        filter(columna == col_actual & !metodo %in% excluir_metodos) %>%
        filter(!is.na(RMSE) & !is.na(MAPE))

      # Si no hay métodos válidos, intentar fallback a MEAN (aunque esté en excluir_metodos)
      if (nrow(metricas_col) == 0) {
        warning(paste("No hay métodos válidos para la columna", col_actual, "- usando", fallback_method, "como fallback"))
        metricas_mean <- metricas %>% filter(columna == col_actual & metodo == fallback_method)
        datos_mejor_modelo <- resultado_pronostico %>% filter(metodo == fallback_method & columna == col_actual)

        # Si no existen datos de MEAN, construir placeholder si es posible
        if (nrow(datos_mejor_modelo) == 0) {
          any_row <- resultado_pronostico %>% filter(columna == col_actual) %>% slice(1)
          if (nrow(any_row) == 0) {
            datos_mejor_modelo <- resultado_pronostico[0, , drop = FALSE]
          } else {
            any_row$metodo <- fallback_method
            datos_mejor_modelo <- any_row
          }
        }

        attr(datos_mejor_modelo, "metricas") <- if (nrow(metricas_mean) > 0) metricas_mean else NULL
        attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
        attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
        attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
        attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
        attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
        attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
        attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

        seleccion_columna <- list(
          columna = col_actual,
          mejor_modelo = fallback_method,
          metricas_mejor = if (nrow(metricas_mean) > 0) metricas_mean else tibble::tibble(),
          ranking_completo = metricas %>% filter(columna == col_actual),
          criterio_usado = paste("Fallback:", fallback_method),
          datos_mejor_modelo = datos_mejor_modelo
        )

        selecciones_por_columna[[col_actual]] <- seleccion_columna

        if (mostrar_analisis) {
          cat("Se seleccionó fallback", fallback_method, "para", col_actual, "\n")
        }

        # pasar a la siguiente columna
        next
      }

      # Continuar con el flujo normal si hay métodos válidos
      amplitud_intervalos <- resultado_pronostico %>%
        filter(tipo == "Pronóstico" & columna == col_actual & !metodo %in% excluir_metodos) %>%
        filter(!is.na(limite_inferior) & !is.na(limite_superior)) %>%
        group_by(metodo) %>%
        summarise(
          amplitud_promedio = mean(limite_superior - limite_inferior, na.rm = TRUE),
          amplitud_relativa = mean((limite_superior - limite_inferior) / abs(valor), na.rm = TRUE),
          .groups = "drop"
        )

      metricas_completas <- metricas_col %>%
        left_join(amplitud_intervalos, by = "metodo")

      metricas_normalizadas <- metricas_completas %>%
        mutate(
          rmse_norm = (RMSE - min(RMSE, na.rm = TRUE)) / (max(RMSE, na.rm = TRUE) - min(RMSE, na.rm = TRUE)),
          mape_norm = (MAPE - min(MAPE, na.rm = TRUE)) / (max(MAPE, na.rm = TRUE) - min(MAPE, na.rm = TRUE)),
          amplitud_norm = abs(amplitud_relativa - median(amplitud_relativa, na.rm = TRUE)) /
            (max(amplitud_relativa, na.rm = TRUE) - min(amplitud_relativa, na.rm = TRUE))
        ) %>%
        mutate(
          rmse_norm = ifelse(is.nan(rmse_norm), 0, rmse_norm),
          mape_norm = ifelse(is.nan(mape_norm), 0, mape_norm),
          amplitud_norm = ifelse(is.nan(amplitud_norm), 0, amplitud_norm)
        )

      metricas_puntuacion <- metricas_normalizadas %>%
        mutate(
          puntuacion_compuesta = peso_rmse * rmse_norm +
            peso_mape * mape_norm +
            peso_intervalo * amplitud_norm
        ) %>%
        arrange(puntuacion_compuesta)

      if (criterio_principal == "rmse") {
        mejor_modelo <- metricas_puntuacion %>% arrange(RMSE) %>% slice(1)
        criterio_usado <- "RMSE mínimo"
      } else if (criterio_principal == "mape") {
        mejor_modelo <- metricas_puntuacion %>% arrange(MAPE) %>% slice(1)
        criterio_usado <- "MAPE mínimo"
      } else if (criterio_principal == "compuesto") {
        mejor_modelo <- metricas_puntuacion %>% slice(1)
        criterio_usado <- "Puntuación compuesta"
      }

      mejor_metodo_nombre <- mejor_modelo$metodo
      datos_mejor_modelo <- resultado_pronostico %>%
        filter(metodo == mejor_metodo_nombre & columna == col_actual)

      attr(datos_mejor_modelo, "metricas") <- metricas %>% filter(metodo == mejor_metodo_nombre & columna == col_actual)
      attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
      attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
      attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
      attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
      attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
      attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
      attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

      seleccion_columna <- list(
        columna = col_actual,
        mejor_modelo = mejor_metodo_nombre,
        metricas_mejor = mejor_modelo,
        ranking_completo = metricas_puntuacion,
        criterio_usado = criterio_usado,
        datos_mejor_modelo = datos_mejor_modelo
      )

      selecciones_por_columna[[col_actual]] <- seleccion_columna

      if (mostrar_analisis) {
        cat("Mejor modelo para", col_actual, ":", mejor_metodo_nombre, "\n")
        cat("RMSE:", round(mejor_modelo$RMSE, 4), "| MAPE:", round(mejor_modelo$MAPE, 4), "%\n")
      }

    }, error = function(e) {
      # En caso de error inesperado, usar fallback_method
      warning(paste("Error al procesar columna", col_actual, ":", e$message, "- usando", fallback_method, "como fallback"))
      metricas_mean <- metricas %>% filter(columna == col_actual & metodo == fallback_method)
      datos_mejor_modelo <- resultado_pronostico %>% filter(metodo == fallback_method & columna == col_actual)

      if (nrow(datos_mejor_modelo) == 0) {
        any_row <- resultado_pronostico %>% filter(columna == col_actual) %>% slice(1)
        if (nrow(any_row) == 0) {
          datos_mejor_modelo <- resultado_pronostico[0, , drop = FALSE]
        } else {
          any_row$metodo <- fallback_method
          datos_mejor_modelo <- any_row
        }
      }

      attr(datos_mejor_modelo, "metricas") <- if (nrow(metricas_mean) > 0) metricas_mean else NULL
      attr(datos_mejor_modelo, "nivel_confianza") <- attr(resultado_pronostico, "nivel_confianza")
      attr(datos_mejor_modelo, "datos_originales") <- attr(resultado_pronostico, "datos_originales")
      attr(datos_mejor_modelo, "periodos_pronostico") <- attr(resultado_pronostico, "periodos_pronostico")
      attr(datos_mejor_modelo, "datos_historicos") <- attr(resultado_pronostico, "datos_historicos")
      attr(datos_mejor_modelo, "metodo_imputacion") <- attr(resultado_pronostico, "metodo_imputacion")
      attr(datos_mejor_modelo, "incluye_historicos") <- attr(resultado_pronostico, "incluye_historicos")
      attr(datos_mejor_modelo, "frecuencia") <- attr(resultado_pronostico, "frecuencia")

      seleccion_columna <- list(
        columna = col_actual,
        mejor_modelo = fallback_method,
        metricas_mejor = if (nrow(metricas_mean) > 0) metricas_mean else tibble::tibble(),
        ranking_completo = metricas %>% filter(columna == col_actual),
        criterio_usado = paste("Fallback por error:", fallback_method),
        datos_mejor_modelo = datos_mejor_modelo
      )

      selecciones_por_columna[[col_actual]] <- seleccion_columna
    })
  }

  if (length(selecciones_por_columna) == 1) {
    return(selecciones_por_columna[[1]])
  } else {
    return(selecciones_por_columna)
  }
}

PronSerie <- function(seleccion, columna = NULL) {
  # Si seleccion es una lista de múltiples columnas
  if (is.list(seleccion) && "columna" %in% names(seleccion)) {
    # Es una selección de una sola columna
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    metricas <- seleccion$metricas_mejor
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    # Es una lista de múltiples columnas, seleccionar una específica
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    metricas <- seleccion_col$metricas_mejor
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  historicos <- datos_mejor %>% filter(tipo == "Histórico")
  pronosticos <- datos_mejor %>% filter(tipo == "Pronóstico")

  pronosticos_clean <- pronosticos %>%
    filter(!is.na(limite_inferior), !is.na(limite_superior)) %>%
    mutate(hover_text = paste0("Límite inferior: ", round(limite_inferior, 2),
                               "<br>Límite superior: ", round(limite_superior, 2)))

  # Crear gráfico plotly
  subtitulo <- paste0("RMSE: ", round(metricas$RMSE, 2), " | MAPE: ",
                      format(round(metricas$MAPE, 2), big.mark = ",", decimal.mark = "."), " %")

  titulo_principal <- paste0("<b>Serie Pronosticada ", str_to_title(col_actual), " - Método: ", mejor_metodo, "</b><br>",
                             "<span style='font-size:14px; color:#7F8C8D'>", subtitulo, "</span>")

  p <- plot_ly() %>%
    add_ribbons(data = pronosticos_clean, x = ~fecha, ymin = ~limite_inferior,
                ymax = ~limite_superior, fillcolor = "rgba(231, 76, 60, 0.3)",
                line = list(color = "transparent"), name = "Intervalo de Confianza",
                text = ~hover_text,
                hovertemplate = paste0("<b>Intervalo de Confianza</b><br>",
                                       "%{text}",
                                       "<extra></extra>")) %>%
    add_lines(data = historicos, x = ~fecha, y = ~valor,
              line = list(color = "#2C3E50", width = 2), name = "Histórico",
              hovertemplate = paste0("<b>Datos Históricos</b><br>",
                                     "Valor: %{y:.2f}<br>",
                                     "<extra></extra>")) %>%
    add_trace(data = pronosticos, x = ~fecha, y = ~valor, type = "scatter",
              mode = "lines+markers", line = list(color = "#E74C3C", width = 2),
              marker = list(color = "#E74C3C", size = 5, symbol = "diamond"),
              name = "Pronóstico",
              hovertemplate = paste0("<b>Punto de Pronóstico</b><br>",
                                     "Valor: %{y:.2f}<br>",
                                     "<extra></extra>")) %>%
    layout(title = list(text = titulo_principal,
                        font = list(size = 18, color = "#2C3E50"), x = 0.5),
           xaxis = list(title = list(text = "Fecha", font = list(size = 14)),
                        showgrid = TRUE, gridcolor = "#E8E8E8", gridwidth = 1),
           yaxis = list(title = list(text = col_actual, font = list(size = 14)),
                        showgrid = TRUE, gridcolor = "#E8E8E8", gridwidth = 1),
           hovermode = "x unified", showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center", yanchor = "top"),
           plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 100)) %>%
    config(displayModeBar = FALSE)

  return(p)
}
PronMensual <- function(seleccion, columna = NULL, incluir_pronosticos = TRUE) {

  if (is.list(seleccion) && "columna" %in% names(seleccion)) {
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    metricas <- seleccion$metricas_mejor
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    #
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    metricas <- seleccion_col$metricas_mejor
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  # Preparar datos
  datos_preparados <- datos_mejor %>%
    mutate(mes = format(fecha, "%B"),
           mes_num = month(fecha),
           año = year(fecha),
           mes_abrev = format(fecha, "%b")) %>%
    arrange(fecha)

  if (incluir_pronosticos) {
    datos_grafico <- datos_preparados
  } else {
    datos_grafico <- datos_preparados %>% filter(tipo == "Histórico")
  }

  # Obtener años únicos para colores
  años_unicos <- sort(unique(datos_grafico$año))
  año_actual <- as.numeric(format(Sys.Date(), "%Y"))

  # Función para asignar colores según el año
  asignar_color_año <- function(años) {
    # Paletas de colores
    azules_verdes <- c("rgba(31, 78, 121, 0.7)",
                       "rgba(0, 128, 128, 0.6)",
                       "rgba(0, 168, 150, 0.55)",
                       "rgba(255, 193, 7, 0.5)",
                       "rgba(255, 87, 34, 0.5)",
                       "rgba(156, 39, 176, 0.6)",
                       "rgba(63, 81, 181, 0.6)",
                       "rgba(0, 188, 212, 0.5)",
                       "rgba(139, 195, 74, 0.5)",
                       "rgba(205, 220, 57, 0.5)",
                       "rgba(255, 202, 40, 0.5)" )

    rojos <- c("#8B0000", "#B22222")  # Rojo oscuro y rojo fuego
    negro <- c("#292526", "#3E3839")  # Negros

    colores_asignados <- character(length(años))

    años_anteriores <- años[años < año_actual]
    años_actuales <- años[años == año_actual]
    años_futuros <- años[años > año_actual]

    if (length(años_anteriores) > 0) {
      n_anteriores <- length(años_anteriores)
      colores_anteriores <- azules_verdes[1:min(n_anteriores, length(azules_verdes))]
      colores_asignados[años %in% años_anteriores] <- colores_anteriores
    }

    # Asignar rojo al año actual
    if (length(años_actuales) > 0) {
      colores_asignados[años %in% años_actuales] <- rojos[1]
    }

    # Asignar rojo al año futuro (siguiente)
    if (length(años_futuros) > 0) {
      colores_asignados[años %in% años_futuros] <- negro[1]
    }

    return(setNames(colores_asignados, as.character(años)))
  }

  colores_asignados <- asignar_color_año(años_unicos)

  meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

  # Crear gráfico base
  p <- plot_ly()

  # Agregar línea para cada año
  for (año in años_unicos) {
    datos_año <- datos_grafico %>% filter(año == !!año)

    if (any(datos_año$tipo == "Pronóstico")) {
      estilo_linea <- list(color = colores_asignados[as.character(año)], width = 3, dash = "dash")
      estilo_marcador <- list(color = colores_asignados[as.character(año)], size = 5, symbol = "diamond")
      nombre_año <- paste(año, "(Pronóstico)")
    } else {
      estilo_linea <- list(color = colores_asignados[as.character(año)], width = 2)
      estilo_marcador <- list(color = colores_asignados[as.character(año)], size = 4, symbol = "circle")
      nombre_año <- as.character(año)
    }

    p <- p %>%
      add_trace(data = datos_año, x = ~mes_num, y = ~valor, type = "scatter", mode = "lines+markers",
                line = estilo_linea, marker = estilo_marcador, name = nombre_año, customdata = ~mes,
                text = ~format(fecha, "%Y-%m-%d"),
                hovertemplate = paste("<b>", nombre_año, ": </b><br>",
                                      "Valor: %{y:.2f}<br>",
                                      "<extra></extra>"))
  }

  # Crear subtítulo con métricas
  subtitulo <- paste0("RMSE: ", round(metricas$RMSE, 2), " | MAPE: ",
                      format(round(metricas$MAPE, 2), big.mark = ",", decimal.mark = "."), " %")

  # Título principal con nombre de columna
  titulo_principal <- paste0("<b>Comparación Mensual por Año - ", col_actual, " - Método: ", mejor_metodo, "</b><br>",
                             "<span style='font-size:14px; color:#7F8C8D'>", subtitulo, "</span>")

  p <- p %>%
    layout(title = list(text = titulo_principal,
                        font = list(size = 18, color = "#2C3E50"), x = 0.5),
           xaxis = list(title = "", tickmode = "array", tickvals = 1:12, ticktext = meses_esp),
           yaxis = list(title = col_actual),
           hovermode = "x unified",
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center", yanchor = "top"),
           plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 100)) %>%
    config(displayModeBar = FALSE)

  return(p)
}
PronPatronMes <- function(seleccion, columna = NULL) {

  # Determinar si es una selección de una sola columna o múltiples
  if (is.list(seleccion) && "columna" %in% names(seleccion)) {
    # Es una selección de una sola columna
    datos_mejor <- seleccion$datos_mejor_modelo
    mejor_metodo <- seleccion$mejor_modelo
    col_actual <- seleccion$columna
  } else if (is.list(seleccion) && !is.null(columna)) {
    # Es una lista de múltiples columnas, seleccionar una específica
    if (!columna %in% names(seleccion)) {
      stop(paste("La columna", columna, "no existe en las selecciones"))
    }
    seleccion_col <- seleccion[[columna]]
    datos_mejor <- seleccion_col$datos_mejor_modelo
    mejor_metodo <- seleccion_col$mejor_modelo
    col_actual <- columna
  } else {
    stop("Debe especificar una columna cuando hay múltiples selecciones")
  }

  # Análisis solo de datos históricos
  historicos <- datos_mejor %>%
    filter(tipo == "Histórico") %>%
    mutate(mes = format(fecha, "%B"),
           mes_num = month(fecha),
           año = year(fecha))

  # Estadísticas por mes
  stats_mensuales <- historicos %>%
    group_by(mes, mes_num) %>%
    summarise(promedio = mean(valor, na.rm = TRUE),
              mediana = median(valor, na.rm = TRUE),
              desviacion = sd(valor, na.rm = TRUE),
              minimo = min(valor, na.rm = TRUE),
              maximo = max(valor, na.rm = TRUE),
              coef_variacion = sd(valor, na.rm = TRUE) / mean(valor, na.rm = TRUE) * 100,
              n_observaciones = n(),
              .groups = "drop") %>%
    arrange(mes_num)

  meses_esp <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

  p <- plot_ly() %>%
    add_lines(data = stats_mensuales, x = ~mes_num, y = ~promedio,
              line = list(color = "#2C3E50", width = 4),
              name = "Promedio", customdata = ~mes,
              hovertemplate = paste0("<b>Promedio</b><br>",
                                     "Valor: %{y:.2f}<br>",
                                     "<extra></extra>")) %>%
    add_ribbons(data = stats_mensuales, x = ~mes_num,
                ymin = ~(promedio - desviacion), ymax = ~(promedio + desviacion),
                fillcolor = "rgba(44, 62, 80, 0.2)",
                line = list(color = "transparent"), name = "± 1 Desv. Estándar",
                customdata = ~mes,
                hovertemplate = paste0("<b>Banda de Desviación</b><br>",
                                       "Promedio: %{y:.2f}<br>",
                                       "<extra></extra>")) %>%
    add_lines(data = stats_mensuales, x = ~mes_num, y = ~mediana,
              line = list(color = "#E74C3C", width = 2, dash = "dot"),
              name = "Mediana", customdata = ~mes,
              hovertemplate = paste0("<b>Mediana</b><br>",
                                     "Valor: %{y:.2f}<br>",
                                     "<extra></extra>")) %>%
    add_markers(data = stats_mensuales, x = ~mes_num, y = ~maximo,
                marker = list(color = "#27AE60", size = 8, symbol = "triangle-up"),
                name = "Máximo", customdata = ~mes,
                hovertemplate = paste0("<b>Máximo</b><br>",
                                       "Valor: %{y:.2f}<br>",
                                       "<extra></extra>")) %>%
    add_markers(data = stats_mensuales, x = ~mes_num, y = ~minimo,
                marker = list(color = "#E67E22", size = 8, symbol = "triangle-down"),
                name = "Mínimo", customdata = ~mes,
                hovertemplate = paste0("<b>Mínimo</b><br>",
                                       "Mes: %{customdata}<br>",
                                       "Valor: %{y:.2f}<br>",
                                       "<extra></extra>")) %>%
    layout(title = list(text = paste("<b>Patrón Estacional Histórico -", col_actual, "</b>"),
                        font = list(size = 18, color = "#2C3E50"), x = 0.5),
           xaxis = list(title = "", tickmode = "array", tickvals = 1:12, ticktext = meses_esp),
           yaxis = list(title = col_actual),
           hovermode = "x unified",
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, y = -0.1, xanchor = "center", yanchor = "top"),
           plot_bgcolor = "white", paper_bgcolor = "white",
           margin = list(t = 100)) %>%
    config(displayModeBar = FALSE)

  # Mostrar estadísticas
  cat("=== ESTADÍSTICAS MENSUALES PARA", col_actual, "===\n")
  print(stats_mensuales)

  return(p)
}
