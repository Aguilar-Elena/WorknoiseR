(function(global){
  'use strict';

  const pairs = [
    ["¿Qué desea realizar?", "What would you like to do?"],
    ["Seleccione el módulo de trabajo. Ambos forman parte de la misma aplicación y comparten la identidad WorknoiseR.", "Select a work module. Both are part of the same application and share the WorknoiseR identity."],
    ["Cálculos de ruido", "Noise calculations"],
    ["Evaluación de exposición, LAeq,T, LAeq,d, exposición semanal, dosis, TMA, bandas de octava, protectores auditivos y campañas masivas de trabajadores.", "Exposure assessment, LAeq,T, LEX,8h, weekly exposure, dose, maximum allowable time, octave bands, hearing protectors and bulk worker campaigns."],
    ["Entrar en Cálculos", "Open Calculations"],
    ["Mapas de ruido", "Noise maps"],
    ["Diseño o importación de planos, DWG/DXF, geometría espacial, fuentes, receptores, paredes, reverberación, mapas 2D, isófonas y representación 3D.", "Plan design or import, DWG/DXF, spatial geometry, sources, receivers, walls, reverberation, 2D maps, contours and 3D representation."],
    ["Entrar en Mapas", "Open Maps"],
    ["WorknoiseR integra cálculo higiénico y modelización espacial en una única interfaz Shiny.", "WorknoiseR integrates occupational-hygiene calculations and spatial modelling in a single Shiny interface."],
    ["← Inicio", "← Home"],
    ["WorknoiseR · Cálculos de ruido", "WorknoiseR · Noise calculations"],
    ["WorknoiseR · Mapas de ruido", "WorknoiseR · Noise maps"],
    ["Inicio", "Home"],
    ["Cálculos", "Calculations"],
    ["Mapas", "Maps"],

    ["Cálculo de niveles sonoros", "Sound-level calculations"],
    ["¿Deseas introducir Lp por bandas de octava?", "Do you want to enter Lp by octave band?"],
    ["Número de fuentes sonoras:", "Number of sound sources:"],
    ["Introduce el Lp por bandas de octava (dB)", "Enter Lp by octave band (dB)"],
    ["Horas de exposición diaria (para LAeq,D):", "Daily exposure hours (for LEX,8h):"],
    ["Nivel de presión acústica de pico LCpico [dB(C)]:", "Peak sound pressure level Lpeak,C [dB(C)]:"],
    ["¿Deseas calcular la jornada por tareas/tramos de exposición?", "Do you want to calculate the workday by tasks/exposure periods?"],
    ["Cada fila representa una tarea o tramo con su LAeq,T y duración. El LAeq,d conjunto se calcula por suma energética y normalización a 8 h.", "Each row represents a task or exposure period with its LAeq,T and duration. The combined daily noise exposure LEX,8h is calculated by energetic summation and normalisation to 8 h."],
    ["Importar CSV de tramos (Tarea, LAeq_T_dBA, Duracion_h):", "Import task-period CSV (Task, LAeq_T_dBA, Duration_h):"],
    ["¿Deseas calcular dosis de ruido y TMA?", "Do you want to calculate noise dose and maximum allowable time?"],
    ["Criterio de dosis:", "Dose criterion:"],
    ["Personalizado", "Custom"],
    ["Nivel criterio [dB(A)]:", "Criterion level [dB(A)]:"],
    ["Tasa de intercambio [dB]:", "Exchange rate [dB]:"],
    ["Tiempo de referencia [h]:", "Reference time [h]:"],
    ["Umbral de integración [dB(A)]:", "Integration threshold [dB(A)]:"],
    ["La dosis es una métrica configurable. Los presets NIOSH/OSHA no constituyen los valores de acción del RD 286/2006.", "Noise dose is configurable. NIOSH/OSHA presets are not the exposure action values established by Directive 2003/10/EC."],
    ["¿Deseas calcular la atenuación con protectores auditivos?", "Do you want to calculate hearing-protector attenuation?"],
    ["Método de cálculo:", "Calculation method:"],
    ["Introduce los valores del protector auditivo (mf y sf)", "Enter the hearing protector values (mf and sf)"],
    ["El espectro ambiental incluye 31,5–8000 Hz. El método APV utiliza las bandas 63–8000 Hz declaradas habitualmente por el fabricante.", "The ambient spectrum includes 31.5–8000 Hz. The APV method uses the 63–8000 Hz bands commonly declared by the manufacturer."],
    ["H (alta frecuencia) [dB]", "H (high frequency) [dB]"],
    ["M (media frecuencia) [dB]", "M (medium frequency) [dB]"],
    ["L (baja frecuencia) [dB]", "L (low frequency) [dB]"],
    ["Valor SNR [dB]", "SNR value [dB]"],
    ["Corrección/derating adicional definido por el usuario [dB]:", "Additional user-defined correction/derating [dB]:"],
    ["Por defecto = 0 dB. Esta corrección no forma parte del cálculo SNR básico; solo se aplica si el usuario decide introducirla.", "Default = 0 dB. This correction is not part of the basic SNR calculation; it is applied only if the user chooses to enter it."],
    ["LCeq,T medido [dB(C)] (opcional si se usan bandas de octava):", "Measured LCeq,T [dB(C)] (optional when octave bands are used):"],
    ["LCeq,T y LCpico son magnitudes distintas. El nivel de pico no se utiliza como sustituto de LCeq,T.", "LCeq,T and Lpeak,C are different quantities. Peak level is not used as a substitute for LCeq,T."],
    ["¿Deseas calcular exposición semanal?", "Do you want to calculate weekly exposure?"],
    ["Días con exposición:", "Exposure days:"],
    ["Lunes", "Monday"], ["Martes", "Tuesday"], ["Miércoles", "Wednesday"], ["Jueves", "Thursday"], ["Viernes", "Friday"], ["Sábado", "Saturday"], ["Domingo", "Sunday"],
    ["Puede seleccionar entre 1 y 7 días. Se muestran dos resultados: LAeq,s normativo (referencia fija de 5 jornadas) y media energética de los días realmente expuestos.", "You may select 1 to 7 days. Two results are shown: weekly noise exposure LEX,w (fixed reference of five workdays) and the energetic mean of the days actually exposed. Under Directive 2003/10/EC, weekly exposure may replace daily exposure only in the circumstances specified by Article 3(3)."],
    ["Importar CSV semanal (Dia, LAeq_d_dBA):", "Import weekly CSV (Day, LAeq_d_dBA):"],
    ["Importación masiva de trabajadores/campañas", "Bulk worker/campaign import"],
    ["Subir Excel/CSV de trabajadores y exposiciones:", "Upload worker and exposure Excel/CSV:"],
    ["Descargar plantilla Excel", "Download Excel template"],
    ["La plantilla es recomendada, no obligatoria. Si usa un Excel propio podrá mapear las columnas manualmente.", "The template is recommended, not mandatory. If you use your own Excel file, you can map the columns manually."],
    ["Procesar campaña importada", "Process imported campaign"],
    ["Calcular", "Calculate"],
    ["Importación masiva", "Bulk import"],
    ["Descargar resultados procesados (Excel)", "Download processed results (Excel)"],
    ["Diagnóstico de datos importados", "Imported-data diagnostics"],
    ["Advertencias y datos no calculables", "Warnings and non-calculable data"],
    ["Resultados por trabajador", "Results by worker"],
    ["Resultados diarios por trabajador", "Daily results by worker"],
    ["Vista previa de exposiciones normalizadas", "Preview of normalized exposures"],
    ["Resultados", "Results"],
    ["Conformidad RD 286/2006", "Directive 2003/10/EC compliance"],
    ["Perfil de exposición", "Exposure profile"],
    ["El perfil muestra los LAeq,T de cada tramo y el LAeq,d acumulado. Las líneas de 80/85/87 dB(A) se aplican al LAeq,d acumulado, no al nivel aislado de cada tramo.", "The profile shows LAeq,T for each period and cumulative daily noise exposure LEX,8h. The 80/85/87 dB(A) lines apply to cumulative LEX,8h, not to the isolated level of each period."],
    ["Bandas de octava", "Octave bands"],
    ["Gráfico de bandas de octava", "Octave-band chart"],
    ["Correspondencia de columnas", "Column mapping"],
    ["La aplicación intenta reconocerlas automáticamente. Revise las asignaciones antes de procesar.", "The application attempts to recognize them automatically. Review the assignments before processing."],
    ["— No asignar —", "— Do not assign —"],
    ["ID trabajador", "Worker ID"],
    ["Nombre/identificador descriptivo", "Name/descriptive identifier"],
    ["Puesto", "Job"],
    ["Zona/área", "Zone/area"],
    ["Tarea/actividad", "Task/activity"],
    ["Día/fecha de exposición", "Exposure day/date"],
    ["Tiempo de exposición [h]", "Exposure time [h]"],
    ["LAeq,d [dB(A)] ya disponible (opcional)", "Daily exposure LEX,8h [dB(A)] already available (optional; LAeq_d column supported)"],
    ["Coordenada X", "X coordinate"], ["Coordenada Y", "Y coordinate"],
    ["ID/modelo protector", "Protector ID/model"],
    ["Derating SNR opcional [dB]", "Optional SNR derating [dB]"],
    ["Hoja/tabla principal de exposiciones:", "Main exposure sheet/table:"],

    ["Generador de mapas de ruido laboral", "Occupational noise map generator"],
    ["Configuración, modelización espacial y evaluación de exposición en una única interfaz.", "Configuration, spatial modelling and exposure assessment in a single interface."],
    ["↻ Nuevo mapa / reiniciar", "↻ New map / reset"],
    ["Programación", "Setup"],
    ["defina primero la geometría, después las fuentes y receptores, y finalmente las condiciones de propagación. Los resultados se consultan en las pestañas superiores.", "define the geometry first, then sources and receivers, and finally propagation conditions. Results are available in the upper tabs."],
    ["1. Plano y geometría", "1. Plan and geometry"],
    ["Origen del plano", "Plan source"],
    ["Seleccione una única vía de definición. La aplicación mostrará solo los controles necesarios.", "Choose one definition method. The application will show only the controls required."],
    ["Diseño manual / nave básica", "Manual design / basic room"],
    ["Usar plano importado", "Use imported plan"],
    ["Modo interno de importación:", "Internal import mode:"],
    ["Automático", "Automatic"],
    ["Diseño manual", "Manual design"],
    ["Use esta opción para una nave rectangular o para definir manualmente un contorno mediante coordenadas.", "Use this option for a rectangular room or to define a custom outline with coordinates."],
    ["Largo de la nave (m):", "Room length (m):"],
    ["Ancho de la nave (m):", "Room width (m):"],
    ["Definir contorno personalizado", "Define custom outline"],
    ["Introduzca los vértices del contorno. No es necesario repetir manualmente el primer punto al final.", "Enter the outline vertices. You do not need to repeat the first point at the end."],
    ["Subir CSV con coordenadas x, y:", "Upload CSV with x, y coordinates:"],
    ["Carga del plano", "Plan upload"],
    ["Importación vectorial para conservar capas y geometrías CAD/GIS.", "Vector import to preserve CAD/GIS layers and geometries."],
    ["Importe coordenadas x/y o segmentos x1/y1/x2/y2. En Excel, cada hoja puede convertirse en una capa.", "Import x/y coordinates or x1/y1/x2/y2 segments. In Excel, each sheet can become a layer."],
    ["Use un plano como fondo y ajuste su escala mediante dimensiones de nave o calibración con dos puntos.", "Use a plan as a background and adjust its scale using room dimensions or two-point calibration."],
    ["Seleccione el archivo:", "Select file:"],
    ["Importar y preparar plano", "Import and prepare plan"],
    ["Elementos auxiliares", "Auxiliary elements"],
    ["Indicar una puerta de referencia", "Indicate a reference door"],
    ["Zonas de oficina", "Office zones"],
    ["Definir zona de oficinas", "Define office zone"],
    ["X desde:", "X from:"], ["X hasta:", "X to:"], ["Y desde:", "Y from:"], ["Y hasta:", "Y to:"],
    ["2. Fuentes", "2. Sources"],
    ["Fuentes sonoras / máquinas", "Sound sources / machines"],
    ["Número de máquinas:", "Number of machines:"],
    ["Cerramientos de máquinas", "Machine enclosures"],
    ["Aplicar cerramiento a una o varias máquinas", "Apply enclosure to one or more machines"],
    ["El cerramiento de una máquina modifica su emisión antes de calcular la propagación espacial.", "A machine enclosure modifies its emission before spatial propagation is calculated."],
    ["3. Receptores", "3. Receivers"],
    ["Trabajadores / receptores", "Workers / receivers"],
    ["Incluir trabajadores", "Include workers"],
    ["Número de trabajadores:", "Number of workers:"],
    ["Protección auditiva", "Hearing protection"],
    ["Evaluar protección auditiva mediante SNR", "Evaluate hearing protection using SNR"],
    ["SNR del protector [dB]:", "Protector SNR [dB]:"],
    ["Derating adicional definido por el usuario [dB]:", "Additional user-defined derating [dB]:"],
    ["SNR requiere LCeq,T. El derating es opcional y por defecto vale 0 dB.", "SNR requires LCeq,T. Derating is optional and defaults to 0 dB."],
    ["4. Propagación", "4. Propagation"],
    ["Campo reverberante", "Reverberant field"],
    ["Incluir campo reverberante", "Include reverberant field"],
    ["Modelo de reverberación:", "Reverberation model:"],
    ["Por recinto aproximado (recomendado)", "Approximate room model (recommended)"],
    ["Global sin compartimentación (comparación)", "Global without compartmentation (comparison)"],
    ["Coeficiente medio de absorción del recinto (α):", "Mean room absorption coefficient (α):"],
    ["Muy reflectante (α ≈ 0,03)", "Highly reflective (α ≈ 0.03)"],
    ["Reflectante (α ≈ 0,05)", "Reflective (α ≈ 0.05)"],
    ["Madera barnizada / absorción baja (α ≈ 0,08)", "Varnished wood / low absorption (α ≈ 0.08)"],
    ["Superficie mixta / absorción media (α ≈ 0,25)", "Mixed surface / medium absorption (α ≈ 0.25)"],
    ["Absorbente (α ≈ 0,50)", "Absorptive (α ≈ 0.50)"],
    ["Muy absorbente (α ≈ 0,75)", "Highly absorptive (α ≈ 0.75)"],
    ["Tratamiento poroso intenso (α ≈ 0,80)", "Intensive porous treatment (α ≈ 0.80)"],
    ["Altura del techo (m):", "Ceiling height (m):"],
    ["En el modo por recinto, el campo reverberante se limita al recinto aproximado de la fuente. La transmisión entre recintos se trata por separado.", "In room mode, the reverberant field is limited to the approximate room containing the source. Transmission between rooms is handled separately."],
    ["Criterios de representación", "Display criteria"],
    ["Nivel objetivo para distancia teórica en campo libre [dB(A)]:", "Target level for theoretical free-field distance [dB(A)]:"],
    ["Las propiedades de paredes y barreras procedentes de un plano vectorial se configuran en la sección avanzada que aparece tras importarlo.", "Wall and barrier properties from a vector plan are configured in the advanced section shown after import."],
    ["La distancia teórica es un resultado de campo libre y se presenta separada del modelo interior con cerramientos.", "The theoretical distance is a free-field result and is presented separately from the indoor model with enclosures."],
    ["5. Calibración", "5. Calibration"],
    ["Estimación de niveles de fuente a partir de mediciones", "Source-level estimation from measurements"],
    ["Estimar Lᵢ a partir de mediciones", "Estimate Lᵢ from measurements"],
    ["Introduzca las coordenadas de medición y el nivel Lp observado.", "Enter measurement coordinates and the observed Lp level."],
    ["Estimar niveles Lᵢ", "Estimate Lᵢ levels"],
    ["Plano", "Plan"],
    ["Mapa 2D", "2D map"],
    ["Mapa con isófonas", "Contour map"],
    ["Mapa 3D", "3D map"],
    ["Trabajadores", "Workers"],
    ["Distancias", "Distances"],
    ["Sin plano importado.", "No plan imported."],
    ["Listo para cargar un nuevo plano.", "Ready to load a new plan."],
    ["Crear un nuevo mapa", "Create a new map"],
    ["Se eliminará el plano actualmente importado, su selección de capas, la calibración y el encuadre.", "The currently imported plan, selected layers, calibration and framing will be removed."],
    ["También se reiniciarán reverberación, barreras acústicas y estimación inversa, ya que dependen del recinto.", "Reverberation, acoustic barriers and inverse estimation will also be reset because they depend on the room."],
    ["Se conservarán las fuentes sonoras y los trabajadores", "Sound sources and workers will be retained"],
    ["Reiniciar y cargar otro plano", "Reset and load another plan"],
    ["Cancelar", "Cancel"],

    ["Nivel ambiental principal: no calculado", "Main ambient level: not calculated"],
    ["no es necesario para el cálculo por tramos/semanal", "not required for task-period/weekly calculation"],
    ["LAeq,d simple: no calculado", "Daily noise exposure LEX,8h: not calculated"],
    ["se requiere 0 < tiempo <= 24 h", "0 < time <= 24 h is required"],
    ["Jornada por tareas/tramos:", "Workday by tasks/exposure periods:"],
    ["Duración total incluida:", "Total included duration:"],
    ["LAeq,d conjunto de la jornada:", "Combined daily noise exposure LEX,8h:"],
    ["no hay filas válidas", "no valid rows"],
    ["LCpico: no indicado. No se estima a partir de LCeq,T ni de las bandas.", "Lpeak,C: not provided. It is not estimated from LCeq,T or octave bands."],
    ["Exposición semanal:", "Weekly exposure:"],
    ["Días con LAeq,d válido:", "Days with valid LEX,8h:"],
    ["LAeq,s normativo (referencia fija 5 jornadas):", "Weekly noise exposure LEX,w (fixed five-workday reference):"],
    ["Media energética de los días realmente expuestos:", "Energetic mean of days actually exposed:"],
    ["Nota: el segundo valor es descriptivo; no sustituye al LAeq,s definido por el RD 286/2006.", "Note: the energetic mean of exposed days is descriptive. For regulatory assessment, Directive 2003/10/EC defines LEX,w over a nominal week of five eight-hour working days and permits weekly exposure to replace daily exposure only under Article 3(3)."],
    ["Dosis/TMA: parámetros de criterio no válidos.", "Dose/maximum allowable time: invalid criterion parameters."],
    ["Dosis de ruido / TMA", "Noise dose / maximum allowable time"],
    ["Dosis acumulada:", "Accumulated dose:"],
    ["Nota: TMA significa tiempo máximo según el criterio seleccionado; no se etiqueta como 'tiempo seguro' universal.", "Note: maximum allowable time means the maximum time under the selected criterion; it is not labelled as a universal 'safe time'."],
    ["Evaluación RD 286/2006:", "Directive 2003/10/EC assessment:"],
    ["Evaluación RD 286/2006: no disponible sin LAeq,d válido.", "Directive 2003/10/EC assessment: unavailable without a valid daily noise exposure level."],
    ["Valor superior de acción alcanzado/superado", "Upper action value reached/exceeded"],
    ["Valor inferior de acción alcanzado/superado", "Lower action value reached/exceeded"],
    ["Por debajo del valor inferior de acción", "Below the lower action value"],
    ["Exposición real usada:", "Actual exposure used:"],
    ["No evaluado", "Not evaluated"],
    ["No supera", "Does not exceed"],
    ["Supera", "Exceeds"],
    ["Archivo cargado:", "File loaded:"],
    ["hoja(s)/tabla(s)", "sheet(s)/table(s)"],
    ["Sin campaña procesada", "No campaign processed"],
    ["No hay trabajadores con ID suficiente para resumir.", "There are no workers with sufficient ID information to summarize."],
    ["No hay jornadas calculables.", "There are no calculable workdays."],
    ["Error al resumir la campaña:", "Error summarizing campaign:"],
    ["No se han detectado advertencias automáticas relevantes.", "No relevant automatic warnings were detected."],
    ["Hay registros sin identificador de trabajador.", "Some records have no worker identifier."],
    ["Hay registros sin duración válida: no podrán contribuir al LAeq,d/dosis.", "Some records have no valid duration and cannot contribute to LEX,8h/dose."],
    ["Registros de exposición", "Exposure records"],
    ["Registros con duración válida", "Records with valid duration"],
    ["Estimación realizada con reverberación activada", "Estimation performed with reverberation enabled"],
    ["modelo:", "model:"],
    ["Distancia teórica campo libre [m]", "Theoretical free-field distance [m]"],
    ["Estimación de campo libre; no incorpora reflexiones, reverberación, barreras ni suma con otras fuentes. El mapa sí puede incorporar barreras CAD.", "Free-field estimate; it does not include reflections, reverberation, barriers or summation with other sources. The map may include CAD barriers."],
    ["Trabajador", "Worker"],
    ["Máquina", "Machine"],
    ["Fuente", "Source"],
    ["Nivel estimado", "Estimated level"],
    ["Advertencia", "Warning"],
    ["Indicador", "Indicator"],
    ["Valor", "Value"],
    ["Magnitud", "Magnitude"],
    ["Resultado", "Result"],

    ["Mapa reiniciado. Ya puede seleccionar e importar un nuevo plano.", "Map reset. You can now select and import a new plan."],
    ["La visualización del plano es independiente del modelo acústico.", "Plan visualization is independent of the acoustic model."],
    ["Todo el DWG/plano, incluidos outliers", "Entire DWG/plan, including outliers"],
    ["Restaurar plano completo", "Restore full plan"],
    ["Usar selección rectangular", "Use rectangular selection"],
    ["Capa a usar como contorno acústico:", "Layer to use as acoustic outline:"],
    ["Capas que actúan como barrera:", "Layers acting as barriers:"],
    ["Modelo de propagación con cerramientos:", "Propagation model with enclosures:"],
    ["Cruce recto rápido (modelo v13.8)", "Fast straight crossing (v13.8 model)"],
    ["Pérdida modelizada del primer cerramiento [dB]:", "Modelled loss for first enclosure [dB]:"],
    ["Pérdida por cerramiento adicional [dB]:", "Loss per additional enclosure [dB]:"],
    ["Pérdida máxima acumulada por transmisión [dB]:", "Maximum accumulated transmission loss [dB]:"],
    ["Suavizado visual energético (no altera los cálculos)", "Energetic visual smoothing (does not alter calculations)"],
    ["El modelo híbrido suma transmisión a través del cerramiento y una contribución que rodea sus bordes, evitando sombras geométricas excesivamente bruscas.", "The hybrid model combines transmission through the enclosure with an edge-routing contribution, avoiding excessively abrupt geometric shadows."],
    ["El suavizado se aplica solo a la representación gráfica; trabajadores, tablas y cálculos conservan el campo sin suavizar.", "Smoothing is applied only to the graphical representation; workers, tables and calculations retain the unsmoothed field."],
    ["Los parámetros son de modelización y no equivalen automáticamente a Rw/R de un cerramiento.", "These are modelling parameters and do not automatically correspond to the Rw/R of an enclosure."],
    ["Seleccione / confirme unidades", "Select / confirm units"],
    ["Unidades del plano por metro:", "Plan units per metre:"],
    ["Actualizar largo/ancho según el plano", "Update length/width from plan"],
    ["Usar la capa seleccionada como dominio de cálculo", "Use selected layer as calculation domain"],
    ["Interpretación del contorno:", "Outline interpretation:"],
    ["Rectángulo envolvente (bbox)", "Bounding rectangle (bbox)"],
    ["Escala del plano de fondo:", "Background-plan scale:"],
    ["Distancia real entre los dos puntos (m):", "Real distance between the two points (m):"],
    ["Opacidad del plano:", "Plan opacity:"],
    ["Plano importado", "Imported plan"],
    ["Capas del plano", "Plan layers"],
    ["Barrera acústica", "Acoustic barrier"],
    ["Barreras acústicas", "Acoustic barriers"],
    ["Contorno programado manualmente", "Manually programmed outline"],
    ["Contorno personalizado/CSV", "Custom outline/CSV"],
    ["El contorno necesita al menos tres vértices válidos.", "The outline requires at least three valid vertices."],
    ["Las mediciones contienen datos no válidos.", "Measurements contain invalid data."],
    ["Niveles estimados Lᵢ a 1 m [dB(A)]:", "Estimated Lᵢ levels at 1 m [dB(A)]:"],
    ["Error cuadrático total:", "Total squared error:"],
    ["El dominio de cálculo no tiene dimensiones válidas.", "The calculation domain has invalid dimensions."],
    ["No se ha podido generar la malla de cálculo.", "The calculation grid could not be generated."],
    ["La malla necesita al menos dos coordenadas X e Y distintas para el mapa 3D.", "The grid requires at least two distinct X and Y coordinates for the 3D map."],
    ["La hoja de exposiciones no contiene filas.", "The exposure sheet contains no rows."],
    ["No se encontraron días/niveles semanales válidos.", "No valid weekly days/levels were found."],
    ["Lp de la fuente", "Source Lp"],
    ["Nivel sonoro (dB)", "Sound level (dB)"],
    ["Tiempo acumulado (h)", "Accumulated time (h)"],
    ["Nivel [dB(A)]", "Level [dB(A)]"],
    ["Faltan datos válidos en la tabla de bandas de octava.", "Valid data are missing from the octave-band table."],
    ["No se han introducido niveles válidos para todas las fuentes.", "Valid levels have not been entered for all sources."],
    ["LAeq,d (nivel actual normalizado a 8 h):", "Daily noise exposure LEX,8h (normalized to 8 h):"],
    ["No se han introducido valores diarios LAeq,d válidos.", "No valid daily LEX,8h values have been entered."],
    ["Método APV: falta la tabla de datos del protector.", "APV method: protector data table is missing."],
    ["Método APV: no se han podido emparejar todas las frecuencias del protector.", "APV method: not all protector frequencies could be matched."],
    ["Nota: la banda ambiental de 31,5 Hz se incluye en LAeq,T/LCeq,T, pero no en APV si el protector no aporta mf/sf a 31,5 Hz.", "Note: the 31.5 Hz ambient band is included in LAeq,T/LCeq,T, but not in APV if the protector does not provide mf/sf at 31.5 Hz."],
    ["Método SNR: falta el valor SNR del protector.", "SNR method: protector SNR value is missing."],
    ["L'Aeq,d estimado con protector para el tiempo indicado:", "Estimated protected daily exposure L'EX,8h for the specified time:"],
    ["Nota: este L'Aeq,d protegido corresponde al nivel/tiempo principal, no a la jornada multitramos completa.", "Note: this protected daily exposure corresponds to the main level/time, not to the complete multi-period workday."],
    ["Nivel criterio:", "Criterion level:"],
    ["No hay niveles y duraciones válidos para calcular la dosis.", "There are no valid levels and durations for dose calculation."],
    ["Nota: con jornada multitramos no se aplica automáticamente la atenuación de un único cálculo de protector a todos los tramos.", "Note: for a multi-period workday, attenuation from a single protector calculation is not automatically applied to all periods."],
    ["LAeq,d calculado difiere", "Calculated LEX,8h differs"],
    ["del LAeq,d importado; revisar datos/criterio de cálculo.", "from the imported daily exposure value; review data/calculation criterion."],
    ["duración importada", "imported duration"],
    ["frente a jornada declarada", "versus declared workday"],
    ["Faltan paquetes obligatorios para ejecutar WorknoiseR:", "Required packages are missing to run WorknoiseR:"],
    ["Faltan paquetes obligatorios para ejecutar la aplicación:", "Required packages are missing to run the application:"],
    ["Instálelos con:", "Install them with:"],
    ["No se pudo cargar el módulo de cálculos de ruido.", "The noise-calculation module could not be loaded."],
    ["No se pudo cargar el módulo de mapas de ruido.", "The noise-mapping module could not be loaded."],
    ["Pulse 'Calcular' para evaluar la conformidad.", "Click 'Calculate' to assess compliance."],
    ["Faltan valores mf o sf.", "mf or sf values are missing."],
    ["Método APV (APV = mf - sf; eficacia asumida 84 %):", "APV method (APV = mf - sf; assumed effectiveness 84%):"],
    ["L'Aeq,T (protección APV, bandas 63-8000 Hz):", "Protected L'Aeq,T (APV, 63-8000 Hz bands):"],
    ["Método HML: se requiere LCeq,T medido o calculable a partir de bandas de octava.", "HML method: measured LCeq,T or a value calculable from octave bands is required."],
    ["Método HML: faltan H, M o L.", "HML method: H, M or L is missing."],
    ["Método HML:", "HML method:"],
    ["L'Aeq,T (protección HML):", "Protected L'Aeq,T (HML):"],
    ["Método SNR: se requiere LCeq,T medido o calculable a partir de bandas de octava.", "SNR method: measured LCeq,T or a value calculable from octave bands is required."],
    ["Método SNR: el derating no puede ser mayor que el SNR.", "SNR method: derating cannot exceed the SNR value."],
    ["Método SNR:", "SNR method:"],
    ["Derating adicional definido por el usuario =", "Additional user-defined derating ="],
    ["Jornada por tareas/tramos:", "Workday by tasks/exposure periods:"],
    ["Duración total incluida:", "Total included duration:"],
    ["LAeq,d simple: no calculado (se requiere 0 < tiempo <= 24 h).", "Daily noise exposure LEX,8h not calculated (0 < time <= 24 h is required)."],
    ["Tabla por bandas de octava:", "Octave-band table:"],
    ["LAeq,T calculado a partir de las bandas:", "LAeq,T calculated from octave bands:"],
    ["LCeq,T calculado a partir de las bandas:", "LCeq,T calculated from octave bands:"],
    ["Nivel ambiental principal: no calculado (no es necesario para el cálculo por tramos/semanal).", "Main ambient level: not calculated (not required for multi-period/weekly assessment)."],
    ["LAeq,T (suma energética de fuentes):", "LAeq,T (energetic sum of sources):"],
    ["LCpico indicado:", "Lpeak,C provided:"],
    ["No se pudo leer el CSV semanal.", "The weekly CSV file could not be read."],
    ["El CSV semanal debe contener Dia y LAeq_d_dBA.", "The weekly CSV must contain Dia and LAeq_d_dBA."],
    ["Datos semanales importados. Pulse Calcular.", "Weekly data imported. Click Calculate."],
    ["No se pudo leer el CSV de tramos.", "The exposure-period CSV file could not be read."],
    ["El CSV debe contener las columnas: Tarea, LAeq_T_dBA, Duracion_h.", "The CSV must contain the columns: Tarea, LAeq_T_dBA, Duracion_h."],
    ["Tramos importados. Pulse Calcular para obtener el LAeq,d de la jornada.", "Exposure periods imported. Click Calculate to obtain the workday LEX,8h."],
    ["Seleccione primero una ventana rectangular sobre el plano.", "First select a rectangular window on the plan."],
    ["No se pudo leer el archivo CSV.", "The CSV file could not be read."],
    ["El CSV debe tener columnas 'x' e 'y'.", "The CSV must contain 'x' and 'y' columns."],
    ["Para una estimación estable se recomiendan al menos tantos puntos de medición como fuentes.", "For a stable estimate, at least as many measurement points as sources are recommended."]

  ];

  const esToEn = new Map(pairs);
  const enToEs = new Map(pairs.map(p => [p[1], p[0]]));
  let lang = 'es';
  let busy = false;
  let observer = null;

  // Keep the text supplied by Shiny as the canonical source for every text
  // node. Language changes are always rendered from that source, never from a
  // previous translation. This makes ES -> EN -> ES fully reversible.
  const textState = new WeakMap();
  const attrState = new WeakMap();

  function ordered(map){
    return Array.from(map.entries()).sort((a,b) => b[0].length - a[0].length);
  }

  function isLetterOrDigit(ch){
    if (!ch) return false;
    try { return /[\p{L}\p{N}_]/u.test(ch); }
    catch(e) { return /[A-Za-z0-9_ÁÉÍÓÚÜÑáéíóúüñ]/.test(ch); }
  }

  function replaceSafe(text, from, token){
    if (!text || !from || text.indexOf(from) === -1) return text;
    let out = '';
    let pos = 0;
    while (pos < text.length) {
      const idx = text.indexOf(from, pos);
      if (idx === -1) { out += text.slice(pos); break; }
      const before = idx > 0 ? text[idx - 1] : '';
      const afterIdx = idx + from.length;
      const after = afterIdx < text.length ? text[afterIdx] : '';
      const leftOK = !isLetterOrDigit(from[0]) || !isLetterOrDigit(before);
      const rightOK = !isLetterOrDigit(from[from.length - 1]) || !isLetterOrDigit(after);
      if (leftOK && rightOK) {
        out += text.slice(pos, idx) + token;
        pos = afterIdx;
      } else {
        out += text.slice(pos, idx + 1);
        pos = idx + 1;
      }
    }
    return out;
  }

  function convertText(text, target){
    if (!text || !text.trim()) return text;
    const map = target === 'en' ? esToEn : enToEs;
    const entries = ordered(map);
    let out = text;
    const replacements = [];

    // First replace source phrases with opaque tokens. The translated text is
    // inserted only after all matching has finished, preventing cascades.
    entries.forEach(([from, to]) => {
      if (out.indexOf(from) === -1) return;
      const token = `__WORKNOISER_I18N_${replacements.length}__`;
      const next = replaceSafe(out, from, token);
      if (next !== out) {
        out = next;
        replacements.push([token, to]);
      }
    });

    replacements.forEach(([token, value]) => {
      out = out.split(token).join(value);
    });
    return out;
  }

  function sourceForTextNode(node){
    const current = node.nodeValue || '';
    let state = textState.get(node);
    if (!state) {
      state = {source: current, rendered: current};
      textState.set(node, state);
      return state;
    }
    // If Shiny (or another external script) has changed the text since our
    // last render, accept the new value as the new canonical source.
    if (current !== state.rendered) {
      state.source = current;
      state.rendered = current;
    }
    return state;
  }

  function sourceForAttr(node, attr){
    let state = attrState.get(node);
    if (!state) {
      state = {};
      attrState.set(node, state);
    }
    const current = node.getAttribute(attr);
    const rec = state[attr];
    if (!rec || current !== rec.rendered) {
      state[attr] = {source: current, rendered: current};
    }
    return state[attr];
  }

  function translateNode(node, target){
    if (!node) return;
    if (node.nodeType === Node.TEXT_NODE) {
      const state = sourceForTextNode(node);
      const next = convertText(state.source, target);
      if (next !== node.nodeValue) node.nodeValue = next;
      state.rendered = next;
      return;
    }
    if (node.nodeType !== Node.ELEMENT_NODE) return;
    const tag = (node.tagName || '').toLowerCase();
    if (['script','style','code'].includes(tag)) return;
    ['placeholder','title','aria-label','alt'].forEach(attr => {
      if (node.hasAttribute && node.hasAttribute(attr)) {
        const state = sourceForAttr(node, attr);
        const next = convertText(state.source, target);
        if (next !== node.getAttribute(attr)) node.setAttribute(attr, next);
        state.rendered = next;
      }
    });
    Array.from(node.childNodes || []).forEach(child => translateNode(child, target));
  }

  function apply(target){
    if (busy) return;
    busy = true;
    try {
      translateNode(document.body, target);
      document.documentElement.lang = target;
      const sel = document.getElementById('wn_language');
      if (sel && sel.value !== target) sel.value = target;
      try { localStorage.setItem('worknoiser.language', target); } catch(e){}
    } finally { busy = false; }
  }

  function setLanguage(target){
    if (!['es','en'].includes(target)) target = 'es';
    lang = target;
    apply(target);
  }

  function init(initial){
    let saved = null;
    try { saved = localStorage.getItem('worknoiser.language'); } catch(e){}
    setLanguage(saved || initial || 'es');

    observer = new MutationObserver(function(mutations){
      if (busy) return;
      mutations.forEach(m => {
        m.addedNodes.forEach(n => translateNode(n, lang));
        if (m.type === 'characterData') {
          // sourceForTextNode() recognises external Shiny changes by comparing
          // the current value with the last value rendered by this translator.
          translateNode(m.target, lang);
        }
      });
    });
    observer.observe(document.body, {subtree:true, childList:true, characterData:true});

    if (global.jQuery) {
      global.jQuery(document).on('shiny:inputchanged', function(e){
        if (e && e.name === 'wn_language') setLanguage(e.value);
      });
    }
  }

  global.WorknoiseRI18n = {setLanguage, init, getLanguage:() => lang, pairs};
})(window);
