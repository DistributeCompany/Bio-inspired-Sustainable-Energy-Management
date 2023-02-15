breed [jobs job ] ;agent set of jobs that need to be performed by workers
breed [workers worker] ;agent set of workers that perform jobs

jobs-own[
 assigned-worker ;id of worker that is assigned to job or is -1 if no worker is assigned yet
 created-at ;tick when job is created
 processed-at ;tick when job is processed (= a worker is assigned to this job)
 queue-grid-position ;position in the job queue grid (for visualization purposes)
]

workers-own[
 worker-battery-level ;level of battery of worker (0%-100%)
 active? ;boolean, indicates if worker is working (= performing a job)
 current-job ;ID of job that worker is processing or -1 if worker is not active
 charging-x-cor ;x position of worker in charging grid (for visualization purposes)
 charging-y-cor ;y position of worker in charging grid (for visualization purposes)
]

globals[
 hour; hour of the day (visualized on top-left corner of model)
 prop-current-worker ;probability that worker is going to charger

 buffer-battery? ;boolean, indicates if buffer battery is used (= central battery pack for all workers)
 buf-bat-cap ;capacity of the buffer battery
 buffer-battery-level ;level of buffer battery in %

 ;variables required for visualization purposes
 ;all x and y coordinates are set in 'init-visuals'
 solar-energy-source-x ;x-pos of solar power source in cable (first patch that has green color)
 solar-energy-source-y ;y-pos of solar power source in cable (first patch that has green color)
 wind-energy-source-x
 wind-energy-source-y
 grid-energy-source-x
 grid-energy-source-y
 battery-energy-source-x
 battery-energy-source-y
 solar-panel-x ;x-pos of solar panel visual reference point
 solar-panel-y ;y-pos of solar panel visual reference point
 windmill-x
 windmill-y
 grid-x
 grid-y
 battery-x
 battery-y
 charge-station-x
 charge-station-y
 horizon-y-pos; ;y-position of horizon

 ;patch location of first job in queue (initilized at setup)
 x-cor-jobs
 y-cor-jobs

 ;position of job to which worker will be assigned (for visualization)
 x-cor-current-job
 y-cor-current-job

 num-jobs ;number of jobs in system
 num-active-workers ;number of active workers

 ;inputs for size of job-queue
 queue-x ;size of queue in x-direction
 queue-y ;size of queue in y-direction
 queue-increment ;increment of pixels (both in x- and y direction)
 jobs-on-last-grid-pos ; a maximum of queue-x multiplied by queue-y jobs can be visualized, this variable indicates the number of jobs above this amount

 grid-power-to-battery? ;boolean that indicates if there is a power flow from grid to battery
 battery-power-to-grid? ;boolean that indicates if there is a power flow from battery to grid
 renewable-power-to-grid? ;boolean that indicates if there is a power flow from the renewable resources to grid
 renewable-power-to-battery? ;boolean that indicates if there is a power flow from the renewable resources to battery
 renewable-power-to-charging-station? ;boolean that indicates if there is a power flow from the renewable resources to charging station
 battery-power-to-charging-station? ;boolean that indicates if there is a power flow from the battery to charging station
 grid-power-to-charging-station? ;boolean that indicates if there is a power flow from grid to charging station

 renewable-energy-yield ;current production per MINUTE (=tick) in kWh
 renewable-energy-yield-per-hour ;current production per hour in kWh

 charging-demand ;current demand of energy in kWh from charging station

 renewable-for-consumption ;energy flow renewable energy to charging station in kWh
 battery-for-consumption ;energy flow from battery to charging station in kWh
 grid-for-consumption ;energy flow from grid to charging station in kWh
 renewable-to-battery ;energy flow from renewable energy to battery in kWh
 renewable-to-grid ;energy flow from renewable energy to grid in kWh

 wind-energy? ;boolean that indicates if wind energy should be visualized
 solar-energy? ;boolean that indicates if solar energy should be visualized
]

patches-own[
  power-cable ;indicates if patch is a power cable
  power ;indicates if there is a power flow through the power cable
  queue-number ;indicates the queue position for jobs, only applicable in the working area where jobs are created
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;SETUP;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all

  set buffer-battery? buffer-battery ;set user input on whether or not buffer-battery is used to global variable

  ;set buffer capacity if there is a battery
  ifelse buffer-battery?
  [set buf-bat-cap buffer-battery-capacity ]
  [set buf-bat-cap 1] ;to prevent division by zero error

  ;init power flows to False
  set grid-power-to-battery? False
  set battery-power-to-grid? False
  set renewable-power-to-grid? False
  set renewable-power-to-battery? False
  set renewable-power-to-charging-station? False
  set battery-power-to-charging-station? False
  set grid-power-to-charging-station? False

  ;init battery level
  set buffer-battery-level 0

  set hour 1 ;init hour
  set-hour ;sets hour of the day as label in the left uppercorner

  ;calculate the yield of renewable energy the first hour
  calculate-renewable-energy-yield

  ;set queue global properties
  set queue-x 5
  set queue-y 5
  set queue-increment 6

  ;reset properties of all patches
  ask patches [
    set power-cable Nobody
    set power False
  ]

  ;init of renewable energy source visualizations
  set wind-energy? True
  set solar-energy? False

  ;init visualisation of power elements on screen
  init-visuals

  ;init workers
  setup-workers

  ;set global variable with number of active workers
  ;set num-active-workers count workers with [active? = True]
  set num-active-workers 0

  ;set position of first job in queue (origin of queuing grid)
  set x-cor-jobs 44
  set y-cor-jobs -45

  ;init queuing grid (assign queue-ids to patches in grid)
  setup-queue-grid

  reset-ticks

end


to setup-queue-grid
  ;creates job queue grid with size queue-x by queue-y

  ;determine total size of queue (i.e., number of jobs that the queue can store)
  let grid-size (queue-x * queue-y)
  let counter 1
  let current-x x-cor-jobs
  let current-y y-cor-jobs
  let column 1

  while [column <= queue-x][

    ifelse counter mod (queue-y + 1) != 0 [
      ask patches with [pxcor = current-x and pycor = (current-y + ((counter - 1) * queue-increment))] ;determine next queue position, queue-increment = interdistance betwee two queue positions
      [
        set queue-number ((column - 1) * queue-y) + counter ;set queue number to patch
      ]
    ]
    [set current-x (current-x + queue-increment)
     set current-y y-cor-jobs
     set column column + 1
     set counter 0]

   set counter counter + 1 ;increment counter
  ]

end


to setup-workers
  ;set position of workers when charging
  let x-starting -36
  let y-starting -45
  let x x-starting
  let y y-starting

  ;init workers
  foreach (n-values number-of-workers [i -> i]) [ i ->

    ;create worker on patch
    ask patch x y [
      sprout-workers 1 [ set size 5
        set shape "truck"
        set color green
        set worker-battery-level 100
        set label worker-battery-level
        set label-color black
        set active? False ;init workers to be non-active (i.e., charging)
        set current-job -1
        set charging-x-cor xcor
        set charging-y-cor ycor
      ]
    ]
    ifelse (i + 1) mod 5 = 0 [
      set y y-starting
      set x x + 6 ] [
     set y y + 6
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;GO;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;procedure starts after pressing the go-button
to go

  ;check that the sum of param-energy, param-battery and param-jobs should be 1
  if param-energy + param-battery + param-jobs != 1
  [user-message ("Make sure that the sum of param-energy, param-battery and param-jobs is equal to 1. Click halt, reconfigure the parameters, and then click the GO button again.")]

 ; every tick = 1 minute, set new hour after 60 ticks
  if ticks mod 60 = 0 and ticks > 0 [
    if hour = 24 [ set hour 0] ;reset hour of day after 24 hours
    set hour hour + 1
    set-hour

    ;the amount of renewable energy yield changes every hour
    calculate-renewable-energy-yield

    ;init jobs
    create-new-jobs
    update-visuals

    ;only visualize solar energy during day hours
    ifelse hour > 6 and hour < 21 [
      set solar-energy? True
    ]
    [
      set solar-energy? False
    ]
  ]

  ;if there is not enough renewable energy production, but vehicles require charging:
  ;first, check whether battery buffer can be used
  ;second, if battery buffer is depleted, call receive-grid-energy procedure

  ;update battery levels of workers (battery depletes only if active)
   ask workers [
     if active? = True [
       set worker-battery-level (worker-battery-level - (energy-consumption *(1 / 60)))
      if (worker-battery-level < energy-consumption) [set color yellow]
      if worker-battery-level < 0 [
        set worker-battery-level 0
        set color red]
       set label round(worker-battery-level)
     ]
   ]

  set charging-demand 0 ;reset charging-demand
  ;update battery levels of workers who are charging (= active = False
   ask workers [
     if (active? = False) and (worker-battery-level < 100) [
       set worker-battery-level (worker-battery-level + (charging-speed *(1 / 60)))
      if (worker-battery-level < energy-consumption) [set color yellow]
      if (worker-battery-level > energy-consumption) [set color green]
       if worker-battery-level > 100 [
         set worker-battery-level 100]
       set label round(worker-battery-level)
      set charging-demand charging-demand + worker-battery-capacity * ((charging-speed / 100) / 60)
     ]
   ]

  ;check if jobs are finished at current tick (processing time of job is always 60 ticks = 60 minutes)
  if any? jobs with [processed-at + 60 = ticks AND processed-at > -1][
    finish-job
  ]

  ;set num-jobs
  set num-jobs count jobs

  ;update global variable indicating number of active worker
  set num-active-workers count workers with [active? = True]

  ;assign workers to jobs, if not all workers are already active
  if any? workers with [active? = false][
    assign-workers-to-jobs]

  ;calculate the power streams
  calculate-power-streams charging-demand

  ;visualizes the power streams based on the demand from the charging station
  visualize-power-streams

  ;increase tick
  tick

end


to calculate-renewable-energy-yield
  ;calculates the renewable energy yield for the current hour based on a normal distribution with
  ;renewable-energy-wattpeak as mean
  ;renewable-energy-sd as standard deviation
  ;and a scalability factor: season-scale (more or less controls when the sun rises and sets)

  let y 0
  ifelse hour <= 13 [
    set y  ( renewable-energy-wattpeak - ( seasonality-scale * ( 6 - ( hour / 2 )) * renewable-energy-sd) )
  ]
  [
    set y  ( renewable-energy-wattpeak + ( seasonality-scale * ( 6 - ( hour / 2 )) * renewable-energy-sd ) )
  ]

  let x 0

  if y > 0 [ set x y]

  let variation renewable-energy-sd * renewable-energy-sd
  let n-prop-max (1 / sqrt ( 2 * pi * variation) )
  let b ((x - renewable-energy-wattpeak) * (x - renewable-energy-wattpeak)) / ( 2 * variation)
  let n-prop (n-prop-max * exp (-1 * b))
  let p-i-green  (n-prop / n-prop-max)


  ; set yield per hour and per minute (tick)
  set renewable-energy-yield-per-hour (p-i-green * renewable-energy-wattpeak)
  set renewable-energy-yield renewable-energy-yield-per-hour / 60

end


to calculate-power-streams [demand-quantity]
  ; function that calculates power flows based on the demand from the charging station and supply from wind and solar energy
  ; also sets global variables that are uses to visualize the power streams in visualize-power-streams

  ;reset global variables
  set renewable-for-consumption 0
  set battery-for-consumption 0
  set grid-for-consumption 0
  set renewable-to-grid 0
  set renewable-to-battery 0

  ; if there is no demand, but there is renewable energy
  ifelse demand-quantity = 0 [

    ; set all supplies to chargin-station to False
    set grid-power-to-charging-station? False
    set renewable-power-to-charging-station? False
    if buffer-battery? [set battery-power-to-charging-station? False]

    ;there is supply from renewable sources
    ifelse renewable-energy-yield > 0 [

      ;check if there is a battery and battery is not full
      ifelse buffer-battery? and buffer-battery-level < buf-bat-cap[

        ;check if energy yield and current level is higher than battery capacity
        ifelse buffer-battery-level + renewable-energy-yield >= buf-bat-cap
        [set renewable-to-battery buf-bat-cap - buffer-battery-level
          set renewable-to-grid buffer-battery-level + renewable-energy-yield - buf-bat-cap
          set buffer-battery-level buf-bat-cap
         ] ; surplus goes to grid
        ;else
        [set buffer-battery-level buffer-battery-level + renewable-energy-yield
         set renewable-to-battery renewable-energy-yield]

        ;renewable supply to battery
         set renewable-power-to-battery? True
         set renewable-power-to-grid? False
      ][
        ;no battery or battery is full
        ; renewable supply to grid
        set renewable-power-to-grid? True
        set renewable-power-to-battery? False
        set renewable-to-grid renewable-energy-yield
      ]
    ]
    ;there is no supply from renewable sources
    [
      set renewable-power-to-battery? False
      set renewable-power-to-battery? False
    ]
  ]

  ; there is demand from charging station
  [
    ;there is supply from renewable sources
    ifelse renewable-energy-yield > 0
    [
      set renewable-power-to-charging-station? True

      ;check if supply is bigger than demand
      ifelse renewable-energy-yield > demand-quantity
      [
        set renewable-for-consumption demand-quantity ;what is used for consumption of the charging station is equal to the demand
        set grid-power-to-charging-station? False
        set battery-power-to-charging-station? False

        ;check if there is a battery and battery is not full
        ifelse buffer-battery? and buffer-battery-level < buf-bat-cap
        [

          ;check if net energy yield + current level of battery is higher than the capacity of the battery
          ifelse buffer-battery-level + (renewable-energy-yield - demand-quantity) >= buf-bat-cap

          [set buffer-battery-level buf-bat-cap] ;set current battery level to capacity

          [set buffer-battery-level buffer-battery-level + (renewable-energy-yield - demand-quantity) ;update battery level with the not used renewable energy
           set renewable-to-battery renewable-energy-yield - demand-quantity ] ;surplus of energy goes to battery


          ;supply renewable power to battery
          set renewable-power-to-battery? True
          set renewable-power-to-grid? False
        ]
        ;no battery or battery is full
        [
          set renewable-to-grid renewable-energy-yield - demand-quantity
          ;supply renewable power to grid
          set renewable-power-to-battery? False
          set renewable-power-to-grid? True
        ]
      ]
      ;demand is bigger than supply of renewable energy
      [
        set renewable-for-consumption renewable-energy-yield ;all green energy is used for consumption
        set renewable-power-to-grid? False
        set renewable-power-to-battery? False

        ;check if there is a battery an battery is not depleted
       ifelse buffer-battery? and buffer-battery-level > 0
        [

          ;check if net energy yield and current level of battery is lower than 0
          ifelse  buffer-battery-level + (renewable-energy-yield - demand-quantity) <= 0

          [set buffer-battery-level 0] ;set current battery level to 0

          ;update the battery level with distracting the shortage of renewable energy
          [set buffer-battery-level buffer-battery-level - (demand-quantity - renewable-energy-yield)
           set battery-for-consumption demand-quantity - renewable-energy-yield] ;calculate amount of energy that goes from battery to charging station


          ;supply power from battery to charging station
          set battery-power-to-charging-station? True
          set grid-power-to-charging-station? False

        ]
        ;no battery or battery is depleted
        [
          set grid-for-consumption demand-quantity - renewable-energy-yield
          ;supply power from grid to charging station
          set grid-power-to-charging-station? True
          set battery-power-to-charging-station? False
        ]
      ]
    ]
    ;there is no supply from renewable sources
    [
      set renewable-power-to-charging-station? False
      set renewable-power-to-grid? False
      set renewable-power-to-battery? False

      ;check if there is a battery and battery is not depleted
      ifelse buffer-battery? and buffer-battery-level > 0 [
        set battery-power-to-charging-station? True
        set grid-power-to-charging-station? False
      ]
      [
        ;there is no battery or battery is depleted
        set grid-power-to-charging-station? True
        set battery-power-to-charging-station? False
      ]
    ]
  ]

end


to create-new-jobs
  ;creates new jobs at the beginning of every hour based on number-of-jobs-per-hour input slider

  foreach n-values number-of-jobs-per-hour [i -> i] [x ->
    let y count jobs

    if y < (queue-x * queue-y) [
    ;create job on a queue grid position
    ask patches with [queue-number = (y + 1)][
      sprout-jobs 1 [
      set size 5
      set shape "box"
      set color grey
      set created-at ticks
      set assigned-worker -1
      set processed-at -1
      set queue-grid-position queue-number]
      ]
    ]

    ;if all positions in the grid are occupied, create job on the last position of the grid
    if y >= (queue-x * queue-y) [
    ask patches with [queue-number = (queue-x * queue-y) ] [
      sprout-jobs 1 [
      set size 5
      set shape "box"
      set color grey
      set created-at ticks
      set assigned-worker -1
      set processed-at -1
      set queue-grid-position queue-number]

      set jobs-on-last-grid-pos count jobs-here ;update number of jobs on last position of grid
      ]
    ]
  ]
end

;at every tick -> check if worker will process job (transition from passive (charging or idling) to active (performing job))
to assign-workers-to-jobs
  if ticks mod frequency = 0[

  ask workers with [worker-battery-level > energy-consumption][
    ;get random number between 0 and 100 for all non-active workers

    ifelse algorithm = "fixed probability"[
      if random 100 < (100 * prob-of-working) and active? = False [

        ;first sort jobs who still need to be processed on 'who' (=uniqueID, higher the number the later the worker is created)
        let sorted-jobs sort-on [who] jobs with [assigned-worker = -1]

        ;check if there are still jobs that need to be processed
        if length sorted-jobs > 0 [
          ;set worker-property active? to True
          set active? True ; set this worker to active
                           ;set num-current-jobs num-current-jobs - 1 ;decrease number of active jobs by 1

          ;log uniqueID of currently asked worker
          let current-worker who

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; CONNECT WORKER TO JOB ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;link longest waiting job with current-worker
          ask first sorted-jobs [
            set assigned-worker current-worker ;set assigned-worker to current-worker of the FIRST job in the list
            set color green ;change color to green
            set processed-at ticks ;log proccesed-at time
            set x-cor-current-job xcor
            set y-cor-current-job ycor
          ]

          ;move worker close to job to indicate it is allocated to that job
          set xcor (x-cor-current-job - 1)
          set ycor y-cor-current-job
          set current-job first sorted-jobs
        ]
      ]
      ][if algorithm = "bee-berry"[
        ;CALCULATE PROBABILITY OF GOING TO WORK BASED ON BEE-BERRY algorithm
        ;There are three elements: (1) stimulus of the sun/wind, (2) stimulus of the battery, and (3) stimulus of the jobs
        ;1. The sun provides a stimulus for workers to go to the charger. The more sun (and wind) currently available, the higher the stimulus.
        ;2. The battery of the vehicle provides a stimulus to go the charger. The lower the battery-level, the higher the stimulus
        ;3. The jobs that need to be processed provide an ANTI-stimulus to go the charger. The higher the # of current jobs, the higher the ANTI-stimulus
        ;The strength of each elements can be controlled via element-specific parameters
        ;All in all, this 'getouwtrek' should lead to a probability between 0 and 1 which determines for each worker whether or not to go charger (also when already AT the charger)

        ;reset probability
        set prop-current-worker 0

        ;STEP 1: Solar stimulus
        ;Get fitness of current hour of sun production (= % of maximum yield)
        let stimulus-solar (renewable-energy-yield * 60 ) / renewable-energy-wattpeak
        set prop-current-worker prop-current-worker + (param-energy * stimulus-solar) ;iadd to prop-current-worker

        ;STEP 2: battery stimulus
        let stimulus-battery 1 - (worker-battery-level ^ 2) / (worker-battery-level ^ 2 + battery-treshold ^ 2)
        set prop-current-worker prop-current-worker + (param-battery * stimulus-battery) ;add to prop-current-worker

        ;STEP 3: job-queue stimulus
        let waiting-jobs (num-jobs - num-active-workers)
        let stimulus-jobs (waiting-jobs ^ 2 / (waiting-jobs ^ 2 + job-threshold ^ 2)) ;REMARK: power = 2
        set prop-current-worker prop-current-worker + (param-jobs * (1 - stimulus-jobs)) ;add to prop-current-worker

        let rand-number random 100; get random number

        ;draw random number to determine whether worker is going to work (= 1 minus probability of going to charger)
        if rand-number < (100 * (1 - prop-current-worker)) and active? = False [

          ;first sort jobs who still need to be processed on 'who' (=uniqueID, higher the number the later the worker is created, thus first-come-first-serve)
          let sorted-jobs sort-on [who] jobs with [assigned-worker = -1]

          ;check if there are still jobs that need to be processed
          if length sorted-jobs > 0 [
            ;set worker-property active? to True
            set active? True ; set this worker to active
            set num-active-workers count workers with [active? = True] ;update global variable indicating number of active worker
            let current-worker who ;log uniqueID of currently asked worker

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; CONNECT WORKER TO JOB ;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;link longest waiting job with current-worker (FCFS)
            ask first sorted-jobs [
              set assigned-worker current-worker ;set assigned-worker to current-worker of the FIRST job in the list
              set color green ;change color to green
              set processed-at ticks ;log proccesed-at time
              set x-cor-current-job xcor
              set y-cor-current-job ycor
            ]

            ;move worker close to job to indicate it is allocated to that job
            set xcor (x-cor-current-job - 1) ;move worker to working area
            set ycor y-cor-current-job ;move worker to working area
            set current-job first sorted-jobs
          ]
        ]
      ]
    ]
  ]
]

end


;prodecure to determine the hour of the day
to set-hour
    ask patch -67 67 [set plabel hour] ;print hour of the day on patch
end


;remove job after it has been processed
to finish-job

  let jobs-to-finish sort-on [who] jobs with [processed-at + 60 = ticks]
  foreach jobs-to-finish
  [job-to-finish -> ask job-to-finish [
    ask worker assigned-worker [
      set active? False ;worker is no longer active
      set current-job -1 ;disconnect job
      setxy charging-x-cor charging-y-cor ;move worker to charging station
    ]
    die ;delete the finished job
    ]
    re-order-joblist ;re-order joblist
  ]
end

;after finished jobs are removed, re-order jobs on queue grid
to re-order-joblist

  ask jobs with [queue-grid-position < 25][
    set-job-to-queue self queue-grid-position - 1 ;move job one position
  ]

  ; patch of last position in job queue
  ask patch (x-cor-jobs + (queue-x - 1) * queue-increment) (y-cor-jobs + (queue-y - 1) * queue-increment)  [
    let jobs-at-queue-25 count jobs-here
    ifelse jobs-at-queue-25 >= 1[
        ask patch (x-cor-jobs + (queue-x )  * queue-increment + 3) (y-cor-jobs + (queue-y - 1) * queue-increment) [ ;label next to last position of job queue
        set plabel jobs-at-queue-25
        set plabel-color white]
      let counter 0
      ask jobs-here with-min [who][
        if counter = 0 [
          set-job-to-queue self queue-grid-position - 1
          set counter counter + 1
        ]
      ]
    ][
       ask patch (x-cor-jobs + (queue-x) * queue-increment + 3) (y-cor-jobs + (queue-y - 1) * queue-increment) [ ;label next to last position of job queue
        set plabel ""]
    ]
  ]

end


to set-job-to-queue [job-to-set queue-position]
;sets the job-to-set to a specific position in the queue
;called when a job is finished and remaining jobs needed to be reordered

  ask patches with [queue-number = queue-position] [
     let x-cor pxcor
     let y-cor pycor
     ask job-to-set [

      set queue-grid-position queue-position
      setxy x-cor y-cor

      if assigned-worker > -1 [
        ask worker assigned-worker  [
        setxy x-cor y-cor
       ]
     ]
   ]
 ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;VISUALS;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to init-visuals

  ;first set global variables

  set horizon-y-pos 25

  ;viualizes the energy flow through the power cables
  set solar-energy-source-x -62
  set solar-energy-source-y 22
  set wind-energy-source-x -83
  set wind-energy-source-y 22
  set grid-energy-source-x 60
  set grid-energy-source-y 20

  set solar-panel-x -70
  set solar-panel-y 37
  set windmill-x -80
  set windmill-y 63
  set grid-x 70
  set grid-y 64
  set charge-station-x -72
  set charge-station-y -32

  if buffer-battery? [
    set battery-x 0
    set battery-y 6
    set battery-energy-source-x 5
    set battery-energy-source-y -6]


  ;then init visuals based on the x and y cor of the objects
  create-horizon horizon-y-pos
  create-clouds
  create-windmill windmill-x windmill-y
  create-charge-station charge-station-x charge-station-y
  create-solar-panel solar-panel-x solar-panel-y
  if buffer-battery?[ create-battery battery-x battery-y]
  create-grid grid-x grid-y


  ;create all power cables between te objects

  ;create power cables from solar panel and windmill
  create-power-cable wind-energy-source-x wind-energy-source-y -63 10 "y" "from-wind" ;x-pos charge-station-x + 14
  create-power-cable solar-energy-source-x solar-energy-source-y -62 11 "y" "from-solar" ;x-pos charge-station-x + 14
  create-power-cable -62 9 -58 9 "x" "from-renewable"
  create-power-cable -58 9 -57 9 "x" "general1"
  create-power-cable -58 8 -58 -13 "x" "to-charge2"
  create-power-cable -58 -13 -58 -32 "x" "to-charge1"
  create-power-cable -51 -44 -40 -44 "x" "to-charge-area"
  create-power-cable -57 9 -8 9 "x" "to-battery-to-grid"
  create-power-cable -8 9 -7 9 "x" "general2"
  create-power-cable -7 9 9 9 "x" "to-grid1"
  create-power-cable 9 9 80 21 "x" "to-grid2"

  ;create power cable to battery
  if buffer-battery? [
    create-power-cable -8 8 -5 -5 "y" "to-battery"
    create-power-cable battery-energy-source-x battery-energy-source-y 10 -6 "x" "from-battery"
    create-power-cable 9 -7 -57 -12 "y" "from-battery-to-charge"

  ]

  create-power-cable grid-energy-source-x grid-energy-source-y -8 17 "y" "from-grid"
  create-power-cable -9 16 -58 9 "x" "from-grid-to-charge"

  ;create-power-cable wind-energy-source-x wind-energy-source-y -49 -45 "y" "from-wind" ;x-pos charge-station-x + 14

  create-charging-area
  create-job-area


end


to update-visuals

    reset-horizon
    create-sun
    create-clouds
    create-windmill windmill-x windmill-y
    create-solar-panel solar-panel-x solar-panel-y
    create-grid grid-x grid-y

end


to visualize-power-streams
  ;sets per cable the visualisation

  ;if solar production is active, call generate-solar-power procedure
  if solar-energy? [
    generate-solar-power
    visualize-power 53 "from-solar" ["from-renewable"]
  ]


  ;if wind energy is active, call generate-wind-power procedure
  if wind-energy? [
    generate-wind-power
    visualize-power 53 "from-wind" ["from-renewable"]
  ]


  ;there is renewable energy from a source
  if wind-energy? or solar-energy? [
    visualize-power 53 "from-renewable" ["general1"]
  ]


  if grid-power-to-charging-station? or grid-power-to-battery? [
    generate-grid-power
  ]


  if battery-power-to-grid? or battery-power-to-charging-station? [
    generate-battery-power

  ]


  if (renewable-power-to-grid? or renewable-power-to-battery?) and renewable-power-to-charging-station? and grid-power-to-charging-station? = False[
    visualize-power 53 "general1" ["to-battery-to-grid" "to-charge2"]

  ]

  ;generate grid power if there is demand from charging station or battery

  if grid-power-to-battery? [
    visualize-power 3 "from-grid" ["from-grid-to-battery"]
  ]

  if grid-power-to-charging-station?  [
    visualize-power 3 "from-grid" ["from-grid-to-charge"]
  ]



  ;;;;;power-to-chargin-station;;;;;;
  let power-to-charging-station False
  let color-to-charge 9.9


  if renewable-power-to-charging-station? [
    set power-to-charging-station True
    set color-to-charge 53
  ]

  if battery-power-to-charging-station? [
    visualize-power 53  "from-battery" ["from-battery-to-charge"]
    visualize-power 53  "from-battery-to-charge" ["to-charge1"]
    set color-to-charge 53
  ]


  if grid-power-to-charging-station? [
    visualize-power 3 "from-grid-to-charge" ["general1"]
    set power-to-charging-station True


    ifelse color-to-charge = 53 [
      set color-to-charge 43]
     [set color-to-charge 3]


  ]


   if power-to-charging-station =  True [

    visualize-power color-to-charge "general1" ["to-charge2"]
    visualize-power color-to-charge "to-charge2" ["to-charge1"]
  ]



  ; there is demand from charging station
  ; color depends if source is renewable, grid or mixed

  if power-to-charging-station or battery-power-to-charging-station? [
    visualize-power color-to-charge "to-charge1" [""]

    charging-power color-to-charge
    visualize-power color-to-charge "to-charge-area" [""]
  ]


  ;;;;;power-to-grid;;;;;;
  let power-to-grid False

  ;there is supply from renewable source and/or battery
  if renewable-power-to-grid?[
    visualize-power 53  "general1" ["to-battery-to-grid"]

    visualize-power 53  "to-battery-to-grid" ["general2"]

    ifelse renewable-power-to-battery? and grid-power-to-battery? = False[
      visualize-power 53 "general2" ["to-grid1" "to-battery"] ]
      [visualize-power 53  "general2" ["to-grid1"]]

    visualize-power 53  "to-grid1" ["to-grid2"]

    set power-to-grid True
  ]


  if battery-power-to-grid? [
    visualize-power 53  "from-battery" ["from-battery-to-grid"]
    visualize-power 53  "from-battery-to-grid" ["to-grid2"]
    set power-to-grid True

  ]

  if power-to-grid [

    visualize-power 53  "to-grid2" [""]
  ]


  ;;;;;power-to-battery;;;;;;
  let power-to-battery False
  let color-to-bat 9.9 ;init color

  if renewable-power-to-battery? [

    visualize-power 53  "general1" ["to-battery-to-grid"]
    visualize-power 53  "to-battery-to-grid" ["general2"]

    set power-to-battery True
    set color-to-bat 53 ;set color to green

  ]

  if grid-power-to-battery? [

   visualize-power 3  "from-grid-to-battery" ["general2"]

   set power-to-battery True

   ifelse color-to-bat = 53 ;there is already green energy
    [set color-to-bat 43]  ;set to mixed (yellow)
    [set color-to-bat 3]  ;set to gray

  ]


  if power-to-battery[

    ifelse renewable-power-to-grid? and grid-power-to-battery? = False
    [visualize-power 53 "general2" ["to-grid1" "to-battery"] ]
    [visualize-power color-to-bat  "general2" ["to-battery"]]
    visualize-power color-to-bat  "to-battery" [""]
  ]


end


to visualize-power [power-color from-power-cable to-power-cables]



  ask patches [

    ;if there is solar power
    if power-cable = from-power-cable and power [

      ; if color is white = last stage of power
      ifelse pcolor = power-color + 5 [
        ; end solar power in patch
        set power False
        set pcolor white
      ][
      ;if color is light green = middle stage of power
      ifelse  pcolor = power-color + 2
        ; make color white = last stage of power
        [set pcolor power-color + 5]

      ;else color is green = first stage of power
      [
       ; set power to lightgreen = middle stage of power
        set pcolor power-color + 2

          ask neighbors [
          ;ask neighbors and choose patch that is a power cable and has no solar power yet (power cannot go backwards)
            if power-cable = "charge2" [
            ]

          if (power-cable = from-power-cable or (position power-cable to-power-cables != False)) and power = False
          ; always first go to a cable that is "general"
          ;set color to green = first stage of power
            [set power True
             set pcolor power-color
          ]

          ]]]
         ]
       ]



end


to generate-solar-power


  ; intensity of power based on sun intensity on hour of the day
  if ticks mod 5 = 0 [
    ask patch solar-energy-source-x solar-energy-source-y [
    set pcolor 53
    set power True]
   ]

end


to generate-wind-power

  ; intensity of power based on sun intensity on hour of the day
  if ticks mod 5 = 0 [
    ask patch wind-energy-source-x wind-energy-source-y [
    set pcolor 53
    set power True]
   ]

end


to generate-grid-power


 ;intensity of the power visualized through the power cable, every tick a new power stream with color 3 is started
 if ticks mod 3 = 0 [
  ask patch grid-energy-source-x grid-energy-source-y [
   set pcolor 3
    set power True]
  ]

end


to generate-battery-power
    ;patches can have 3 types of color when there is solar power in the cable


 if ticks mod 1 = 0 [
  ask patch battery-energy-source-x battery-energy-source-y [
   set pcolor 53
    set power True]
  ]


end


to charging-power [color-of-power]

  if ticks mod 1 = 0 [
    ask patch -51 -44 [
      set pcolor color-of-power
      set power True]
  ]


end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE NEXT FUNCTIONS ARE USED FOR VISUALIZATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reset-horizon

  let color-horizon black

  if hour >= 8 and hour <= 17 [set color-horizon sky]

  if hour = 7 or hour = 18 [set color-horizon 92]

  let y-pos horizon-y-pos + 1
  while [y-pos <= max-pycor][

    let x-pos min-pxcor
    while [x-pos <= max-pxcor]
    [ask patch x-pos y-pos [set pcolor color-horizon]
      set x-pos (x-pos + 1)
    ]
    set y-pos (y-pos + 1)
  ]



end


to create-clouds
  ;keep y-pos fixed, make x-pos variable, move 10 patches to left every hour

  create-cloud (70 - (hour * 15)) 60

  create-cloud (-55 - (hour * 15)) 65


  create-cloud (-80 - (hour * 15)) 40

  create-cloud (0 - (hour * 15)) 36

  create-cloud (35 - (hour * 15)) 45
end


to create-horizon [y-pos]

  let x-pos min-pxcor

  while [x-pos <= max-pxcor]
    [ask patch x-pos y-pos [set pcolor white]
    set x-pos (x-pos + 1)
  ]


end


to create-charging-area

  let starting-x -40
  let starting-y -50

  let x-size 33
  let y-size 33

  ask patches with [pxcor >= starting-x and pxcor < starting-x + x-size and pycor >= starting-y and pycor < starting-y + y-size] [
    set pcolor 9.9
  ]

  ask patches with [pxcor = 24 + starting-x and pycor = starting-y - 2 ] [

    set plabel "Charging area"
    set plabel-color white

  ]

end


to create-job-area


  let starting-x 40
  let starting-y -50
  let x-size 33
  let y-size 33


    ask patches with [pxcor >= starting-x and pxcor < starting-x + x-size and pycor >= starting-y and pycor < starting-y + y-size] [
    set pcolor 9.9
  ]

  ask patches with [pxcor = 24 + starting-x and pycor = starting-y - 2 ] [

    set plabel "Working area"
    set plabel-color white
  ]



end


to create-power-cable [x-start y-start x-end y-end dir-first name]

  ; this functions creates a path (shortest path manhattan style) by coloring the patches between patch x-start y-start and patch x-end y-end
  ;dir-first determines in which direction to go first
  let x-cur x-start
  let y-cur y-start

  ;move first in x-direction, keep y constant
  if dir-first = "x"[
    ifelse x-start <= x-end [

      while [x-cur < x-end] [
        ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
        set x-cur x-cur + 1
    ]
    ]
     [
        while [x-cur > x-end] [
        ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
        set x-cur x-cur - 1
      ]

      ]

    ifelse y-start <= y-end [

    while [y-cur < y-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set y-cur y-cur + 1

    ]
    ]
    [
      while [y-cur > y-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set y-cur y-cur - 1

    ]
    ]
  ]


    ;move first in x-direction, keep y constant
  if dir-first = "y"[

    ifelse y-start <= y-end [

    while [y-cur <= y-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set y-cur y-cur + 1
    ]
    ]

    [
      while [y-cur >= y-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set y-cur y-cur - 1
      ]
    ]

   ifelse x-start <= x-end [

    while [x-cur <= x-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set x-cur x-cur + 1
    ]
  ]


  [ while [x-cur >= x-end] [
      ask patch x-cur y-cur [
          set pcolor white
          set power-cable name]
      set x-cur x-cur - 1
    ]
    ]
  ]

end


to create-grid [x-start-grid y-start-grid]
  ask patch (x-start-grid) ( y-start-grid) [set pcolor 8]

  ask patch (x-start-grid - 1) ( y-start-grid - 1) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 1) [set pcolor 8]

  ask patch (x-start-grid - 1) ( y-start-grid - 2) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 2) [set pcolor 8]

  ask patch (x-start-grid - 2) ( y-start-grid - 3) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 3) [set pcolor 8]

  ask patch (x-start-grid - 3) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid ) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 4) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 4) [set pcolor 8]

  ask patch (x-start-grid - 5) ( y-start-grid - 5) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 5) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 5) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 5) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 5) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 5) [set pcolor 8]

  ask patch (x-start-grid - 8) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 6) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 6) [set pcolor 8]

  ask patch (x-start-grid - 9) ( y-start-grid - 7) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 7) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 7) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 7) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 8) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 5) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid ) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 8) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 8) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 9) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 9) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 9) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 9) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 9) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 9) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid - 10) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 10) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 10) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 11) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 11) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 11) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 11) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 12) [set pcolor 8]
  ask patch (x-start-grid ) ( y-start-grid - 12) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 12) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 13) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 13) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 13) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 13) [set pcolor 8]

    ask patch (x-start-grid - 4) ( y-start-grid - 14) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 14) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 14) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 14) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 15) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 15) [set pcolor 8]

  ask patch (x-start-grid - 5) ( y-start-grid - 16) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 16) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 16) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 16) [set pcolor 8]

  ask patch (x-start-grid - 8) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 17) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 17) [set pcolor 8]

  ask patch (x-start-grid - 9) ( y-start-grid - 18) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 18) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 18) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 18) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 8) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 5) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid ) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 19) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 19) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 20) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 20) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 20) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 20) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid - 10) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 21) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 21) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 22) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 22) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 23) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 23) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 24) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 24) [set pcolor 8]

  ask patch (x-start-grid - 4) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 25) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 25) [set pcolor 8]

  ask patch (x-start-grid - 5) ( y-start-grid - 26) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 26) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 26) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 26) [set pcolor 8]

  ask patch (x-start-grid - 5) ( y-start-grid - 27) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 27) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 27) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 27) [set pcolor 8]

  ask patch (x-start-grid - 6) ( y-start-grid - 28) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 28) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 28) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 28) [set pcolor 8]

  ask patch (x-start-grid - 6) ( y-start-grid - 29) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 29) [set pcolor 8]
  ask patch (x-start-grid ) ( y-start-grid - 29) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 29) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 29) [set pcolor 8]

  ask patch (x-start-grid - 7) ( y-start-grid - 30) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 30) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 30) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 30) [set pcolor 8]

  ask patch (x-start-grid - 7) ( y-start-grid - 31) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 31) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 31) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 31) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 31) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 31) [set pcolor 8]

  ask patch (x-start-grid - 8) ( y-start-grid - 32) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 32) [set pcolor 8]
  ask patch (x-start-grid - 5) ( y-start-grid - 32) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 32) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 32) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 32) [set pcolor 8]

  ask patch (x-start-grid - 8) ( y-start-grid - 33) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 33) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 33) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 33) [set pcolor 8]

  ask patch (x-start-grid - 8) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 5) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 4) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 3) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 2) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid - 1) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 1) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 2) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 3) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 4) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 5) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 34) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 34) [set pcolor 8]

  ask patch (x-start-grid - 9) ( y-start-grid - 35) [set pcolor 8]
  ask patch (x-start-grid - 6) ( y-start-grid - 35) [set pcolor 8]
  ask patch (x-start-grid + 6) ( y-start-grid - 35) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 35) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 36) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 36) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 36) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 36) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 37) [set pcolor 8]
  ask patch (x-start-grid - 7) ( y-start-grid - 37) [set pcolor 8]
  ask patch (x-start-grid + 7) ( y-start-grid - 37) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 37) [set pcolor 8]

  ask patch (x-start-grid - 10) ( y-start-grid - 38) [set pcolor 8]
  ask patch (x-start-grid - 8) ( y-start-grid - 38) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 38) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 38) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 39) [set pcolor 8]
  ask patch (x-start-grid - 8) ( y-start-grid - 39) [set pcolor 8]
  ask patch (x-start-grid + 8) ( y-start-grid - 39) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 39) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 40) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 40) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 40) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 40) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 41) [set pcolor 8]
  ask patch (x-start-grid - 9) ( y-start-grid - 41) [set pcolor 8]
  ask patch (x-start-grid + 9) ( y-start-grid - 41) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 41) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 42) [set pcolor 8]
  ask patch (x-start-grid - 10) ( y-start-grid - 42) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 42) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 42) [set pcolor 8]

  ask patch (x-start-grid - 11) ( y-start-grid - 43) [set pcolor 8]
  ask patch (x-start-grid - 10) ( y-start-grid - 43) [set pcolor 8]
  ask patch (x-start-grid + 10) ( y-start-grid - 43) [set pcolor 8]
  ask patch (x-start-grid + 11) ( y-start-grid - 43) [set pcolor 8]

end


to create-solar-panel [x-start-panel y-start-panel]
  ask patch (x-start-panel) ( y-start-panel)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 1)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 1)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 1)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 1)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 1)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 1)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 2)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 2)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 2)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 2)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 2)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 2)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 3)  [ set pcolor 106 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 3)  [ set pcolor 106 ]


  ask patch (x-start-panel) ( y-start-panel - 4)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 4)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 4)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 4)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 4)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 4)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 5)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 5)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 5)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 5)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 5)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 5)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 6)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 6)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 6)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 6)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 6)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 6)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 7)  [ set pcolor 106 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 7)  [ set pcolor 106 ]


  ask patch (x-start-panel) ( y-start-panel - 8)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 8)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 8)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 8)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 8)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 8)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 9)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 9)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 9)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 9)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 9)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 9)  [ set pcolor 103 ]

  ask patch (x-start-panel) ( y-start-panel - 10)  [ set pcolor 106 ]
  ask patch (x-start-panel + 1) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 2) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 3) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 4) ( y-start-panel - 10)  [ set pcolor 106 ]
  ask patch (x-start-panel + 5) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 6) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 7) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 8) ( y-start-panel - 10)  [ set pcolor 106 ]
  ask patch (x-start-panel + 9) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 10) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 11) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 12) ( y-start-panel - 10)  [ set pcolor 106 ]
  ask patch (x-start-panel + 13) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 14) ( y-start-panel - 10)  [ set pcolor 103 ]
  ask patch (x-start-panel + 15) ( y-start-panel - 10)  [ set pcolor 103 ]

 ask patch (x-start-panel + 7) ( y-start-panel - 11)  [ set pcolor 5 ]
 ask patch (x-start-panel + 8) ( y-start-panel - 11)  [ set pcolor 5 ]

 ask patch (x-start-panel + 7) ( y-start-panel - 12)  [ set pcolor 5 ]
 ask patch (x-start-panel + 8) ( y-start-panel - 12)  [ set pcolor 8 ]

 ask patch (x-start-panel + 7) ( y-start-panel - 13)  [ set pcolor 5 ]
 ask patch (x-start-panel + 8) ( y-start-panel - 13)  [ set pcolor 8 ]

 ask patch (x-start-panel + 2) ( y-start-panel - 14)  [ set pcolor 5 ]
 ask patch (x-start-panel + 3) ( y-start-panel - 14)  [ set pcolor 5 ]
 ask patch (x-start-panel + 4) ( y-start-panel - 14)  [ set pcolor 5 ]
 ask patch (x-start-panel + 5) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 6) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 7) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 8) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 9) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 10) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 11) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 12) ( y-start-panel - 14)  [ set pcolor 8 ]
 ask patch (x-start-panel + 13) ( y-start-panel - 14)  [ set pcolor 8 ]

end


to create-sun
  ;moving sun, position based on the hour of the day
  let x-start-sun 0
  let y-start-sun 0

  ;only sun between 6h and 18h
  if hour > 6 and hour <= 18 [

    if hour = 7 [
      set x-start-sun -65
      set y-start-sun 25 ]

    if hour = 8 [
      set x-start-sun -53
      set y-start-sun 31]

    if hour = 9 [
      set x-start-sun -41
      set y-start-sun 37]

    if hour = 10 [
      set x-start-sun -29
      set y-start-sun 43]

    if hour = 11 [
      set x-start-sun -17
      set y-start-sun 49]

    if hour = 12 [
      set x-start-sun -6
      set y-start-sun 55]

    if hour = 13 [
      set x-start-sun 6
      set y-start-sun 55]

    if hour = 14 [
      set x-start-sun 17
      set y-start-sun 49]

    if hour = 15 [
      set x-start-sun 29
      set y-start-sun 43]

    if hour = 16 [
      set x-start-sun 41
      set y-start-sun 37]

    if hour = 17 [
      set x-start-sun 53
      set y-start-sun 31]

    if hour = 18 [
      set x-start-sun 65
      set y-start-sun 25]



  ;vlammen bovenste helft
  ask patch x-start-sun (y-start-sun + 14) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 13) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun + 12) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 12) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun + 11) [ set pcolor orange ]
  ask patch (x-start-sun ) (y-start-sun + 11) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 11) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun + 11) [ set pcolor orange ]

  ask patch (x-start-sun - 11) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun ) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 10) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun + 10) [ set pcolor orange ]

  ask patch (x-start-sun - 9) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun - 3) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun + 9) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun + 9) [ set pcolor orange ]

  ask patch (x-start-sun - 8) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun - 3) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun) (y-start-sun + 8) [ set pcolor yellow ]
  ask patch (x-start-sun + 1) (y-start-sun + 8) [ set pcolor yellow ]

  ask patch (x-start-sun + 4) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun + 8) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun + 8) [ set pcolor orange]

  ask patch (x-start-sun - 8) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun - 5) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun + 7) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun + 7) [ set pcolor orange ]

  ask patch (x-start-sun - 8) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun - 5) (y-start-sun + 6) [ set pcolor yellow ]
  ask patch (x-start-sun - 4) (y-start-sun + 6) [ set pcolor yellow ]
  ask patch (x-start-sun + 3) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun + 6) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun + 6) [ set pcolor orange ]

  ask patch (x-start-sun - 6) (y-start-sun + 5) [ set pcolor orange ]
  ask patch (x-start-sun - 5) (y-start-sun + 5) [ set pcolor yellow ]
  ask patch (x-start-sun + 5) (y-start-sun + 5) [ set pcolor yellow ]
  ask patch (x-start-sun + 6) (y-start-sun + 5) [ set pcolor yellow ]
  ask patch (x-start-sun + 7) (y-start-sun + 5) [ set pcolor orange ]


  ask patch (x-start-sun - 8) (y-start-sun + 4) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun + 4) [ set pcolor yellow ]

  ask patch (x-start-sun - 7) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun + 3) [ set pcolor orange ]

  ask patch (x-start-sun + 7) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun + 2) [ set pcolor orange ]


  ask patch (x-start-sun - 11) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun + 1) [ set pcolor yellow ]
  ask patch (x-start-sun + 10) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 11) (y-start-sun + 1) [ set pcolor orange]


  if hour > 7 and hour < 18 [

  ask patch (x-start-sun - 14) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 13) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 12) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 11) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun) [ set pcolor yellow ]
  ask patch (x-start-sun + 8) (y-start-sun) [ set pcolor yellow ]
  ask patch (x-start-sun + 9) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun + 11) (y-start-sun) [ set pcolor orange]
  ask patch (x-start-sun + 12) (y-start-sun) [ set pcolor orange]
  ask patch (x-start-sun + 13) (y-start-sun) [ set pcolor orange]
  ask patch (x-start-sun + 14) (y-start-sun) [ set pcolor orange]

  if hour > 8 and hour < 17 [

   if hour > 9 and hour < 16   [
  ask patch x-start-sun (y-start-sun - 14) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 13) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun - 12) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 12) [ set pcolor orange ]
  ]

  ask patch (x-start-sun + 1) (y-start-sun - 11) [ set pcolor orange ]
  ask patch (x-start-sun ) (y-start-sun - 11) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 11) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun - 11) [ set pcolor orange ]

  ask patch (x-start-sun + 11) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun ) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 10) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun - 10) [ set pcolor orange ]

  ask patch (x-start-sun + 9) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun - 9) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun - 9) [ set pcolor orange ]

  ask patch (x-start-sun + 8) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun) (y-start-sun - 8) [ set pcolor yellow ]
  ask patch (x-start-sun - 1) (y-start-sun - 8) [ set pcolor yellow ]

  ask patch (x-start-sun - 4) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun - 8) [ set pcolor orange ]
  ask patch (x-start-sun - 10) (y-start-sun - 8) [ set pcolor orange]

  ask patch (x-start-sun + 8) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun - 3) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun - 7) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun - 7) [ set pcolor orange ]

  ask patch (x-start-sun + 8) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun + 7) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun - 6) [ set pcolor yellow ]
  ask patch (x-start-sun + 4) (y-start-sun - 6) [ set pcolor yellow ]
  ask patch (x-start-sun - 3) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun - 5) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun - 6) [ set pcolor orange ]
  ask patch (x-start-sun - 7) (y-start-sun - 6) [ set pcolor orange ]
  ]

  ask patch (x-start-sun + 6) (y-start-sun - 5) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun - 5) [ set pcolor yellow ]
  ask patch (x-start-sun - 5) (y-start-sun - 5) [ set pcolor yellow ]
  ask patch (x-start-sun - 6) (y-start-sun - 5) [ set pcolor yellow ]
  ask patch (x-start-sun - 7) (y-start-sun - 5) [ set pcolor orange ]

  ask patch (x-start-sun + 8) (y-start-sun - 4) [ set pcolor orange ]
  ask patch (x-start-sun - 6) (y-start-sun - 4) [ set pcolor yellow ]

  ask patch (x-start-sun + 7) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun + 6) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun - 9) (y-start-sun - 3) [ set pcolor orange ]

  ask patch (x-start-sun - 7) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun - 8) (y-start-sun - 2) [ set pcolor orange ]


  ask patch (x-start-sun + 11) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 10) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 9) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 8) (y-start-sun - 1) [ set pcolor yellow ]
  ask patch (x-start-sun - 10) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun - 11) (y-start-sun - 1) [ set pcolor orange]

  ]

  ;kern

  if hour > 7 and hour < 18 [
  ask patch (x-start-sun - 1) (y-start-sun - 5) [ set pcolor yellow ]
  ask patch x-start-sun (y-start-sun - 5) [ set pcolor yellow ]
  ask patch (x-start-sun + 1) (y-start-sun - 5) [ set pcolor yellow ]

  ask patch (x-start-sun - 3) (y-start-sun - 4) [ set pcolor yellow ]
  ask patch (x-start-sun - 2) (y-start-sun - 4) [ set pcolor yellow ]
  ask patch (x-start-sun - 1) (y-start-sun - 4) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 4) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun - 4) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 4) [ set pcolor yellow ]
  ask patch (x-start-sun + 3) (y-start-sun - 4) [ set pcolor yellow ]

  ask patch (x-start-sun - 4) (y-start-sun - 3) [ set pcolor yellow ]
  ask patch (x-start-sun - 3) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 3) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun - 3) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun - 3) [ set pcolor yellow ]

  ask patch (x-start-sun - 4) (y-start-sun - 2) [ set pcolor yellow ]
  ask patch (x-start-sun - 3) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 2) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun - 2) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun - 2) [ set pcolor yellow ]

  ask patch (x-start-sun - 5) (y-start-sun - 1) [ set pcolor yellow ]
  ask patch (x-start-sun - 4) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun - 3) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun - 1) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun - 1) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun - 1) [ set pcolor yellow ]

  ask patch (x-start-sun - 5) (y-start-sun) [ set pcolor yellow ]
  ask patch (x-start-sun - 4) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun - 3) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun - 2) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun - 1) y-start-sun [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun) [ set pcolor orange ]
  ask patch (x-start-sun + 1) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun + 2) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun + 3) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun + 4) y-start-sun [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun) [ set pcolor yellow ]

  ]

  ask patch (x-start-sun - 5) (y-start-sun + 1) [ set pcolor yellow ]
  ask patch (x-start-sun - 4) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 3) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun + 1) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun + 1) [ set pcolor orange ]
  ask patch (x-start-sun + 5) (y-start-sun + 1) [ set pcolor yellow ]

  ask patch (x-start-sun - 4) (y-start-sun + 2) [ set pcolor yellow ]
  ask patch (x-start-sun - 3) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun + 2) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun + 2) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun + 2) [ set pcolor yellow ]

  ask patch (x-start-sun - 4) (y-start-sun + 3) [ set pcolor yellow ]
  ask patch (x-start-sun - 3) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun - 2) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun - 1) (y-start-sun + 3) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 3) (y-start-sun + 3) [ set pcolor orange ]
  ask patch (x-start-sun + 4) (y-start-sun + 3) [ set pcolor yellow ]

  ask patch (x-start-sun - 3) (y-start-sun + 4) [ set pcolor yellow  ]
  ask patch (x-start-sun - 2) (y-start-sun + 4) [ set pcolor yellow  ]
  ask patch (x-start-sun - 1) (y-start-sun + 4) [ set pcolor orange ]
  ask patch x-start-sun (y-start-sun + 4) [ set pcolor orange ]
  ask patch (x-start-sun + 1) (y-start-sun + 4) [ set pcolor orange ]
  ask patch (x-start-sun + 2) (y-start-sun + 4) [ set pcolor yellow  ]
  ask patch (x-start-sun + 3) (y-start-sun + 4) [ set pcolor yellow  ]

  ask patch (x-start-sun - 1) (y-start-sun + 5) [ set pcolor yellow ]
  ask patch x-start-sun (y-start-sun + 5) [ set pcolor yellow ]
  ask patch (x-start-sun + 1) (y-start-sun + 5) [ set pcolor yellow ]
  ]

end


to create-windmill [x-start-windmill y-start-windmill]

  ask patch (x-start-windmill) (y-start-windmill - 1) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 2) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 2) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 2) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 3) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 3) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 3) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 4) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 4) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 4) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 5) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 5) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 5) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 6) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 6) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 6) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 7) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 7) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 7) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 8) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 8) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 8) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 9) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 9) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 9) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 10) [ set pcolor white ]
  ask patch (x-start-windmill) (y-start-windmill - 10) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 10) [ set pcolor white ]

  ask patch (x-start-windmill) (y-start-windmill - 11) [ set pcolor white ]

  ask patch (x-start-windmill + 1) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 1) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 12) [ set pcolor 8 ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 12) [ set pcolor 8 ]

  ask patch (x-start-windmill + 2) (y-start-windmill - 13) [ set pcolor white ]
  ask patch (x-start-windmill + 1) (y-start-windmill - 13) [ set pcolor white ]

  ask patch (x-start-windmill) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 1) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 13) [ set pcolor 8 ]
  ask patch (x-start-windmill - 7) (y-start-windmill - 13) [ set pcolor 8 ]


 ask patch (x-start-windmill + 2) (y-start-windmill - 14) [ set pcolor white ]
 ask patch (x-start-windmill + 1) (y-start-windmill - 14) [ set pcolor white ]

  ask patch (x-start-windmill) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 1) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 14) [ set pcolor 8 ]
  ask patch (x-start-windmill - 7) (y-start-windmill - 14) [ set pcolor 5 ]


  ask patch (x-start-windmill + 1) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 1) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 15) [ set pcolor 5 ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 15) [ set pcolor 5 ]


  ask patch (x-start-windmill + 2) (y-start-windmill - 16) [ set pcolor white ]
  ask patch (x-start-windmill + 3) (y-start-windmill - 16) [ set pcolor white ]
  ask patch (x-start-windmill + 4) (y-start-windmill - 16) [ set pcolor white ]

  ask patch (x-start-windmill + 2) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill + 3) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill + 4) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill + 5) (y-start-windmill - 17) [ set pcolor white ]

  ask patch (x-start-windmill + 3) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill + 4) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill + 5) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill + 6) (y-start-windmill - 18) [ set pcolor white ]


  ask patch (x-start-windmill + 4) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill + 5) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill + 6) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill + 7) (y-start-windmill - 19) [ set pcolor white ]



  ask patch (x-start-windmill + 5) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill + 6) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill + 7) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill + 8) (y-start-windmill - 20) [ set pcolor white ]


  ask patch (x-start-windmill + 6) (y-start-windmill - 21) [ set pcolor white ]
  ask patch (x-start-windmill + 7) (y-start-windmill - 21) [ set pcolor white ]
  ask patch (x-start-windmill + 8) (y-start-windmill - 21) [ set pcolor white ]

  ask patch (x-start-windmill + 7) (y-start-windmill - 22) [ set pcolor white ]
  ask patch (x-start-windmill + 8) (y-start-windmill - 22) [ set pcolor white ]



  ask patch (x-start-windmill - 1) (y-start-windmill - 16) [ set pcolor white ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 16) [ set pcolor white ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 16) [ set pcolor white ]

  ask patch (x-start-windmill - 1) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 17) [ set pcolor white ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 17) [ set pcolor white ]


  ask patch (x-start-windmill - 2) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 18) [ set pcolor white ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 18) [ set pcolor white ]


  ask patch (x-start-windmill - 3) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill - 4) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 19) [ set pcolor white ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 19) [ set pcolor white ]


  ask patch (x-start-windmill - 4) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill - 5) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 20) [ set pcolor white ]
  ask patch (x-start-windmill - 7) (y-start-windmill - 20) [ set pcolor white ]


  ask patch (x-start-windmill - 5) (y-start-windmill - 21) [ set pcolor white ]
  ask patch (x-start-windmill - 6) (y-start-windmill - 21) [ set pcolor white ]
  ask patch (x-start-windmill - 7) (y-start-windmill - 21) [ set pcolor white ]
  ask patch (x-start-windmill - 8) (y-start-windmill - 21) [ set pcolor white ]

 ask patch (x-start-windmill - 6) (y-start-windmill - 22) [ set pcolor white ]
  ask patch (x-start-windmill - 7) (y-start-windmill - 22) [ set pcolor white ]
  ask patch (x-start-windmill - 8) (y-start-windmill - 22) [ set pcolor white ]


  ask patch (x-start-windmill - 7) (y-start-windmill - 23) [ set pcolor white ]
  ask patch (x-start-windmill - 8) (y-start-windmill - 23) [ set pcolor white ]


  ;paal

  ask patch (x-start-windmill - 4) (y-start-windmill - 16) [ set pcolor 5 ]


  ask patch (x-start-windmill - 2) (y-start-windmill - 19) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 20) [ set pcolor 5 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 20) [ set pcolor 5 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 21) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 21) [ set pcolor 5 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 21) [ set pcolor 5 ]


  ask patch (x-start-windmill - 4) (y-start-windmill - 22) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 22) [ set pcolor 5 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 22) [ set pcolor 8 ]


  ask patch (x-start-windmill - 4) (y-start-windmill - 23) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 23) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 23) [ set pcolor 8 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 24) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 24) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 24) [ set pcolor 8 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 24) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 24) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 24) [ set pcolor 8 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 25) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 25) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 25) [ set pcolor 8 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 26) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 26) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 26) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 27) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 27) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 27) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 28) [ set pcolor 103 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 28) [ set pcolor 107]
  ask patch (x-start-windmill - 2) (y-start-windmill - 28) [ set pcolor 107]

  ask patch (x-start-windmill - 4) (y-start-windmill - 29) [ set pcolor 103 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 29) [ set pcolor 107]
  ask patch (x-start-windmill - 2) (y-start-windmill - 29) [ set pcolor 107]

    ask patch (x-start-windmill - 4) (y-start-windmill - 30) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 30) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 30) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 31) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 31) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 31) [ set pcolor 8 ]

  ask patch (x-start-windmill - 4) (y-start-windmill - 32) [ set pcolor 103 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 32) [ set pcolor 107]
  ask patch (x-start-windmill - 2) (y-start-windmill - 32) [ set pcolor 107]

  ask patch (x-start-windmill - 4) (y-start-windmill - 33) [ set pcolor 103 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 33) [ set pcolor 107]
  ask patch (x-start-windmill - 2) (y-start-windmill - 33) [ set pcolor 107]

      ask patch (x-start-windmill - 4) (y-start-windmill - 34) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 34) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 34) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 35) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 35) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 35) [ set pcolor 8 ]

      ask patch (x-start-windmill - 4) (y-start-windmill - 36) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 36) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 36) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 37) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 37) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 37) [ set pcolor 8 ]

      ask patch (x-start-windmill - 4) (y-start-windmill - 38) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 38) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 38) [ set pcolor 8 ]

      ask patch (x-start-windmill - 4) (y-start-windmill - 39) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 39) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 39) [ set pcolor 8 ]

    ask patch (x-start-windmill - 4) (y-start-windmill - 40) [ set pcolor 5 ]
  ask patch (x-start-windmill - 3) (y-start-windmill - 40) [ set pcolor 8 ]
  ask patch (x-start-windmill - 2) (y-start-windmill - 40) [ set pcolor 8 ]




end


to create-battery [x-start-battery y-start-battery]


  ask patch (x-start-battery - 1 ) (y-start-battery) [ set pcolor 5 ]
  ask patch (x-start-battery) (y-start-battery) [ set pcolor 8 ]
  ask patch (x-start-battery + 1) (y-start-battery) [ set pcolor 8 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 1) [ set pcolor 13 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery + 1) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 1) [ set pcolor 16 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 1) [ set pcolor red ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 1) [ set pcolor red ]

    ask patch (x-start-battery - 4 ) (y-start-battery - 2) [ set pcolor red ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 2) [ set pcolor 13 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 2) [ set pcolor red ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 2) [ set pcolor red ]
  ask patch (x-start-battery) (y-start-battery - 2) [ set pcolor 103 ]
  ask patch (x-start-battery + 1) (y-start-battery - 2) [ set pcolor red ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 2) [ set pcolor 16 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 2) [ set pcolor red ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 2) [ set pcolor red ]


    ask patch (x-start-battery - 4 ) (y-start-battery - 3) [ set pcolor red ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 3) [ set pcolor 13 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 3) [ set pcolor red ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 3) [ set pcolor 103 ]
  ask patch (x-start-battery) (y-start-battery - 3) [ set pcolor 103 ]
  ask patch (x-start-battery + 1) (y-start-battery - 3) [ set pcolor 105 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 3) [ set pcolor 16 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 3) [ set pcolor red ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 3) [ set pcolor red ]

    ask patch (x-start-battery - 4 ) (y-start-battery - 4) [ set pcolor red ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 4) [ set pcolor 13 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 4) [ set pcolor red ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 4) [ set pcolor red ]
  ask patch (x-start-battery) (y-start-battery - 4) [ set pcolor 103 ]
  ask patch (x-start-battery + 1) (y-start-battery - 4) [ set pcolor red ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 4) [ set pcolor 16 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 4) [ set pcolor red ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 4) [ set pcolor red ]


  ask patch (x-start-battery - 4 ) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 5) [ set pcolor 13 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery + 1) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 5) [ set pcolor 16 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 5) [ set pcolor red ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 5) [ set pcolor red ]

   ask patch (x-start-battery - 4 ) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 6) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 6) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 6) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 6) [ set pcolor 106 ]


  ask patch (x-start-battery - 4 ) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 7) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 7) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 7) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 7) [ set pcolor 106 ]


  ask patch (x-start-battery - 4 ) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 8) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 8) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 8) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 8) [ set pcolor 106 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 9) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 9) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 9) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 9) [ set pcolor 106 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 10) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 10) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 10) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 10) [ set pcolor 106 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 11) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 11) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 11) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 11) [ set pcolor 106 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 12) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 12) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 12) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 12) [ set pcolor 106 ]

  ask patch (x-start-battery - 4 ) (y-start-battery - 13) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 13) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 13) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 13) [ set pcolor 103 ]
  ask patch (x-start-battery) (y-start-battery - 13) [ set pcolor 103 ]
  ask patch (x-start-battery + 1) (y-start-battery - 13) [ set pcolor 105 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 13) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 13) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 13) [ set pcolor 106 ]

    ask patch (x-start-battery - 4 ) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 14) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 14) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 14) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 14) [ set pcolor 106 ]

    ask patch (x-start-battery - 4 ) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery - 3 ) (y-start-battery - 15) [ set pcolor 103 ]
  ask patch (x-start-battery - 2 ) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery - 1 ) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery + 1) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery + 2 ) (y-start-battery - 15) [ set pcolor 109 ]
  ask patch (x-start-battery + 3 ) (y-start-battery - 15) [ set pcolor 106 ]
  ask patch (x-start-battery + 4 ) (y-start-battery - 15) [ set pcolor 106 ]

end


to create-charge-station [ x-start-charge-station y-start-charge-station]


  ask patch (x-start-charge-station + 1) ( y-start-charge-station) [ set pcolor gray ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station) [ set pcolor gray ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station) [ set pcolor 26 ]


  ask patch (x-start-charge-station + 1) ( y-start-charge-station - 1) [ set pcolor gray ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 1) [ set pcolor gray ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 1) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 1) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 1) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 1) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 1) [ set pcolor 26 ]

  ask patch (x-start-charge-station) ( y-start-charge-station - 2) [ set pcolor 106 ]
  ask patch (x-start-charge-station + 1) ( y-start-charge-station - 2) [ set pcolor 106 ]
  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 2) [ set pcolor 106 ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 2) [ set pcolor 106 ]
  ask patch (x-start-charge-station + 4) ( y-start-charge-station - 2) [ set pcolor 106 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 2) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 2) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 2) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 2) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 2) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 2) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 2) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 2) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 2) [ set pcolor 26 ]


  ask patch (x-start-charge-station) ( y-start-charge-station - 3) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 1) ( y-start-charge-station - 3) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 3) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 3) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 4) ( y-start-charge-station - 3) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 3) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 3) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 3) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 3) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 3) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 3) [ set pcolor yellow]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 3) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 3) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 3) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 3) [ set pcolor 26 ]


  ask patch (x-start-charge-station) ( y-start-charge-station - 4) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 1) ( y-start-charge-station - 4) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 4) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 4) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 4) ( y-start-charge-station - 4) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 4) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 4) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 4) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 4) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 4) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 4) [ set pcolor yellow]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 4) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 4) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 4) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 4) [ set pcolor 26 ]


  ask patch (x-start-charge-station) ( y-start-charge-station - 5) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 1) ( y-start-charge-station - 5) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 5) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 5) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 4) ( y-start-charge-station - 5) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 5) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 5) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 5) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 5) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 5) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 5) [ set pcolor yellow]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 5) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 5) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 5) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 5) [ set pcolor 26 ]


  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 6) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 6) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 6) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 6) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 6) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 6) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 6) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 6) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 6) [ set pcolor yellow]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 6) [ set pcolor yellow]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 6) [ set pcolor yellow]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 6) [ set pcolor yellow]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 6) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 6) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 6) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 6) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 7) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 7) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 7) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 7) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 7) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 7) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 7) [ set pcolor yellow]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 7) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 7) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 7) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 7) [ set pcolor 26 ]


  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 8) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 8) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 8) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 8) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 8) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 8) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 8) [ set pcolor yellow]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 8) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 8) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 8) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 8) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 9) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 9) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 9) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 9) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 9) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 9) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 9) [ set pcolor yellow]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 9) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 9) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 9) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 9) [ set pcolor 26 ]

    ask patch (x-start-charge-station + 2) ( y-start-charge-station - 10) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 10) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 10) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 10) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 10) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 10) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 10) [ set pcolor 106]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 10) [ set pcolor 103]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 10) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 10) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 11) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 11) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 11) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 11) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 11) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 11) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 11) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 11) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 11) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 12) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 12) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 12) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 12) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 12) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 12) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 12) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 12) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 12) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 2) ( y-start-charge-station - 13) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 3) ( y-start-charge-station - 13) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 4) ( y-start-charge-station - 13) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 5) ( y-start-charge-station - 13) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 13) [ set pcolor 103 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 13) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 13) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 13) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 13) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 13) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 13) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 13) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 14) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 14) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 14) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 14) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 14) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 14) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 14) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 14) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 15) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 15) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 15) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 15) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 15) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 15) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 15) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 15) [ set pcolor 26 ]

  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 16) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 16) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 16) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 16) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 16) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 16) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 16) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 16) [ set pcolor 26 ]


  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 17) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 17) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 17) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 17) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 17) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 17) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 17) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 17) [ set pcolor 26 ]


  ask patch (x-start-charge-station + 5) ( y-start-charge-station - 18) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 6) ( y-start-charge-station - 18) [ set pcolor 24 ]
  ask patch (x-start-charge-station + 7) ( y-start-charge-station - 18) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 8) ( y-start-charge-station - 18) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 9) ( y-start-charge-station - 18) [ set pcolor yellow ]
  ask patch (x-start-charge-station + 10) ( y-start-charge-station - 18) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 11) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 12) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 13) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 14) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 15) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 16) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 17) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 18) ( y-start-charge-station - 18) [ set pcolor 26]
  ask patch (x-start-charge-station + 19) ( y-start-charge-station - 18) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 20) ( y-start-charge-station - 18) [ set pcolor 26 ]
  ask patch (x-start-charge-station + 21) ( y-start-charge-station - 18) [ set pcolor 26 ]

end


to create-cloud [x-start-cloud y-start-cloud]


  ask patch (x-start-cloud) ( y-start-cloud)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 1) ( y-start-cloud)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 2) ( y-start-cloud)  [ set pcolor 8 ]

  ask patch (x-start-cloud - 1) ( y-start-cloud - 1)  [ set pcolor 8 ]
  ask patch (x-start-cloud) ( y-start-cloud - 1)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 1)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 1)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 1)  [ set pcolor 8]


  ask patch (x-start-cloud - 2) ( y-start-cloud - 2)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 2)  [ set pcolor white ]
  ask patch (x-start-cloud) ( y-start-cloud - 2)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 2)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 2)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 2)  [ set pcolor white ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 2)  [ set pcolor 8]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 2)  [ set pcolor 8]


  ask patch (x-start-cloud - 3) ( y-start-cloud - 3)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 3)  [ set pcolor white ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 3)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 3)  [ set pcolor 8 ]

  ask patch (x-start-cloud - 5) ( y-start-cloud - 4)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 4)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 4)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 4)  [ set pcolor white ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 4)  [ set pcolor 8 ]

  ask patch (x-start-cloud - 8) ( y-start-cloud - 5)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 7) ( y-start-cloud - 5)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 6) ( y-start-cloud - 5)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 5) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 5)  [ set pcolor 9]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 5)  [ set pcolor white ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 5)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 5)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 7) ( y-start-cloud - 5)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 8) ( y-start-cloud - 5)  [ set pcolor 8 ]

  ask patch (x-start-cloud - 9) ( y-start-cloud - 6)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 8) ( y-start-cloud - 6)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 7) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud - 6) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud - 5) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 6)  [ set pcolor white]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 6)  [ set pcolor 9 ]
  ask patch (x-start-cloud) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 6)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 7) ( y-start-cloud - 6)  [ set pcolor white ]
  ask patch (x-start-cloud + 8) ( y-start-cloud - 6)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 9) ( y-start-cloud - 6)  [ set pcolor 8 ]


  ask patch (x-start-cloud - 9) ( y-start-cloud - 7)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 8) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 7) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 6) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud - 5) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 7)  [ set pcolor 9]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 7)  [ set pcolor 9]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud + 7) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud + 8) ( y-start-cloud - 7)  [ set pcolor white ]
  ask patch (x-start-cloud + 9) ( y-start-cloud - 7)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 10) ( y-start-cloud - 7)  [ set pcolor 8 ]



  ask patch (x-start-cloud - 8) ( y-start-cloud - 8)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 7) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 6) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 5) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 8)  [ set pcolor 9]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 8)  [ set pcolor 9]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 7) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 8) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 9) ( y-start-cloud - 8)  [ set pcolor 9 ]
  ask patch (x-start-cloud + 10) ( y-start-cloud - 8)  [ set pcolor 8 ]



  ask patch (x-start-cloud - 7) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 6) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 5) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 4) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 3) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud - 2) ( y-start-cloud - 9)  [ set pcolor 8]
  ask patch (x-start-cloud - 1) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 1) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 2) ( y-start-cloud - 9)  [ set pcolor 8]
  ask patch (x-start-cloud + 3) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 4) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 5) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 6) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 7) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 8) ( y-start-cloud - 9)  [ set pcolor 8 ]
  ask patch (x-start-cloud + 9) ( y-start-cloud - 9)  [ set pcolor 8 ]


end
@#$#@#$#@
GRAPHICS-WINDOW
277
14
944
536
-1
-1
3.644
1
10
1
1
1
0
1
1
1
-90
90
-70
70
1
1
1
ticks
30.0

BUTTON
12
16
76
49
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
188
16
251
49
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
963
15
1197
222
Usage of renewable energy
Hour
Energy (kWh)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Yield" 1.0 0 -13840069 true "" "plotxy (floor ticks / 60 ) renewable-energy-yield-per-hour"
"Consumption" 1.0 0 -16777216 true "" "plotxy (floor ticks / 60 ) (renewable-for-consumption + renewable-to-battery) * 60"

SLIDER
11
496
261
529
buffer-battery-capacity
buffer-battery-capacity
0
6000
4000.0
100
1
kWh
HORIZONTAL

TEXTBOX
282
25
432
43
Hour of the day:
10
9.9
1

PLOT
962
240
1199
430
Current Buffer Battery Level
NIL
% of capacity
0.0
3.0
0.0
100.0
false
false
"" "clear-plot"
PENS
"default" 1.0 1 -13345367 true "" "plotxy 1 (buffer-battery-level / buf-bat-cap ) * 100"

SLIDER
10
164
255
197
renewable-energy-wattpeak
renewable-energy-wattpeak
0
2000
840.0
10
1
max kW
HORIZONTAL

SLIDER
11
118
254
151
number-of-workers
number-of-workers
1
25
7.0
1
1
NIL
HORIZONTAL

SLIDER
12
69
252
102
number-of-jobs-per-hour
number-of-jobs-per-hour
1
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
280
613
426
646
prob-of-working
prob-of-working
0
1
0.23
0.01
1
NIL
HORIZONTAL

PLOT
1219
236
1439
430
Energy sources
NIL
Energy (kWh)
0.0
2.0
0.0
10.0
true
true
"" "clear-plot"
PENS
"Grid" 1.0 1 -7500403 true "" "plotxy 0 grid-for-consumption * 60"
"Battery" 1.0 1 -13345367 true "" "plotxy 1 battery-for-consumption * 60"
"Renewable" 1.0 1 -13840069 true "" "plotxy 2 renewable-for-consumption * 60"

SLIDER
11
304
257
337
energy-consumption
energy-consumption
1
100
10.0
1
1
 (% of battery per hour)
HORIZONTAL

SLIDER
11
351
258
384
charging-speed
charging-speed
0
100
25.0
1
1
(% of battery per hour)
HORIZONTAL

CHOOSER
280
552
424
597
algorithm
algorithm
"fixed probability" "bee-berry"
1

SLIDER
11
209
253
242
renewable-energy-sd
renewable-energy-sd
0
1000
100.0
10
1
kW
HORIZONTAL

SLIDER
11
258
255
291
seasonality-scale
seasonality-scale
0.7
3
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
565
550
737
583
job-threshold
job-threshold
1
25
1.0
1
1
NIL
HORIZONTAL

SLIDER
435
551
551
584
param-energy
param-energy
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
434
590
548
623
param-battery
param-battery
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
435
630
550
663
param-jobs
param-jobs
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
566
631
738
664
frequency
frequency
0
120
15.0
5
1
NIL
HORIZONTAL

PLOT
1219
15
1439
221
#Jobs in queue
Hour
Current number of Jobs
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -7500403 true "" "plotxy floor (ticks / 60) num-jobs - num-active-workers"

SLIDER
11
403
257
436
worker-battery-capacity
worker-battery-capacity
100
1000
500.0
100
1
kWh
HORIZONTAL

SWITCH
12
449
259
482
buffer-battery
buffer-battery
0
1
-1000

SLIDER
567
592
739
625
battery-treshold
battery-treshold
0
100
50.0
10
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?


This model explores the stability of a sustainable electric vehicle transport system. One can imagine a transport system where electric vehicles have to perform jobs (e.g., transport of cargo). Whenever a job is performed, the battery of the vehicle is depleted a bit. Vehicles need to go to the charging every once in a while to recharge their batteries. The energy used for charging is either produced locally – by solar panels and wind turbines – or supplied by the grid. We wish to balance the transport in such a way that the jobs are performed in a timely manner whilst simultaneously trying to use as much as locally produced energy as possible. 


## HOW IT WORKS

The electric vehicles are denoted by “workers”. Workers either perform a job (right-side of the model) or are at the charging area (left-side of the model). Energy is supplied by solar panels, wind turbines, and/or by the grid. Which ‘type of energy’ is currently being supplied, is visualized in the top part of the model. Optionally, one can setup a battery system (i.e., a buffer). This buffer can be used to temporarily store produced energy. The buffer can also be used to charge workers.
Jobs are generated every hour with a fixed and deterministic demand rate and can be set using the parameter NUMBER-OF-JOBS-PER-HOUR. Whenever a worker is assigned to a job, it moves from the charging area (left) to the corresponding job (right). Jobs which are currently being processed are color-coded green. Jobs which are not yet assigned to a worker are grey (i.e., the queue). Every 15 minutes (this is the default seting of the FREQUENCY paramter, but can be adjusted if desired) we make a decision on whether or not to allocate a worker to a job using the following logic.
 
To allocate workers to jobs we deploy two main variations. 

In the first variation, the “fixed-probability” version, every worker has a fixed probability of being assigned to an available job. When there are no jobs available, the worker remains at the charging area. 

In the second variation, the “bee-berry” version, we deploy a bio-inspired AI approach, similarly to the behavior of bees. We identify three components in the system: (i) the sustainable energy sources, (ii) the electric vehicles, and (iii) the jobs. Each component has a ‘natural tendency’ to ‘want’ something, and the logic is as follows. 

The sustainable energy sources ‘want’ to supply as much solar and wind energy currently being produced to the electric vehicles. Of course, they can supply their (surplus) energy to the grid, but we assume that this component wishes to supply as much locally produced energy to the electric vehicles as possible. It goes without saying, that solar energy is produced more during the day than at night. So around noon, this component wants to maximally ‘attract’ vehicles to the charging area to make use of the solar energy being produced. When a battery (i.e, a buffer) is deployed, this component can also supply energy to the battery, such that it can be used at a later point in time to charge the electric vehicles (e.g., at night). 

The electric vehicles ‘want’ to have a full battery. When there is no charge left, they cannot perform jobs. When the battery of a vehicle is fully charged, there is no stimulus to go to the charger. However, when the battery level is low, the charging area should attract the electric vehicles.  

The jobs ‘want’ to be executed as quick as possible. When there are no jobs in the queue, everything is fine. However, when the queue builds up, the jobs try to attract workers (i.e., electric vehicles). 

Each of the three components generates a probability between 0 and 1. The probability of assigning a worker to a job is determined by combining the probabilities of these three components in a linear fashion, and the weights of each component can be controlled using the parameters PARAM-ENERGY, PARAM-BATTERY, and PARAM-JOBS.


## HOW TO USE IT

1.	Set the algorithm to either “fixed-probability” or “bee-berry”. 
2.	Adjust the slider parameters (see below), or use the default settings
3.	Press the SETUP button. 
4.	Press the GO button to begin the simulation. 
5.	Look at the screen to see the current situation (updated every hour)
6.	Look at the different monitors to study the system

Parameters:
NUMBER-OF-JOBS-PER-HOUR: The exact number of jobs generated every 60 minutes.
 
NUMBER-OF-WORKERS: The number of workers (i.e., electric vehicles) in the system. 

RENEWABLE-ENERGY-WATTPEAK: The maximum energy that can be produced locally (solar + wind). We assume that the energy production follows a Normal distribution over the day. This parameter thus denotes mu. 

RENEWABLE-ENERGY-SD: The standard deviation of the local energy production. This parameters thus denotes sigma of our Normal distribution. 

SEASONALITY-SCALE: Determines when the sun rises and sets. It scales the Normal distribution discussed above. 

ENERGY-CONSUMPTION: The hourly energy consumption of an electric vehicle, expressed in %.  This is also the minimum required charge required for a vehicle to perform a job. If the current charge of a vehicle is below this threshold, it is not eligible to be assigned to a job. 

CHARGING-SPEED. The hourly charging speed of the electric vehicles, expressed in %. 

WORKER-BATTERY-CAPACITY. The maximum capacity of an electric vehicle, expressed in kWh. 

BUFFER-BATTERY. Toggle switch to determine whether or not the system has a central battery to temporarily store locally generated energy. 

BUFFER-BATTERY-CAPACITY. The capacity of the buffer battery, expressed in kWh. 

ALGORITHM. Determines whether the model uses the ‘fixed-probability’ algorithm or the ‘bee-berry’ algorithm. 

PROB-OF-WORKING. This parameters denotes the fixed probability used when the ‘fixed-probability’ algorithm is used. 

PARAM-ENERGY. Denotes the relative important of the ‘will’ of the sustainable energy sources when the ‘bee-berry’ algorithm is used. This parameters together with PARAM-BATTERY and PARAM-JOBS should sum up to 1. 

PARAM-BATTERY. Denotes the relative important of the ‘will’ of the battery of the electric vehicles when the ‘bee-berry’ algorithm is used. This parameters together with PARAM-ENERGY and PARAM-JOBS should sum up to 1. 

PARAM-JOBS. Denotes the relative important of the ‘will’ of the jobs when the ‘bee-berry’ algorithm is used. This parameters together with PARAM-ENERGY and PARAM-ENERGY should sum up to 1. 

JOB-THRESHOLD. Sets the threshold value for the activation function of the ‘will’ of the jobs. Note: when the number of jobs in the queue is equal to this threshold, the probability is 50%. 

BATTERY-THRESHOLD. Sets the threshold value for the activation function of the ‘will’ of the battery of the electric vehicle. Note: when the current charge is equal to this threshold, the probability is 50%. 

FREQUENCY. This parameters sets how often the algorithm is evaluated expressed in minutes. Default is 15 minutes. 

There are four monitors to show the current state of the system. 
1.	The usage of renewable energy (yield per hour and the actual consumption per hour)
2.	The number of jobs in the queue (i.e., jobs who are not assigned to a worker)
3.	The fill-rate of the buffer battery (only if used)
4.	The distribution on the current energy used, i.e., where does the current energy consumption originates from (local renewable sources, the battery, or the grid). 

## THINGS TO NOTICE

This simulation model is suitable to play around with and test different settings in the cloud. An extended (offline) version of this model was used to perform experiments with multiple replications and analyse the results to compare the outcomes on statistical differences. One can for example implement the Excel-extension of NetLogo to log simulation results. 

The top left corner of the screen shows the current hour of the day (updated every 60 ticks). That is, every tick is equal to one minute. 

When the battery of a worker if full, it remains at the charging area.

Default setting: decisions on allocating workers to jobs is done every 15 minutes (i.e., every 15 ticks). Can be controlled using the parameter FREQUENCY. 

## THINGS TO TRY

DEFAULT SETTINGS: NUMBER-OF-JOBS-PER-HOUR = 5, WORKERS = 7, RENEWABLE-ENERGY-WATTPEAK = 840 kW, RENEWABLE-ENERGY-SD = 100 kW, SEASONALITY-SCALE = 0.7, ENERGY-CONSUMPTION = 10%, CHARGING-SPEED = 25%, WORKER-BATTERY-CAPACITY = 500 kWh, BUFFER-BATTERY = ON, BUFFER-BATTERY-CAPACITY = 4000 kWh, ALGORITHM = “bee-berry", JOB-THRESHOLD = 1, BATTERY-THRESHOLD = 50, FREQUENCY = 15. 

With this setting there is a balance between arriving jobs and processing of jobs (including the charging time). Also, there is a balance between KWh needed to process all jobs (6000 kWh per day) and the energy yield from the renewable resources. With this setting, the focus from the energy yield perspective is on the solar energy, i.e., most yield during day hours.

Increasing the SEASONALITY-SCALE, shifts the production of renewable energy to wind energy, i.e., the yield during night hours will increase. See how the yield changes in the upperleft chart when changing this parameter. Also, RENEWABLE-ENERGY-WATTPEAK and RENEWABLE-ENERGY-SD influence the renewable energy yield. 

Play around with the PARAM-ENERGY, PARAM-BATTERY & PARAM-JOBS (make sure they sum up to 1) and see what different settings do to the systems. With a high PARAM-ENERGY, workers want to charge when the renewable energy production is high, i.e., during day hours. Also, try increasing JOB-THRESHOLD and see how the job queue increases (only in combination with a high PARAM-JOBS). Changing the BATTERY-THRESHOLD in combination with a high PARAM-BATTERY influences the moment a worker decides to go to the charger based on its battery level and indirect the job queue. Note that, changing the algorithm from “bee-berry" to "fixed-probability" makes the PARAM-ENERGY, PARAM-BATTERY, PARAM-JOBS, BATTERY-THRESHOLD and JOB-THRESHOLD independent for the decision for a worker to go to the charging station or performing a job.

See that switching off the BUFFER-BATTERY and keeping the rest of the default settings the same, it is difficult to be self-sufficient in terms of usage of own (renewable) energy production, i.e., energy from the grid is needed to perform all jobs. What is needed to become more self-sufficient in this case?

## EXTENDING THE MODEL

PRICING MECHANISM. Inserting the buying and selling price (as adjustable parameters) of energy could be interesting, since costs becomes a KPI and a new challenge arises to configure a cost-efficient energy system. Also, dynamic pricing can also lead to different decisions, e.g.,charge the buffer battery with energy from the grid when the buying price is low, or sell stored energy from the buffer battery when the selling price is high.

STOCHASTIC JOB ARRIVALS. Extending the model with stochastic job arrivals will influence the job queue and may also show that certain configurations are more robust against unpredictable job arrivals (or peak hours) than others. 

CAPACITATED CHARGING SPOTS. In this model, we assume that a worker can always go the charger when it wants to. However, if there are less charging spots than workers, not all workers can charge at the same time, which makes the decision to go to the charging area of perform a job more complex.

## NETLOGO FEATURES

Later...

## RELATED MODELS

Later...

## HOW TO CITE 
If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.
For the model itself:

•	Gerrits, B., and Andringa, R. (2022). Bio-inspired agent-based simulation model for a local energy management system with electric vehicles . INSERT LINK TO MODEL. Distribute, Enschede, The Netherlands.

Please cite the NetLogo software as:
•	Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.


## CREDITS AND REFERENCES

Copyright 2022 Berry Gerrits
 
This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
Commercial licenses are also available. To inquire about commercial licenses, please contact Berry Gerrits at b.gerrits@distribute.company
This model was created as part of the RAAK-KIEM project: INGENIOUS. 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
