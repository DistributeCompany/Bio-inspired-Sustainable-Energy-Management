;REMARK: from version 12 the model has the csv extension to write the outputs to a CSV file
;This is (probably) not working in the web version

breed [jobs job ]
breed [workers worker]

jobs-own[
 assigned-worker
 created-at
 processed-at
 queue-grid-position
]

workers-own[
 worker-battery-level
 active?
 current-job
 charging-x-cor
 charging-y-cor
]

globals[
 hour; hour of the day
 stimulus-solar
 prop-current-worker

 buffer-battery?
 buf-bat-cap

 ;all x and y coordinates are set in init-visuals
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

 horizon-y-pos;

 buffer-battery-level ;buffer

 x-cor-workers-charging
 y-cor-workers-charging

 x-cor-workers-working
 y-cor-workers-working

 ;patch location of first job in queue (initilized at setup)
 x-cor-jobs
 y-cor-jobs

 x-cor-current-job
 y-cor-current-job

 num-jobs
 num-active-workers

 ;inputs for size of job-queue
 queue-x ;size of queue in x-direction
 queue-y ;size of queue in y-direction
 queue-increment ;increment of pixels (both in x- and y direction)

 jobs-on-last-grid-pos


 price-mechanism? ;if this is true, it is possible to supply from battery to grid and grid to battery based on the price,
  ;or to supply from renewable sources to grid and not use it for the charging station although there might be demand
  ; however, this is future work (TODO)

 grid-power-to-battery?
 battery-power-to-grid?
 renewable-power-to-grid?
 renewable-power-to-battery?
 renewable-power-to-charging-station?
 battery-power-to-charging-station?
 grid-power-to-charging-station?


 renewable-energy-yield ;current production per MINUTE (=tick) in kWm
 renewable-energy-yield-per-hour ;current production per hour in kWh

 charging-demand


 renewable-for-consumption ;share of energy consumption for charging station per tick
 battery-for-consumption
 grid-for-consumption
 renewable-to-battery
 renewable-to-grid

 ;globals below are used for the output file. Every 30 ticks the averages are calculated and stored in the output-list
 avg-production
 avg-use-of-production
 avg-use-of-battery
 avg-use-of-grid
 avg-renewable-to-grid
 avg-renewable-to-battery

 avg-charging
 avg-working
 avg-idle

 avg-queue


 output-list

 wind-energy? ;only used for visualization of energy stream
 solar-energy? ;only used for visualization of energy stream


run-experiments?
current-run

]

patches-own[
  power-cable
  power;visualizes power through power cable
  queue-number
]



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;setup;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup
  clear-all

  set run-experiments? false

  ;init output list
  set output-list [["hour" "production" "use of own production" "use of battery" "use of grid" "battery fill rate" "to battery" "to grid" "working (%)" "charging (%)" "idling (%)" "queue"]]

  set buffer-battery? buffer-battery

  ifelse buffer-battery? 
  [set buf-bat-cap buffer-battery-capacity ]
  [set buf-bat-cap 1] ;to prevent division by zero

  ;note; price-mechanism -> True for future work
  set price-mechanism? False

  set grid-power-to-battery? False
  set battery-power-to-grid? False
  set renewable-power-to-grid? False
  set renewable-power-to-battery? False
  set renewable-power-to-charging-station? False
  set battery-power-to-charging-station? False
  set grid-power-to-charging-station? False


  set hour 1
  set buffer-battery-level 0
  set-hour

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


  set wind-energy? True
  set solar-energy? False

  ;init visualisation of power elements on screen
  init-visuals

  ;set of solar power yield over the day (24h)
  ;TODO: make percentages of Watt piek (according to some Normal-distribution)



  ;init workers
  setup-workers

  ;set active to false

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
  ;determine total size of queue (i.e., number of jobs that the queue can store)
  let grid-size (queue-x * queue-y)
  let counter 1
  let current-x x-cor-jobs
  let current-y y-cor-jobs
  let column 1

  while [column <= queue-x][

    ifelse counter mod (queue-y + 1) != 0 [
      ask patches with [pxcor = current-x and pycor = (current-y + ((counter - 1) * queue-increment))]
      [
        ;show patch-at current-x (current-y + ((counter - 1) * queue-increment))
        set queue-number ((column - 1) * queue-y) + counter
        ;set plabel queue-number
       ; show patch-at (x-cor-jobs + (counter * queue-increment)) (y-cor-jobs + (counter * queue-increment))
      ]

    ]
    [set current-x (current-x + queue-increment)
     set current-y y-cor-jobs
     set column column + 1
     set counter 0]

   set counter counter + 1
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


  if param-energy + param-battery + param-jobs != 1
  [user-message ("Make sure that the sum of param-energy, param-battery and param-jobs is equal to 1. Click halt, reconfigure the parameters, and then click the GO button again.")]

 ; every tick = 1 minute, set new hour after 60 ticks
  if ticks mod 60 = 0 and ticks > 0 [
    if hour = 24 [ set hour 0] ;reset hour of day after 24 hours
    set hour hour + 1
    set-hour

    calculate-renewable-energy-yield

    ;init jobs
    create-new-jobs
    update-visuals

    ifelse hour > 6 and hour < 21 [
      set solar-energy? True
    ]
    [
      set solar-energy? False
    ]
  ]





  ;if no solar production, but vehicles require charging:
  ;first, check whether battery buffer can be used
  ;TODO: seems like a common-sense static rule, but a dynamic rule might also be useful, e.g., when grid-power is currently cheaper than 'using the battery')
  ;second, if battery buffer is depleted, call receive-grid-energy procedure


  ;update battery levels of workers (battery depletes only if active)
   ask workers [
     if active? = True [
       set worker-battery-level (worker-battery-level - (energy-consumption *(1 / 60)))
      if (worker-battery-level < min-charge) [set color yellow]
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
      if (worker-battery-level < min-charge) [set color yellow]
      if (worker-battery-level > min-charge) [set color green]
       if worker-battery-level > 100 [
         set worker-battery-level 100]
       set label round(worker-battery-level)
      set charging-demand charging-demand + worker-battery-capacity * ((charging-speed / 100) / 60)


     ]
   ]


  ;check if jobs are finished at current tick
  if any? jobs with [processed-at + 60 = ticks AND processed-at > -1][
    finish-job
  ]


  ;assign workers to jobs, if not all workers are already active
  ;TODO: might occur that worker IS active, but battery is depleted?
  if any? workers with [active? = false][
    assign-workers-to-jobs]

  ;calculate the power streams
  calculate-power-streams charging-demand

  ;visualizes the power streams based on the demand from the charging station
  visualize-power-streams

  ;set num-jobs
  set num-jobs count jobs

  ;update global variable indicating number of active worker
  set num-active-workers count workers with [active? = True]



 ;increase tick
 tick

  ;1 tick = 1 minute (i.e., every thick equals 1/60th of the solar-power-yield of that hour, and may be added to the battery-level)
  ;TODO: replace global variable vehicle-charging with run-time variable that determines whether buffer battery receives charging

  if run-experiments? [go]

end




to calculate-renewable-energy-yield
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


  ; set yield per minute (tick)
  set renewable-energy-yield-per-hour (p-i-green * renewable-energy-wattpeak)
  set renewable-energy-yield renewable-energy-yield-per-hour / 60

  ;show renewable-energy-yield


end



to calculate-power-streams [demand-quantity]
  ; function that calculates power streams based on the ademand from the charging station and supply from wind and solar energy
  ; also sets global variables that are uses to visualize the power streams in visualize-power-streams

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
  ;init the jobs of the first hour

  foreach n-values number-of-jobs-per-hour [i -> i] [x ->
  ;  let counter i
    let y count jobs

    if y < (queue-x * queue-y) [

    ask patches with [queue-number = (y + 1)][
      sprout-jobs 1 [
      set size 5
      set shape "box"
      set color grey
      ;set num-jobs num-jobs + 1 ;global variable indicating total number of jobs currently active
      set created-at ticks
      set assigned-worker -1
      set processed-at -1
      set queue-grid-position queue-number]
      ]
    ]

    if y >= (queue-x * queue-y) [
    ask patches with [queue-number = (queue-x * queue-y) ] [
      sprout-jobs 1 [
      set size 5
      set shape "box"
      set color grey
      ;set num-jobs num-jobs + 1 ;global variable indicating total number of jobs currently active
      set created-at ticks
      set assigned-worker -1
      set processed-at -1
      set queue-grid-position queue-number]

      set jobs-on-last-grid-pos count jobs-here
      ]
    ]
  ]

  ;foreach (n-values number-of-jobs-per-hour [i -> 5 * i + y-cor-jobs]) [ x ->
  ;  ask patch x-cor-jobs x [
  ;    sprout-jobs 1 [ set size 5
  ;      set shape "box"
  ;      set color grey
  ;      set num-jobs num-jobs + 1 ;global variable indicating total number of jobs currently active
  ;      set created-at 0
  ;      set assigned-worker -1
  ;    ]
  ;  ]
  ;]
end

;at every tick -> check if worker will process job (transition from passive to active)
to assign-workers-to-jobs
  if ticks mod frequency = 0[

  ask workers with [worker-battery-level > min-charge][
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
        ;CALCULATE PROBABILITY OF GOING TO WORK BASED ON BEE-BERRY
        ;There are three elements: (1) power of the sun, (2) power of the battery, and (3) power of the jobs
        ;1. The sun provides a stimulus for workers to go to the charger. The more sun (and wind) currently available, the higher the stimulus. But also the time of day has influence.
        ;2. The battery of the vehicle provides a stimulus to go the charger. The lower the battery-level, the higher the stimulus
        ;3. The jobs that need to be processed provide an ANTI-stimulus to go the charger. The higher the # of current jobs, the higher the ANTI-stimulus
        ;The strength of each elements can be controlled via element-specific parameters
        ;All in all, this 'getouwtrek' should lead to a probability between 0 and 1 which determines for each worker whether or not to go charger (also when already AT the charger)

        ;reset probability
        set prop-current-worker 0
        ;show prop-current-worker
        ;STEP 1: Solar stimulus
        ;Get fitness of current hour of sun production (= % of maximum yield)
        ;let current-sun-fitness (current-solar-power / solar-energy-yield )
        set stimulus-solar (renewable-energy-yield * 60 ) / renewable-energy-wattpeak
        set prop-current-worker prop-current-worker + (param-energy * stimulus-solar)


        ;let current-hour-relative-to-noon ((hour - 12) / 12)

        ;ifelse current-hour-relative-to-noon < 0[
          ; BEFORE noon
         ; set stimulus-solar current-sun-fitness + (param-hour-of-the-day * current-hour-relative-to-noon)

          ;if stimulus-solar < 0 [set stimulus-solar 0]
          ;show stimulus-solar
        ;  set prop-current-worker prop-current-worker + (param-energy * stimulus-solar)
        ;]
        ;[; AFTER NOON
        ;  set stimulus-solar current-sun-fitness + (param-hour-of-the-day * current-hour-relative-to-noon)

         ; if stimulus-solar > 1 [set stimulus-solar 1]
          ;show stimulus-solar
         ; set prop-current-worker prop-current-worker + (param-energy * stimulus-solar)
        ;]
        ;STEP 2: battery stimulus
        ;show worker-battery-level
        ;let stimulus-battery (1 / worker-battery-level)
        let stimulus-battery 1 - (worker-battery-level ^ 2) / (worker-battery-level ^ 2 + battery-treshold ^ 2)
        set prop-current-worker prop-current-worker + (param-battery * stimulus-battery)

        ;show "worker battery"
        ;show worker-battery-level
        ;show "stimulus battery"
        ;show prop-current-worker

        ;show prop-current-worker
        ;STEP 3: job-queue stimulus
        let waiting-jobs (num-jobs - num-active-workers)
        let stimulus-jobs (waiting-jobs ^ 2 / (waiting-jobs ^ 2 + job-threshold ^ 2)) ;REMARK: power = 2
        set prop-current-worker prop-current-worker + (param-jobs * (1 - stimulus-jobs))

        ;show "waiting jobs"
        ;show waiting-jobs
        ;show "stimulus jobs"
        ;show stimulus-jobs
        ;final probability of going to the charging

        let rand-number random 100
        ;show "random number"
        ;show rand-number
        ;show "probability worker"
        ;show 100 * (1 - prop-current-worker)
        ;draw random number to determine whether working is going to work (= 1 minus probability of going to charger)
        if rand-number < (100 * (1 - prop-current-worker)) and active? = False [

          ;first sort jobs who still need to be processed on 'who' (=uniqueID, higher the number the later the worker is created)
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

      ]
    ]
  ]
]

end


;prodecure to determine the hour of the day
to set-hour
    ask patch -67 67 [set plabel hour] ;print hour of the day on patch  ;HARDCODE
end



;remove job after it has been processed
to finish-job

  let jobs-to-finish sort-on [who] jobs with [processed-at + 60 = ticks]
  foreach jobs-to-finish
  [job-to-finish -> ask job-to-finish [
    ask worker assigned-worker [
      set active? False
     ; set num-active-workers num-active-workers - 1
      set current-job -1
      setxy charging-x-cor charging-y-cor
    ]
    die]
    re-order-joblist
  ]


end

;after finished jobs are removed, re-order jobs on queue grid
to re-order-joblist

  ask jobs with [queue-grid-position < 25][
    set-job-to-queue self queue-grid-position - 1
  ]



  ; patch of last position in job queue
  ask patch (x-cor-jobs + (queue-x - 1) * queue-increment) (y-cor-jobs + (queue-y - 1) * queue-increment)  [ ;HARDCODE WILL NEVER DIE!!!
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
;;;;;;;;;;;;;;visuals;;;;;;;;;;;;;;;;;
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
    if price-mechanism? [
      create-power-cable 9 -5 9 9 "x" "from-battery-to-grid"
      create-power-cable -8 15 -8 11 "y" "from-grid-to-battery"
    ]
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
