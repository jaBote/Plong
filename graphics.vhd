library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity graphics is
    port(
        clk, not_reset: in  std_logic;
		ctl_start, ctl_left, ctl_right: in  std_logic; -- ctl_start will also launch the ball
        px_x, px_y: in  std_logic_vector(9 downto 0);
        video_on: in  std_logic;
        rgb_stream: out std_logic_vector(2  downto 0);
        ball_bounced: out std_logic;
        ball_missed: out std_logic
    );
end graphics;

architecture dispatcher of graphics is
    constant SCREEN_WIDTH: integer := 640;
    constant SCREEN_HEIGHT: integer := 480;

    type game_states is (start, waiting, ball_start, playing, player_won, game_over);
    signal state, state_next: game_states;
	signal win_flag: std_logic;

    signal ball_control_counter,
           ball_control_counter_next: std_logic_vector(17 downto 0);

    -- counts how many times the ball hits the bar
    -- used for nothing in particular
    signal bounce_counter, bounce_counter_next: std_logic_vector(7 downto 0);

    signal score_1, score_1_next: std_logic_vector(5 downto 0);
    signal cur_lives, cur_lives_next: std_logic_vector(5 downto 0);
	constant MAX_LIVES: integer := 3;

    signal score_on: std_logic;
    signal current_score: std_logic_vector(5 downto 0);
    signal score_font_addr: std_logic_vector(8 downto 0);

    signal message_on: std_logic;
    signal message_font_addr: std_logic_vector(8 downto 0);

    signal font_addr: std_logic_vector(8 downto 0);
    signal font_data: std_logic_vector(0 to 7);
    signal font_pixel: std_logic;
    signal font_rgb: std_logic_vector(2 downto 0);

    constant BALL_SIZE: integer := 16; -- ball is square
    signal ball_enable: std_logic;
    signal ball_addr: std_logic_vector(3 downto 0);
    signal ball_px_addr: std_logic_vector(3 downto 0);
    signal ball_data: std_logic_vector(0 to BALL_SIZE - 1);
    signal ball_pixel: std_logic;
    signal ball_rgb: std_logic_vector(2 downto 0);
    signal ball_x, ball_x_next: std_logic_vector(9 downto 0);
    signal ball_y, ball_y_next: std_logic_vector(9 downto 0);
	signal ball_dx, ball_dy: integer;
	signal ball_stuck, ball_stuck_next: std_logic;

    signal ball_h_dir, ball_h_dir_next, ball_v_dir, ball_v_dir_next: std_logic;

    signal ball_bounce, ball_miss: std_logic;

    constant BAR_1_POS: integer := SCREEN_HEIGHT - 20; -- This is now the vertical height the bar starts in (bottom of screen)

    constant BAR_WIDTH: integer := 128;
    constant BAR_HEIGHT: integer := 20;

    signal bar_pos: integer;
    signal bar_addr: std_logic_vector(4 downto 0);
    signal bar_data: std_logic_vector(0 to BAR_WIDTH - 1);
    signal bar_pixel: std_logic;
    signal bar_rgb: std_logic_vector(2 downto 0);
    signal bar_1_x, bar_1_x_next: std_logic_vector(9 downto 0);
	
	-- We need this type for the bricks array...
	constant BRICK_ROWS: integer := 3;-- 5;
	constant BRICK_COLS: integer := 4;-- 13;
	type brick_matrix is array (BRICK_ROWS-1 downto 0, BRICK_COLS-1 downto 0) of std_logic; -- Original game matrix is 5 rows by 13 columns
	
	constant BRICK_WIDTH: integer := 40;
	constant BRICK_HEIGHT: integer := 20;
	constant BRICK_START_POS_X: integer := 60;
	constant BRICK_START_POS_Y: integer := 100;
	constant BRICK_MAX: integer := BRICK_ROWS * BRICK_COLS; 
	constant BRICK_START: brick_matrix := -- Possibly unneeded?
		(
		   ('1','1','1','1'),('1','1','1','1'),('1','1','1','1')
--			('1','1','1','1','1','1','1','1','1','1','1','1','1'),
--			('1','1','1','1','1','1','1','1','1','1','1','1','1'),
--			('1','1','1','1','1','1','1','1','1','1','1','1','1'),
--			('1','1','1','1','1','1','1','1','1','1','1','1','1'),
--			('1','1','1','1','1','1','1','1','1','1','1','1','1')
		);
--	constant BRICK_END: brick_matrix :=  -- Possibly unneeded too?
--		(
--			('0','0','0','0','0','0','0','0','0','0','0','0','0'),
--			('0','0','0','0','0','0','0','0','0','0','0','0','0'),
--			('0','0','0','0','0','0','0','0','0','0','0','0','0'),
--			('0','0','0','0','0','0','0','0','0','0','0','0','0'),
--			('0','0','0','0','0','0','0','0','0','0','0','0','0')
--		);

	signal brick_addr: std_logic_vector(4 downto 0);
	signal brick_data: std_logic_vector(0 to BRICK_WIDTH - 1);
	signal brick_pixel: std_logic;
	signal brick_rgb: std_logic_vector(2 downto 0);
	signal brick_array: brick_matrix; -- Will set the bricks
	signal brick_count, brick_count_next: integer; -- Will count number of broken bricks
	signal brick_broken_row, brick_broken_col: integer;
	signal brick_init: std_logic;
	signal ball_on, bar_on, brick_on: std_logic;
begin

	process(state, ball_y, ctl_start, ctl_left, ctl_right, cur_lives)
	begin
		state_next <= state;
		ball_enable <= '0';
		ball_miss <= '0';
		cur_lives_next <= cur_lives;

		case state is
			when start =>
				cur_lives_next <= conv_std_logic_vector(MAX_LIVES,6);
				brick_init <= '0';
				state_next <= waiting;
			when waiting =>
				ball_enable <= '0';
				ball_stuck_next <= '1';
				if cur_lives = 0  then
					state_next <= game_over;
				elsif (ctl_start = '1') then
					if (cur_lives = MAX_LIVES) then
						brick_init <= '1';
					end if;
					state_next <= ball_start;
					end if;
			when ball_start =>
				brick_init <= '0';
				ball_enable <= '1';
				if (ctl_start = '1') then
					state_next <= playing;
				end if;				
			when playing =>
				ball_enable <= '1';
				ball_stuck_next <= '0';
				if ball_y = SCREEN_HEIGHT - BALL_SIZE then
					-- player 1 loses a live. Y = SCREEN_HEIGHT is the bottom of the screen
					cur_lives_next <= cur_lives - 1;
					state_next <= waiting;
					ball_miss <= '1';
				elsif win_flag = '1' then
					state_next <= player_won;
				end if;
			when game_over =>
				if (ctl_start = '1') then
					state_next <= start;
				end if;
			when player_won => -- To acknoledge someone has won, but the game is over anyways
--				if (ctl_start = '1') then
--					state_next <= start;
--              end if;
				state_next <= game_over;
		end case;
	end process;

	process(clk, not_reset)
	begin
		if not_reset = '0' then
			state <= start;
			ball_x <= (others => '0');
			ball_y <= (others => '0');
			bar_1_x <= conv_std_logic_vector(SCREEN_WIDTH / 2 - BAR_WIDTH / 2, 10);
			ball_h_dir <= '0';
			ball_v_dir <= '0';
			bounce_counter <= (others => '0');
			ball_control_counter <= (others => '0');
			ball_stuck <= '0';
			score_1 <= (others => '0');
			cur_lives <= (others => '0');
			brick_count <= BRICK_MAX;
--			brick_array <= BRICK_END;
		elsif clk'event and clk = '0' then
			state <= state_next;
			ball_x <= ball_x_next;
			ball_y <= ball_y_next;
			bar_1_x <= bar_1_x_next;
			ball_h_dir <= ball_h_dir_next;
			ball_v_dir <= ball_v_dir_next;
			bounce_counter <= bounce_counter_next;
			ball_control_counter <= ball_control_counter_next;
			ball_stuck <= ball_stuck_next;
			score_1 <= score_1_next;
			cur_lives <= cur_lives_next;
			brick_count <= brick_count_next;
		end if;
	end process;

	score_on <= '1' when px_y(9 downto 3) = 1 and
                        (px_x(9 downto 3) = 42 or px_x(9 downto 3) = 37) else
               '0';
	-- We used the previous score_2 signal to store lives
	current_score <= score_1 when px_x < 320 else cur_lives;
	-- numbers start at memory location 128
	-- '1' starts at 136, '2' at 144 and so on
	score_font_addr <= conv_std_logic_vector(128, 9) +
                       (current_score(2 downto 0) & current_score(5 downto 3));

	message_on <= '1' when state = game_over and
                           -- message on the center of the screen
                           ( px_x(9 downto 3) >= 52 and
                             px_x(9 downto 3) < 66 and
                             px_y(9 downto 3) = 29 ) else
                 '0';
    
	with px_x(9 downto 3) select
        message_font_addr <= "100111000" when "0110100", -- G
                             "100001000" when "0110101", -- A
                             "101101000" when "0110110", -- M
                             "100101000" when "0110111", -- E
                             "000000000" when "0111000", -- SPACE
                             "101111000" when "0111001", -- O
                             "110110000" when "0111010", -- V
                             "100101000" when "0111011", -- E
                             "110010000" when "0111100", -- R
                             "000001000" when "0111101", -- !
                             "000000000" when others;

    -- font address mutltiplexer
    font_addr <= px_y(2 downto 0) + score_font_addr when score_on = '1' else
                 px_y(2 downto 0) + message_font_addr when message_on = '1' else
                 (others => '0');
    font_pixel <= font_data(conv_integer(px_x(2 downto 0)));
    font_rgb <= "000" when font_pixel = '1' else "111";

    direction_control: process(
        not_reset,
		ball_control_counter,
        ball_x, ball_y,
        ball_h_dir, ball_v_dir,
        ball_h_dir_next, ball_v_dir_next,
        bar_1_x
    )
    begin
        ball_h_dir_next <= ball_h_dir;
        ball_v_dir_next <= ball_v_dir;
        ball_bounce <= '0';

		-- Changed most of this process code since now our bar is horizontal
		
        --
        -- BEWARE! Looks like ball_bounce signal is generated twice
        -- due to slower clock! Too lazy to fix now :D
        --
		if not_reset = '0' then
			-- This way, when enabled, ball will start ascending to left at 45 degrees
			ball_dx <= 1;
			ball_dy <= 1;
			ball_h_dir_next <= '1';
			ball_v_dir_next <= '0';
		elsif ball_stuck = '1' then
			-- This way, when unstuck, ball will start ascending to left at 45 degrees
			ball_dx <= 1;
			ball_dy <= 1;
			ball_h_dir_next <= '1';
			ball_v_dir_next <= '0';
		elsif ball_control_counter = 0 then
			if ball_y = BAR_1_POS - BAR_HEIGHT and
            ball_x + BALL_SIZE > bar_1_x and
            ball_x < bar_1_x + BAR_WIDTH then
                ball_v_dir_next <= '0';
                ball_bounce <= '1';
				-- Now that we know ball bounces we can make some gross estimations...
				-- It's also grossly unadvised for performance's sake, but I won't pre-make any divisions for clarity's sake
				-- First 10% is rebound 30 degrees to left.
				if (conv_integer(ball_x) + BALL_SIZE / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 10 ) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 2;
					ball_dy <= 1;
				-- Then 15% for rebound 45 degrees to left.
				elsif (conv_integer(ball_x) + BALL_SIZE / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 25) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 1;
					ball_dy <= 1;
				-- Then 25% for rebound 60 degrees to left.
				elsif (conv_integer(ball_x) + BALL_SIZE / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 50) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 1;
					ball_dy <= 2;
				-- Then vertical rebound if you're lucky enough. dy is set to 2 for preventing the ball to appear to be moving too slow with this
				elsif (conv_integer(ball_x) + BALL_SIZE / 2) = (conv_integer(bar_1_x) + (BAR_WIDTH * 50) / 100) then
					ball_dx <= 0;
					ball_dy <= 2;
				-- Then 25% for rebound 60 degrees to right.
				elsif (conv_integer(ball_x) + BALL_SIZE / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 75) / 100) then
					ball_h_dir_next <= '1';
					ball_dx <= 1;
					ball_dy <= 2;
				-- Then 15% for rebound 45 degrees to right.
				elsif (conv_integer(ball_x) + BALL_SIZE / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 90) / 100) then
					ball_h_dir_next <= '1';
					ball_dx <= 1;
					ball_dy <= 1;
				-- Then 10% for rebound 30 degrees to right. Rest of the bar
				else
					ball_h_dir_next <= '1';
					ball_dx <= 2;
					ball_dy <= 1;
				end if;
			-- Ball hits the top of screen
			elsif ball_y = 0 then
				ball_v_dir_next <= '1';
			-- Ball is way too late for being saved but you hit it with any of the sides of the bar
			elsif ball_y < BAR_1_POS + BAR_WIDTH and ball_y + BALL_SIZE > BAR_1_POS then
				if ball_x + BALL_SIZE > BAR_1_POS then
					ball_h_dir_next <= '0';
				elsif ball_x = bar_1_x + BAR_WIDTH then
					ball_h_dir_next <= '1';
				end if;
			end if;
		
			-- Collision with left or right edge of screen
			if ball_x = 0 then
			ball_h_dir_next <= '1';
			elsif ball_x = SCREEN_WIDTH - BALL_SIZE then
				ball_h_dir_next <= '0';
			end if;
			
			-- Collision with brick. Will only use the center of the ball in case of doubt between two blocks
--			for i in 0 to BRICK_ROWS - 1 LOOP
--				for j in 0 to BRICK_COLS - 1 LOOP
--					if brick_array(i,j) = '1' then -- if brick exists...
--					
--						-- Two separate ifs because if you hit a block on its corner you'd hit both sides at the same time
--						-- I know you could sum both of them up to a only 1 monstruous if comparison, but
--						-- that would crazily harm readability.
--
--						-- Vertical collisions: both from the top or the bottom of the brick
--						if ball_y + BALL_SIZE = BRICK_START_POS_Y + (i * BRICK_HEIGHT) or -- Top of the brick
--							ball_y = BRICK_START_POS_Y + (i * BRICK_HEIGHT) then -- Bottom of the brick
--							if ball_x + BALL_SIZE >= BRICK_START_POS_X + (j * BRICK_WIDTH) and 
--								ball_x < BRICK_START_POS_X + ((j+1) * BRICK_WIDTH) then -- Ball hits the brick somewhere
--								if ball_x < BRICK_START_POS_X + (j * BRICK_WIDTH) then -- Ball overflows to the left
--									if j = 0 or -- First column: immediate bump
--										brick_array(i, j-1) = '0' or -- No brick to the left: immediate bump
--										ball_x + (BALL_SIZE/2) >= BRICK_START_POS_X + (j * BRICK_WIDTH) then -- Intermediate column: check if its center is in this brick
--										brick_broken_row <= i;
--										brick_broken_col <= j;
--										ball_v_dir_next <= not ball_v_dir_next;
--									-- No need for else since the other block will be hit in the next iteration
--									end if;
--								elsif ball_x + BALL_SIZE > BRICK_START_POS_X + ((j+1) * BRICK_WIDTH) then -- Ball overflows to the right
--									if j = BRICK_COLS - 1 or -- Last column: immediate bump.
--										brick_array(i, j+1) = '0' or -- No brick to the right: immediate bump
--										ball_x + (BALL_SIZE/2) <= BRICK_START_POS_X + ((j+1) * BRICK_WIDTH) then -- Intermediate column: check if its center is in this brick
--										brick_broken_row <= i;
--										brick_broken_col <= j;
--										ball_v_dir_next <= not ball_v_dir_next;
--									end if;
--								else -- Ball is all on the brick
--									brick_broken_row <= i;
--									brick_broken_col <= j;
--									ball_v_dir_next <= not ball_v_dir_next;
--								end if;	
--							end if;	
--						end if;
--					
--						-- Horizontal collisions both from the left or the right of the brick
--						if ball_x + BALL_SIZE = BRICK_START_POS_X + (j * BRICK_WIDTH) or -- Left side of the brick
--							ball_x = BRICK_START_POS_X + (j * BRICK_WIDTH) then -- Right side of the brick
--							if ball_y + BALL_SIZE >= BRICK_START_POS_Y + (i * BRICK_HEIGHT) and 
--								ball_y < BRICK_START_POS_Y + ((i+1) * BRICK_HEIGHT) and -- Ball hits the brick somewhere
--								brick_array(i,j) = '1' then -- ...and brick exists
--								if ball_y < BRICK_START_POS_Y + (i * BRICK_HEIGHT) then -- Ball overflows to the upper side
--									if i = 0 or -- First row: immediate bump
--									brick_array(i-1, j) = '0' or -- No brick up: immediate bump
--									ball_y + (BALL_SIZE/2) >= BRICK_START_POS_Y + (i * BRICK_HEIGHT) then -- Intermediate row: check if its center is in this brick
--										brick_broken_row <= i;
--										brick_broken_col <= j;
--										ball_h_dir_next <= not ball_h_dir_next;
--									-- No need for else since the other block will be hit in the next iteration
--									end if;
--								elsif ball_y + BALL_SIZE > BRICK_START_POS_Y + ((i+1) * BRICK_HEIGHT) then -- Ball overflows to the down side
--									if i = BRICK_ROWS - 1 or -- Last row: immediate bump.
--										brick_array(i+1, j) = '0' or -- No brick down: immediate bump
--										ball_y + (BALL_SIZE/2) <= BRICK_START_POS_Y + ((i+1) * BRICK_HEIGHT) then -- Intermediate row: check if its center is in this brick
--										brick_broken_row <= i;
--										brick_broken_col <= j;
--										ball_h_dir_next <= not ball_h_dir_next;
--									end if;
--								else -- Ball is all on the brick
--									brick_broken_row <= i;
--									brick_broken_col <= j;
--									ball_h_dir_next <= not ball_h_dir_next;
--								end if;	
--							end if;
--						end if;
--					end if;
--				end loop;
--			end loop;
		end if;
	end process;

    bounce_counter_next <= bounce_counter + 1 when ball_bounce = '1' else
                           (others => '0') when ball_miss = '1' else
                           bounce_counter;

    ball_control_counter_next <= ball_control_counter + 1;

    ball_control: process(
		ball_control_counter,
        ball_x, ball_y,
        ball_x_next, ball_y_next,
        ball_h_dir, ball_v_dir,
		ball_dx, ball_dy,
        ball_enable
    )
    begin
        ball_x_next <= ball_x;
        ball_y_next <= ball_y;

		if ball_stuck = '1' then -- Place ball on a region so that it'll go 45 deg to right when unstuck
			ball_x_next <= bar_1_x + conv_std_logic_vector( ((BAR_WIDTH*75) / 100), 10);
			ball_y_next <= conv_std_logic_vector(BAR_1_POS - BALL_SIZE, 10);
      elsif ball_enable = '1' then
			if ball_control_counter = 0 then
				if ball_h_dir = '1' then
					ball_x_next <= ball_x + conv_std_logic_vector(ball_dx, 2);
				else
					ball_x_next <= ball_x - conv_std_logic_vector(ball_dx, 2);
				end if;
				if ball_v_dir = '1' then
					ball_y_next <= ball_y + conv_std_logic_vector(ball_dy, 2);
				else
					ball_y_next <= ball_y - conv_std_logic_vector(ball_dy, 2);
				end if;
			end if;
       else
			ball_x_next <= conv_std_logic_vector(SCREEN_WIDTH / 2 - BALL_SIZE / 2, 10);
         ball_y_next <= conv_std_logic_vector(SCREEN_HEIGHT / 2 - BALL_SIZE / 2, 10);
       end if;
    end process;

    bar_control: process(
        bar_1_x,
        ctl_left, ctl_right
    )
    begin
        bar_1_x_next <= bar_1_x;
        
        if ctl_left = '1' then
            if bar_1_x > 0 then
                bar_1_x_next <= bar_1_x - 1;
            end if;
        elsif ctl_right = '1' and bar_1_x + BAR_WIDTH + 1 < SCREEN_WIDTH then
                bar_1_x_next <= bar_1_x + 1;
        end if;
    end process;
		
	brick_control: process(
		brick_count, brick_count_next,
		brick_array,
		brick_broken_row, brick_broken_col,
		brick_init
	)
	begin
		if brick_init = '1' then
			brick_array <= BRICK_START;
			win_flag <= '0';
		elsif brick_count = 0 then
			win_flag <= '1';
		end if;
		if brick_array(brick_broken_row,brick_broken_col) = '1' then 
			brick_array(brick_broken_row,brick_broken_col) <= '0';
			brick_count_next <= brick_count - 1;
			score_1_next <= score_1 + 1;
		end if;
	end process;

		ball_on <= '1' when px_x >= ball_x and
							px_x < (ball_x + BALL_SIZE) and
							px_y >= ball_y and
							px_y < (ball_y + BALL_SIZE) else
				   '0';

		-- whether bar_1 is on
		bar_on <= '1' when (px_y >= BAR_1_POS and
							px_y < BAR_1_POS + BAR_HEIGHT and
							px_x >= bar_1_x and
							px_x < bar_1_x + BAR_WIDTH) else
				  '0';

process(px_x, px_y)
begin
	if ( px_x < BRICK_START_POS_X + (BRICK_COLS * BRICK_WIDTH) and
			px_x >= BRICK_START_POS_X and
			px_y < BRICK_START_POS_Y + (BRICK_ROWS * BRICK_HEIGHT) and
			px_y >= BRICK_START_POS_Y ) then
		brick_on <= '0';
		for i in 0 to BRICK_ROWS - 1 LOOP
			for j in 0 to BRICK_COLS - 1 LOOP
				if ( px_x >= BRICK_START_POS_X + (j * BRICK_WIDTH) and
					  px_x < BRICK_START_POS_X + ((j+1) * BRICK_WIDTH) and
					  px_y >= BRICK_START_POS_Y + (i * BRICK_HEIGHT) and
					  px_y < BRICK_START_POS_Y + ((i+1) * BRICK_HEIGHT) --and
--                brick_array(i,j) = '1'
					  
					  ) 
					  then
						brick_on <= '1';
						brick_addr <= conv_std_logic_vector(conv_integer(px_y - (BRICK_START_POS_Y + (i * BRICK_HEIGHT))) ,5);
						brick_pixel <= brick_data(conv_integer(px_x - (BRICK_START_POS_X + (j * BRICK_WIDTH))));
				end if;
				
				-- Unsure on this loop from now on, just a copycat for the bar one
			end loop;
		end loop;
	else
		brick_on <= '0';
	end if;
end process;
		brick_rgb <= "000" when brick_pixel = '1' else "111";
		
		ball_addr <= px_y(3 downto 0) - ball_y(3 downto 0);
		ball_px_addr <= px_x(3 downto 0) - ball_x(3 downto 0);
		ball_pixel <= ball_data(conv_integer(ball_px_addr));
		ball_rgb <= "000" when ball_pixel = '1' else "111";

		bar_addr <= (px_y(4 downto 0)- bar_pos);
		bar_pos <= BAR_1_POS;
		bar_pixel <= bar_data(conv_integer(px_x - bar_1_x));
		bar_rgb <= "000" when bar_pixel = '1' else "111";
	
    process(
        ball_on, bar_on, brick_on,
        ball_rgb, bar_rgb, brick_rgb,
        score_on, message_on, font_rgb,
        video_on
    )
    begin
        if video_on = '1' then
            if bar_on = '1' then
                rgb_stream <= bar_rgb;
            elsif ball_on = '1' then
                rgb_stream <= ball_rgb;
			elsif brick_on = '1' then
				rgb_stream <= brick_rgb;
            -- scores and messages share rgb stream
            elsif score_on = '1' or message_on = '1' then
                rgb_stream <= font_rgb;
            else
                -- background is white
                rgb_stream <= "111";
            end if;
        else
            -- blank screen
            rgb_stream <= "000";
        end if;
    end process;

    ball_unit:
        entity work.ball_rom(content)
        port map(addr => ball_addr, data => ball_data);

    bar_unit:
        entity work.bar_rom(content)
        port map(clk => clk, addr => bar_addr, data => bar_data);
	
	brick_unit:
		entity work.brick_rom(content)
		port map(addr => brick_addr, data => brick_data);

    font_unit:
        entity work.codepage_rom(content)
        port map(addr => font_addr, data => font_data);

    ball_bounced <= ball_bounce;
    ball_missed <= ball_miss;

end dispatcher;



