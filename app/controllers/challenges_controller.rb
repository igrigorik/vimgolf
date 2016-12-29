class ChallengesController < ApplicationController

  before_filter :login, :only => [:create, :new, :destroy]

  def index
    redirect_to root_path
  end

  def new
    @challenge = Challenge.new
    respond_to do |format|
      format.html
    end
  end

  def destroy
    begin
      @challenge = Challenge.find(params['id'])
      @challenge.destroy if @challenge.owner?(current_user)
    rescue Exception => e
    ensure
      redirect_to root_path
    end
  end

  def create
    challenge = params[:challenge]

    challenge[:diff] = challenge.delete(:diff).read rescue nil

    if input = challenge.delete(:input)
      challenge[:input] = input.read rescue nil
      challenge[:input_type] = input.original_filename.split('.').last
    end

    if output = challenge.delete(:output)
      challenge[:output] = output.read rescue nil
      challenge[:output_type] = output.original_filename.split('.').last
    end

    @challenge = Challenge.new(challenge)
    @challenge.user = current_user

    respond_to do |format|
      if @challenge.save
        format.html {
          redirect_to(@challenge, :notice => 'Challenge was successfully created')
        }
      else
        format.html { render :action => "new" }
      end
    end
  end

  def show
    @challenge = Challenge.find(params['id']) rescue nil
    return redirect_to root_path if @challenge.nil?

    # TODO, there is a better way to do this...
    users = @challenge.entries.map {|e| e.user_id }.uniq
    @users = User.where(:_id.in => users).inject({}) {|h,u| h[u.id] = u; h}

    respond_to do |format|
      format.json {
        render :json => {
          'in' => {
            'data' => @challenge.input,
            'type' => @challenge.input_type
          },
          'out' => {
            'data' => @challenge.output,
            'type' => @challenge.output_type
          },
          'client' => Vimgolf::VERSION
      }}

      format.html
    end
  end
end
